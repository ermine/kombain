open Kmb_input
open Printf
  
exception Syntax of string

type 'a result =
  | Parsed of 'a * input
  | Failed

let return r input = Parsed (r, input)      

let transform r f input =
  match r input  with
    | Parsed (v, input) ->
      let r =
        try f v
        with
          | Syntax msg ->
            printf "%s\nSyntax error: %s\n"
              (Kmb_input.string_of_location input) msg;
            exit 1
          | exn ->
            printf "%s\nException: %s\n"
              (Kmb_input.string_of_location input) (Printexc.to_string exn);
            exit 2
      in
        Parsed (r, input)
    | Failed -> Failed

let fail r msg input =
  match r input with
    | Parsed _ as ok -> ok
    | Failed ->
      printf "%s\nSyntax error: %s\n"
        (Kmb_input.string_of_location input) msg;
      exit 1
      
let predicate_not cond input =
  match cond input with
    | Parsed _ -> Failed
    | Failed -> Parsed ((), input)

let predicate_and cond input =
  match cond input with
    | Parsed (r, _) -> Parsed (r, input)
    | Failed -> Failed

let peg_stub input = Parsed ((), input)
  
let opt cond input =
  match cond input with
    | Parsed ((), input) as ok -> ok
    | Failed -> Parsed ((), input)

let opt_accu cond input =
  match cond input with
    | Parsed (r, input) -> Parsed (Some r, input)
    | Failed -> Parsed (None, input)

let get_lexeme cond input =
  match cond input with
    | Parsed ((), input') ->
      let lexeme = String.sub input.buf input.pos (input'.pos - input.pos) in
        Parsed ({start = (input.line, input.col);
                 stop = (input'.line, input'.col);
                 lexeme}, input')
    | Failed -> Failed
      
let test_any input =
  if end_of_file input then
    Failed
  else
    Parsed ((), incr_pos input)

let test_char c input =
  if end_of_file input then
    Failed
  else
    let curr = get_current input in
      if curr = c then
        Parsed ((), incr_pos input)
      else
        Failed

let match_pattern cs input =
  let rec aux_iter input = function
    | [] -> Parsed ((), input)
    | x :: xs ->
      if end_of_file input then
        Failed
      else
        let curr = get_current input in
          if curr = x then
            aux_iter (incr_pos input) xs
          else
            Failed
  in
    aux_iter input cs

let test_class fn input =
  if end_of_file input then
    Failed
  else
    let cur = get_current input in
      if fn cur  then
        Parsed ((), incr_pos input)
      else
        Failed
    
let get_pattern cond input =
  let curr = input in
    match cond input with
      | Parsed ((), input) ->
        let rec aux_iter i =
          if i < input.pos then
            Char.code (input.buf.[i]) :: aux_iter (succ i)
          else
            []
        in
        let chars = aux_iter curr.pos in
          Parsed (chars, input)
      | Failed -> Failed

let seq_l a b input =
  match a input with
    | Failed -> Failed
    | Parsed (r, input) ->
      match b input with
        | Parsed ((), input) -> return r input
        | Failed -> Failed

let seq_r a b input =
  match a input with
    | Parsed ((), input) -> b input
    | Failed -> Failed

let seq_n a b input =
    match a input with
      | Failed -> Failed
      | Parsed ((), input) ->
        match b input with
          | Parsed ((), input) as ok -> ok
          | Failed -> Failed

let seq_b a b input =
  match a input with
    | Failed -> Failed
    | Parsed (r1, input) ->
      match b input with
        | Parsed (r2, input) -> Parsed ((r1, r2), input)
        | Failed -> Failed

let alt a b input =
  match a input with
    | Parsed _ as ok -> ok
    | Failed -> b input


let rec star cond input =
  match cond input with
    | Parsed ((), input') ->
      if input.pos = input'.pos then
        Parsed ((), input')
      else
        star cond input'
    | Failed -> Parsed ((), input)

let star_accu cond input =
  let rec aux_star acc input =
    match cond input with
      | Parsed (r, input') ->
        if input.pos = input'.pos then
          Parsed (r :: acc, input')
        else
          aux_star (r :: acc) input'
      | Failed -> Parsed (List.rev acc, input)
  in
    aux_star [] input

let plus cond input =
  match cond input with
    | Parsed ((), input) -> star cond input
    | Failed -> Failed

let plus_accu cond input =
  transform (seq_b cond (star_accu cond))
    (fun (r1, r2) -> r1 :: r2)
    input
