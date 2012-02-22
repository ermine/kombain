open Kmb_input
open Printf
  
type 'a result =
  | Parsed of 'a * input
  | Failed

let return r input = Parsed (r, input)      

let transform r f input =
  match r input  with
    | Parsed (v, input) -> Parsed (f v, input)
    | Failed as failed -> failed

let predicate_not cond input =
  match cond input with
    | Parsed _ -> Failed
    | Failed -> Parsed ((), input)

let predicate_and cond input =
  match cond input with
    | Parsed _ -> Parsed ((), input)
    | Failed as failed -> failed

let peg_stub input = Parsed ((), input)
  
let opt cond input =
  match cond input with
    | Parsed (_, input) -> Parsed ((), input)
    | Failed -> Parsed ((), input)

let opt_accu cond input =
  match cond input with
    | Parsed (r, input) -> Parsed (Some r, input)
    | Failed -> Parsed (None, input)

let get_lexeme cond input =
  let start = input in
    match cond input with
      | Parsed ((), input) ->
        let end_pos = input.pos in
        let lexeme = String.sub input.buf start.pos (end_pos - start.pos) in
          Parsed ({start = (start.line, start.col);
                   stop = (input.line, input.col);
                   lexeme}, input)
      | Failed -> Failed
      
let test_any input =
  if end_of_file input then
    Failed
  else
    let _curr = get_current input in
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

let seq a b input =
  match a input with
    | Failed as failed -> failed
    | Parsed ((), input) -> b input
          

let seq_l a b input =
  match a input with
    | Failed as failed -> failed
    | Parsed (r, input) ->
      match b input with
        | Parsed ((), input) -> return r input
        | Failed as failed -> failed

let seq_r a b input =
  match a input with
    | Parsed (_, input) -> b input
    | Failed as failed -> failed

let seq_n a b input =
    match a input with
      | Failed as failed -> failed
      | Parsed (_, input) ->
        match b input with
          | Parsed (_, input) -> Parsed ((), input)
          | Failed as failed -> failed

let seq_b a b input =
  match a input with
    | Failed as failed -> failed
    | Parsed (r1, input) ->
      match b input with
        | Parsed (r2, input) -> Parsed ((r1, r2), input)
        | Failed as failed -> failed

let alt a b input =
  match a input with
    | Parsed _ as ok -> ok
    | Failed -> b input


let rec star cond input =
  match cond input with
    | Parsed (_, input) -> star cond input
    | Failed -> Parsed ((), input)

let star_accu cond input =
  let rec aux_star acc input =
    match cond input with
      | Parsed (r, input) -> aux_star (r :: acc) input
      | Failed -> Parsed (List.rev acc, input)
  in
    aux_star [] input

let plus cond input =
  match cond input with
    | Parsed (_, input) -> star cond input
    | Failed as failed -> failed

let plus_accu cond input =
  transform (seq_b cond (star_accu cond))
    (fun (r1, r2) -> r1 :: r2)
    input

let match_result p value input =
  match p input with
    | Parsed r, input as ok ->
      if r = value then
        ok
      else
        Failed
    | Failed as failed -> failed
