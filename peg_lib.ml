open Peg_input
  
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

type lexeme = {
  start : int * int;
  stop : int * int;
  lexeme : string
}

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
      
let end_of_file input =
  input.pos = input.len

let test_any input =
  if end_of_file input then
    Failed
  else (
    Printf.printf "test_any %d:%d %C\n" input.line input.col
      input.buf.[input.pos];
    Parsed ((), incr_pos input)
  )

(*
let test cond input =
  if cond input then
    Parsed state
  else
    Failed
*)

let test_char c input =
  if end_of_file input then
    Failed
  else (
    Printf.printf "test_char %d:%d %C %C\n" input.line input.col
      input.buf.[input.pos] c;
    if input.buf.[input.pos] = c then
      Parsed ((), incr_pos input)
    else
      Failed
  )

let test_string cs input =
  Printf.printf "test_string\n";
  let rec aux_test pos = function
    | [] -> Parsed ((), {input with pos = pos})
    | c :: cs ->
      if end_of_file input then
        Failed
      else if input.buf.[pos] = c then
        aux_test (succ pos) cs
      else
        Failed
  in
    aux_test input.pos cs
    
let test_f f input =
  if end_of_file input then
    Failed
  else (
    Printf.printf "test_f %d:%d %C\n" input.line input.col input.buf.[input.pos];
    if f input.buf.[input.pos] then
      Parsed ((), incr_pos input)
    else
      Failed
  )
let get_pattern cond input =
  let curr = input in
    match cond input with
      | Parsed ((), input) ->
        let lexeme = String.sub input.buf curr.pos (input.pos - curr.pos) in
          Parsed (lexeme, input)
      | Failed -> Failed

let match_pattern str input =
  Printf.printf "match_pattern\n";
  let len = String.length str in
  let rec aux_iter i input =
    if i < len && not (end_of_file input) then
      if str.[i] = input.buf.[input.pos] then
        aux_iter (succ i) (incr_pos input)
      else
        Failed
    else if i = len then
      Parsed ((), input)
    else
      Failed
  in
    aux_iter 0 input

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

