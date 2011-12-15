open Peg_input
  
type 'a result =
  | Parsed of 'a * input
  | Failed

let peg_not cond state input =
  match cond state input with
    | Parsed _ -> Failed
    | Failed -> Parsed (state, input)

let peg_stub state input = Parsed (state, input)
  
let rec peg_sequence fs state input =
  match fs with
    | [] -> Parsed (state, input)
    | f :: fs ->
      match f state input with
        | Parsed (state, input) -> peg_sequence fs state input
        | Failed -> Failed

let rec peg_alternate alts state input =
  match alts with
    | [] -> Failed
    | x :: xs ->
      match x state input with
        | Parsed _ as ok -> ok
        | Failed -> peg_alternate xs state input

let rec peg_star cond state input =
  match cond state input with
    | Parsed (state, input) -> peg_star cond state input
    | Failed -> Parsed (state, input)

let peg_action fn state input =
  Parsed ((fn state input), input)
  
let get_lexeme cond fn state input =
  let start = input in
    match cond state input with
      | Parsed (state, input) ->
        let end_pos = input.pos in
        let lexeme = String.sub input.buf start.pos (end_pos - start.pos) in
          Printf.printf "Token %d,%d %S\n" start.pos end_pos lexeme;
          Parsed (fn state (start.line, start.col) (input.line, input.col)
                    lexeme, input)
      | Failed -> Failed
      
let end_of_file input =
  input.pos = input.len

let test_any state input =
  if end_of_file input then
    Failed
  else (
    Printf.printf "test_any %d:%d %C\n" input.line input.col
      input.buf.[input.pos];
    Parsed (state, incr_pos input)
  )

(*
let test cond state input =
  if cond input then
    Parsed state
  else
    Failed
*)

let test_char c state input =
  if end_of_file input then
    Failed
  else (
    Printf.printf "test_char %d:%d %C %C\n" input.line input.col
      input.buf.[input.pos] c;
    if input.buf.[input.pos] = c then
      Parsed (state, incr_pos input)
    else
      Failed
  )

let test_string cs state input =
  let rec aux_test pos = function
    | [] -> Parsed (state, {input with pos = pos})
    | c :: cs ->
      if end_of_file input then
        Failed
      else if input.buf.[pos] = c then
        aux_test (succ pos) cs
      else
        Failed
  in
    aux_test input.pos cs
    
let test_f f state input =
  if end_of_file input then
    Failed
  else (
    Printf.printf "test_f %d:%d %C\n" input.line input.col input.buf.[input.pos];
    if f input.buf.[input.pos] then
      Parsed (state, incr_pos input)
    else
      Failed
  )
