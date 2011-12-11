type input = {
  buf : string;
  mutable pos : int;
  len : int
}

type token =
  | Epsilon
  | Name of string
  | Literal of char list
  | Class of char list list
  | Action of string
  | Char of char
  | Any
  | Tokenizer of token * string
  | Star of token
  | PredicateNOT of token
  | Alternate of token * token
  | Sequence of token * token
  | Rule of string * token
  | Declaration of string

let rec print_token = function
  | Epsilon ->
    Printf.printf "Epsilon\n"
  | Name str ->
    Printf.printf "Name %S\n" str
  | Literal chars ->
    Printf.printf "Literal ";
    List.iter (Printf.printf " %C " ) (List.rev chars);
    Printf.printf "\n"
  | Class cs ->
    Printf.printf "Class\n"
  | Action str ->
    Printf.printf "Action %S\n" str
  | Char c ->
    Printf.printf "Char %C\n" c
  | Any ->
    Printf.printf "Any\n"
  | Tokenizer (t, fn) ->
    Printf.printf "Tokenizer %S "fn; print_token t
  | PredicateNOT token ->
    Printf.printf "Predicate NOT "; print_token token
  | Alternate (t1, t2) ->
    Printf.printf "Alternate\n";
    Printf.printf "   "; print_token t1;
    Printf.printf "   "; print_token t2;
  | Sequence (t1, t2) ->
    Printf.printf "Sequence\n";
    Printf.printf "   "; print_token t1;
    Printf.printf "   "; print_token t2;
  | Star token ->
    Printf.printf "Star "; print_token token
  | Rule (name, token) ->
    Printf.printf "Rule %S " name; print_token token
  | Declaration str ->
    Printf.printf "Declaration %S\n" str

type state = token list

type result =
  | Parsed of state
  | Failed

let rule_prefix = "peg_grammar_"

let make_input str =
  { pos = 0; len = String.length str; buf = str}

let incr_pos input =
  input.pos <- input.pos + 1

let peg_not cond state input =
  let curr = input.pos in
    match cond state input with
      | Parsed state ->
        input.pos <- curr;
        Failed
      | Failed ->
        Parsed state

let peg_stub state input = Parsed state
  
let rec peg_sequence fs state input =
  let curr = input.pos in
    match fs with
      | [] -> Parsed state
      | f :: fs ->
        match f state input with
          | Parsed state -> peg_sequence fs state input
          | Failed -> input.pos <- curr; Failed

let peg_branch cond state input =
  let curr = input.pos in
    match cond state input with
      | Parsed state as ok -> ok
      | Failed ->
        input.pos <- curr;
        Failed

let rec peg_alternate alts state input =
  match alts with
    | [] -> Failed
    | x :: xs ->
      match peg_branch x state input with
        | Parsed state as ok -> ok
        | Failed -> peg_alternate xs state input

let rec peg_star cond state input =
  match cond state input with
    | Parsed state -> peg_star cond state input
    | Failed -> Parsed state

let peg_action fn state input =
  Parsed (fn state)
  
let push_token cond fn state input =
  let start_pos = input.pos in
    match cond state input with
      | Parsed state ->
        let end_pos = input.pos in
        let token = String.sub input.buf start_pos (end_pos - start_pos) in
          Printf.printf "Token %d,%d %S\n" start_pos end_pos token;
          Parsed (fn token :: state)
      | Failed -> Failed
      
let end_of_file input =
  input.pos = input.len

let test_any state input =
  if end_of_file input then
    Failed
  else (
    incr_pos input;
    Parsed state
  )

let test cond state input =
  if cond input then
    Parsed state
  else
    Failed

let test_char c state input =
  if end_of_file input then
    Failed
  else if input.buf.[input.pos] = c then (
    incr_pos input;
    Parsed state
  )
  else
    Failed

let test_string cs state input =
  let curr = input.pos in
  let rec aux_test = function
    | [] -> Parsed state
    | c :: cs ->
      if end_of_file input then
        Failed
      else if input.buf.[input.pos] = c then (
        incr_pos input;
        aux_test cs
      )
      else (
        input.pos <- curr;
        Failed
      )
  in
      aux_test cs
    
let test_f f state input =
  if end_of_file input then
    Failed
  else
    if f input.buf.[input.pos] then (
      incr_pos input;
      Parsed state
    )
    else
      Failed

let read_file file =
  let f = open_in file in
  let rec aux_read acc =
    let line =
      try Some (input_line f)
      with _ -> None in
      match line with
        | None -> List.rev acc
        | Some line -> aux_read (line :: acc)
  in
  let lines = aux_read [] in
    close_in f;
    String.concat "\n" lines ^ "\n"

let peg_declaration lexeme = Declaration lexeme
