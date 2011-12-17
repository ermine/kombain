open Peg_input

type token =
  | Epsilon
  | Name of string
  | Literal of char list
  | Class of char list list
  | Action of (int * int) * string
  | Char of char
  | Any
  | Tokenizer of token * string
  | Star of token
  | PredicateNOT of token
  | Alternate of token * token
  | Sequence of token * token
  | Rule of string * token
  | Declaration of string
  | Pattern of string * token

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
  | Action ((line, col), str) ->
    Printf.printf "Action %d:%d %S\n" line col str
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
  | Pattern (name, expr) ->
    Printf.printf "Pattern %s [\n" name;
    print_token expr;
    Printf.printf "]\n"

let make_declaration state _ _ lexeme = Declaration lexeme :: state

let make_name state _ _ lexeme = Name lexeme :: state

let make_action state start _ lexeme = Action (start, lexeme) :: state

let make_char state _ _ lexeme =
  Char lexeme.[0] :: state
    
let make_escaped_char state _ _ lexeme =
  match lexeme with
    | "b" -> Char '\b' :: state
    | "n" -> Char '\n' :: state
    | "r" -> Char '\r' :: state
    | "t" -> Char '\t' :: state
    | "\\" -> Char '\\' :: state
    | c -> Char c.[0] :: state
      
let make_octet_char state _ _ lexeme =
  Char (Char.chr (int_of_string ("0o" ^ lexeme))) :: state

let make_any_char state _ _ lexeme =
  Char lexeme.[0] :: state

let print_remaining input =
  Printf.printf "Remaining input:\n%S\n" 
    (String.sub input.buf input.pos (input.len - input.pos))
  
