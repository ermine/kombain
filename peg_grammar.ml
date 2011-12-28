open Peg_input
open Peg_lib

exception Error of string

type class_t =
  | Char of char
  | Range of char * char

type token =
  | Epsilon
  | Name of string
  | Literal of char list
  | Class of class_t list
  | Action of (int * int) * string * token
  | Any
  | Tokenizer of token
  | Opt of token
  | Plus of token
  | Star of token
  | PredicateNOT of token
  | PredicateAND of token
  | Sequence of token * token
  | Alternate of token * token
  | Pattern of string * token

let rec print_token = function
  | Epsilon ->
    Printf.printf "Epsilon\n"
  | Name str ->
    Printf.printf "Name %S\n" str
  | Literal chars ->
    Printf.printf "Literal ";
    List.iter (fun c -> Printf.printf "%C " c) chars;
    Printf.printf "\n"
  | Class cs ->
    Printf.printf "Class\n"
  | Action ((line, col), str, expr) ->
    Printf.printf "Action %d:%d %S\n" line col str
  | Any ->
    Printf.printf "Any\n"
  | Tokenizer t ->
    Printf.printf "Tokenizer ["; print_token t; Printf.printf "]\n"
  | PredicateNOT token ->
    Printf.printf "Predicate NOT "; print_token token
  | Sequence (t1, t2) ->
    Printf.printf "Sequence [\n";
    print_token t1;
    print_token t2;
    Printf.printf "]\n"
  | Alternate (t1, t2) ->
    Printf.printf "Alternate [\n";
    Printf.printf "   "; print_token t1;
    Printf.printf "   "; print_token t2;
  | Opt t ->
    Printf.printf "Opt "; print_token t;
  | Plus t ->
    Printf.printf "Plus "; print_token t;
  | Star token ->
    Printf.printf "Star "; print_token token
  | Pattern (name, expr) ->
    Printf.printf "Pattern %s [\n" name;
    print_token expr;
    Printf.printf "]\n"

let make_declaration {lexeme} = lexeme

let make_name {lexeme} = Name lexeme

let make_char {lexeme} =
  lexeme.[0]
    
let make_escaped_char {lexeme} =
  match lexeme with
    | "b" -> '\b'
    | "n" -> '\n'
    | "r" -> '\r'
    | "t" -> '\t'
    | "\\" -> '\\'
    | c -> c.[0] 
      
let make_octet_char {lexeme} =
  (Char.chr (int_of_string ("0o" ^ lexeme)))

let make_any_char _ _ lexeme =
  lexeme.[0]

let make_literal chars =
  Literal chars

let make_tokenizer expr = Tokenizer expr

let print_remaining input =
  Printf.printf "Remaining input:\n%S\n" 
    (String.sub input.buf input.pos (input.len - input.pos))
  
let make_alternates (s1, s2) =
  match List.rev s2 with
    | [] -> s1
    | [x] -> Alternate (s1, x)
    | x :: xs ->
      Alternate (s1, 
                 List.fold_left (fun acc s -> Alternate (s, acc)) x xs)

let make_sequence (items, a) =
  let expr =
    match List.rev items with
      | [] -> Epsilon
      | [x] -> x
      | x :: xs ->
        List.fold_left (fun acc i -> Sequence (i, acc)) x xs
  in
    match a with
      | None -> expr
      | Some {start; lexeme} -> Action (start, lexeme, expr)
    
let make_pattern ({lexeme}, expr) =
  Pattern (lexeme, expr)

let make_predicate_not () v = PredicateNOT v
let make_predicate_and () v = PredicateAND v

let make_class cs = Class cs

let make_prefix (f, s) =
  match f with
    | None -> s
    | Some f -> f s

let make_definition ({lexeme}, expr) =
  (lexeme, expr)
