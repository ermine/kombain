
open Camlp4.PreCast
  
open Kmb_input
open Kmb_lib
open Printf

exception Error of string

type class_t =
  | Char of int
  | Range of int * int

type token =
  | Epsilon
  | Name of string
  | Literal of int list
  | Class of class_t list
  | Transform of lexeme * token
  | Action of Ast.expr * token list
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

let string_of_char c =
  if c < 255 then
    match Char.chr c with
      | '\n' -> "\\n"
      | '\r' -> "\\r"
      | '\b' -> "\\b"
      | '\t' -> "\\t"
      | '"' -> "\\\""
      | c -> String.make 1 c
  else
    sprintf "\\x%X" c

let string_of_range c =
  if c < 255 then
    match Char.chr c with
      | '[' -> "\\["
      | ']' -> "\\]"
      | '-' -> "\\-"
      | _ -> string_of_char c
  else
    string_of_char c
        
let rec string_of_token = function
  | Epsilon -> ""
  | Name name -> name
  | Literal cs ->
    sprintf "\"%s\"" (String.concat "" (List.map string_of_char cs))
  | Class cs ->
    List.fold_left (fun str -> function
      | Range (c1, c2) ->
        sprintf "%s%s-%s" str (string_of_range c1) (string_of_range c2)
      | Char c ->
        sprintf "%s%s" str (string_of_range c)
    ) "[" cs ^ "]"
  | PredicateNOT t -> "!" ^ string_of_token t
  | PredicateAND t -> "&" ^ string_of_token t
  | Opt t -> string_of_token t ^ "?"
  | Star t -> string_of_token t ^ "*"
  | Plus t -> string_of_token t ^ "+"
  | Sequence (s1, s2) ->
    sprintf "(%s %s)" (string_of_token s1) (string_of_token s2)
  | Alternate (a1, a2) ->
    sprintf "(%s / %s)" (string_of_token a1) (string_of_token a2)
  | Pattern (name, t) ->
    sprintf "%s@%s" name (string_of_token t)
  | Any -> "."
  | Transform (fn, t) ->
    sprintf "%s { %s }" (string_of_token t) fn.lexeme
  | Action (expr, tokens) ->
    "Action AST"
  | Tokenizer t ->
    sprintf "< %s >" (string_of_token t)
  
let make_declaration {lexeme} = lexeme

let make_name {lexeme} = Name lexeme

let make_char {lexeme} =
  Char.code lexeme.[0]
    
let make_escaped_char {lexeme} =
  match lexeme with
    | "b" -> Char.code '\b'
    | "n" -> Char.code '\n'
    | "r" -> Char.code '\r'
    | "t" -> Char.code '\t'
    | "\\" -> Char.code '\\'
    | c -> Char.code c.[0] 
      
let make_dec_char {lexeme} =
  int_of_string lexeme

let make_hex_char {lexeme} =
  int_of_string ("0x" ^ lexeme)

let make_any_char _ _ lexeme =
  lexeme.[0]

let make_literal chars =
  Literal chars

let make_tokenizer expr = Tokenizer expr

let print_remaining input =
  printf "Remaining input:\n%S\n" 
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
      | Some lexeme -> Transform (lexeme, expr)
    
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

let unmatched {start = (line, col) ; lexeme} =
  raise (Error (sprintf "Unmatched %S at line %d col %d" lexeme line col))

let invalid_char {start = (line, col); lexeme} =
  raise (Error (sprintf "Invalid char %S at line %d col %d" lexeme line col))

let rec is_productive known = function
  | Name name -> List.mem name known
  | Class _ -> true
  | Literal _ -> true
  | Any -> true
  | Pattern (_, t) -> is_productive known t
  | PredicateAND t -> is_productive known t
  | PredicateNOT t -> is_productive known t
  | Tokenizer t -> is_productive known t
  | Transform (_, t) -> is_productive known t
  | Star t -> is_productive known t
  | Opt t -> is_productive known t
  | Plus t -> is_productive known t
  | Epsilon -> true
  | Sequence (t1, t2) ->    
    is_productive known t1 && is_productive known t2
  | Alternate (t1, t2) ->
    is_productive known t1 && is_productive known t2
    

let simple_productive known rules =
  let rec aux_rearrange result known rest =
    let sorted, known, unsorted =
      List.fold_left (fun (acc, known, unsorted) (name, expr) ->
        if is_productive known expr then
          (name, expr) :: acc, name :: known, unsorted
        else
          acc, known, (name, expr) :: unsorted
      ) (result, known, []) rest in
      if sorted = result then
        List.rev result, known, List.rev unsorted
      else
        aux_rearrange sorted known unsorted
  in
    aux_rearrange [] known rules

type productive =
  | Simple of (string * token) list
  | Recursive of (string * token) list


let rearrange_grammar rules =
  let sorted, known, unsorted = simple_productive [] rules in
    match unsorted with
      | [] -> [Simple sorted]
      | _ ->
        [Simple sorted; Recursive unsorted]