open Kmb_input
open Kmb_lib
open Printf

type class_t =
  | Char of int
  | Range of int * int

type parameter =
  | Ident of string
  | Value of string * string
  | Func of string * parameter list
(*  | Collection of string * parameter list *)

type token =
  | Epsilon
  | Name of (string * parameter list)
  | Literal of int list
  | Class of class_t list
  | Transform of lexeme * token
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
  | Bind of string * string list * token

let string_of_char c =
  if c < 255 then
    match Char.chr c with
      | '\n' -> "\\n"
      | '\r' -> "\\r"
      | '\b' -> "\\b"
      | '\t' -> "\\t"
      | '"' -> "\\\""
      | '\\' -> "\\\\"
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

let rec string_of_params = function
  | [] -> ""
  | p :: ps ->
    let string_of_param = function
      | Ident n -> n
      | Value (t, v) -> (
        match t with
          | "bool" -> v
          | "string" -> "\"" ^ String.escaped v ^ "\""
          | "int" -> v
          | _ -> failwith "unknown value type"
      )
      | Func (name, ps) ->
        name ^ "(" ^ string_of_params ps ^ ")"
    in
      List.fold_left (fun acc p ->
        acc ^ "," ^ string_of_param p
      ) (string_of_param p) ps

let string_of_literal cs =
  String.concat "" (List.map string_of_char cs)
                   
let is_simple_token = function
  | Any
  | Name _
  | Class _
  | Literal _
  | Tokenizer _
  | Transform _ -> true
  | _ -> false

let string_of_class cs =
  List.fold_left (fun str -> function
    | Range (c1, c2) ->
      sprintf "%s%s-%s" str (string_of_range c1) (string_of_range c2)
    | Char c ->
      sprintf "%s%s" str (string_of_range c)
  ) "[" cs ^ "]"

let rec string_of_token = function
  | Epsilon -> ""
  | Name (name, params) ->
    if params = [] then
      name
    else
      sprintf "%s(%s)" name (string_of_params params)
  | Literal cs ->
    sprintf "\"%s\"" (string_of_literal cs)
  | Class cs ->
    string_of_class cs
  | PredicateNOT t ->
    if is_simple_token t then
      "!" ^ string_of_token t
    else
      sprintf "!(%s)" (string_of_token t)
  | PredicateAND t ->
    if is_simple_token t then
      "&" ^ string_of_token t
    else
      sprintf "&(%s)" (string_of_token t)
  | Opt t ->
    if is_simple_token t then
      string_of_token t ^ "?"
    else
      sprintf "(%s)?" (string_of_token t)
  | Star t ->
    if is_simple_token t then
      string_of_token t ^ "*"
    else
      sprintf "(%s)*" (string_of_token t)
  | Plus t ->
    if is_simple_token t then
       string_of_token t ^ "+"
    else
      sprintf "(%s)+" (string_of_token t)
  | Sequence (s1, s2) -> (
    match s1, s2 with
      | Alternate _, Alternate _ ->
        sprintf "(%s) (%s)" (string_of_token s1) (string_of_token s2)
      | Alternate _, _ ->
        sprintf "(%s) %s" (string_of_token s1) (string_of_token s2)
      | _, Alternate _ ->
        sprintf "%s (%s)" (string_of_token s1) (string_of_token s2)
      | _, _ ->
        sprintf "%s %s" (string_of_token s1) (string_of_token s2)
  )      
  | Alternate (a1, a2) ->
    sprintf "%s / %s" (string_of_token a1) (string_of_token a2)
  | Pattern (name, t) ->
    if is_simple_token t then
      sprintf "%s@%s" name (string_of_token t)
    else
      sprintf "%s@(%s)" name (string_of_token t)
  | Any -> "."
  | Transform (fn, t) ->
    sprintf "%s { %s }" (string_of_token t) fn.lexeme
  | Tokenizer t ->
    sprintf "< %s >" (string_of_token t)
  | Bind (var, vars, t) ->
    let param =
      if vars = [] then
        var
      else
        let r = List.fold_left (fun str v -> sprintf "%s,%s" str v) var vars in
        "(" ^ r ^ ")"
    in
      if is_simple_token t then
        sprintf "%s = %s" param (string_of_token t)
      else
        sprintf "%s = (%s)" param (string_of_token t)

let rec remove_transforms = function
  | Transform (_, t) -> remove_transforms t
  | Opt t -> Opt (remove_transforms t)
  | Plus t -> Plus (remove_transforms t)
  | Star t -> Star (remove_transforms t)
  | PredicateNOT t -> PredicateNOT (remove_transforms t)
  | PredicateAND t -> PredicateAND (remove_transforms t)
  | Sequence (s1, s2) -> Sequence (remove_transforms s1, remove_transforms s2)
  | Alternate (a1, a2) -> Alternate (remove_transforms a1, remove_transforms a2)
  | Pattern (s, t) -> Pattern (s, remove_transforms t)
  | Bind (v1, vs, t) -> Bind (v1, vs, remove_transforms t)
  | t -> t
      
let string_of_rule ((name, params), expr) =
  if params = [] then
    sprintf "%s <- %s" name (string_of_token expr)
  else
    let args =
      match params with
        | [] -> ""
        | p :: ps -> List.fold_left (fun acc p -> acc ^ "," ^ p) p ps
    in
      sprintf "%s(%s) <- %s" name args (string_of_token expr)

let make_declaration {lexeme} = lexeme

let make_name {lexeme} = Name (lexeme, [])

let make_definition ({lexeme}, expr) =
  (lexeme, expr)

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

let make_bind (ps, expr) =
  match ps with
    | [] -> assert false
    | p :: ps -> Bind (p, ps, expr)

let make_predicate_not () v = PredicateNOT v
let make_predicate_and () v = PredicateAND v

let make_class cs = Class cs

let make_prefix (f, s) =
  match f with
    | None -> s
    | Some f -> f s

let unmatched {lexeme} =
  raise (Kmb_lib.Syntax (sprintf "Not found matched pair for %S" lexeme))

let invalid_char {start = (line, col); lexeme} =
  raise (Kmb_lib.Syntax (sprintf "Invalid char %S" lexeme))

let rec is_productive known = function
  | Name (name, params) -> List.mem name known
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
  | Bind (_, _, t) -> is_productive known t

let simple_productive known rules =
  let rec aux_rearrange result known rest =
    let sorted, known, unsorted =
      List.fold_left (fun (acc, known, unsorted) ((name, params), expr) ->
        if is_productive known expr then
          ((name, params), expr) :: acc, name :: known, unsorted
        else
          acc, known, ((name, params), expr) :: unsorted
      ) (result, known, []) rest in
      if sorted = result then
        List.rev result, known, List.rev unsorted
      else
        aux_rearrange sorted known unsorted
  in
    aux_rearrange [] known rules

type productive =
  | Simple of ((string * string list) * token) list
  | Recursive of ((string * string list) * token) list


let rearrange_grammar rules =
  let sorted, known, unsorted = simple_productive [] rules in
    match unsorted with
      | [] -> [Simple sorted]
      | _ ->
        [Simple sorted; Recursive unsorted]
