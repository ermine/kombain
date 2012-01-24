open Camlp4.PreCast

open Kmb_lib
  
type param =
  | Fn of string * param list
  | N of string
  | Number of int

type token =
  | Function of string * param list
  | Identifier of string
  | Star of token
  | Plus of token
  | Opt of token
  | Alternate of token * token
  | Sequence of token * token
  | Epsilon
  | Class of Kmb_grammar.class_t list
  | Literal of int list
  | TimesVar of token * string
  | Times of token * int
  | TimesLT of token * string
  | TimesLE of token * string
  | Cases of (token * token) list
  | Reject of token * token list
  | Cmp of string * token

type rule =
  | Rule of string * token
  | RuleFun of (string * string list) * token

let make_alternates (s1, s2) =
  match List.rev s2 with
    | [] -> s1
    | [x] -> Alternate (s1, x)
    | x :: xs ->
      Alternate (s1, 
                 List.fold_left (fun acc s -> Alternate (s, acc)) x xs)

let make_sequence (items) =
  match List.rev items with
    | [] -> Epsilon
    | [x] -> x
    | x :: xs ->
      List.fold_left (fun acc i -> Sequence (i, acc)) x xs
      
let rec repeat fail n symbol input =
  if n = 0 then
    Parsed ((), input)
  else
    match symbol input with
      | Parsed (_, input) -> repeat fail (pred n) symbol input
      | Failed -> if fail then Failed else Parsed ((), input)

let rec convert_token = function
  | Identifier name -> Kmb_grammar.Name name
  | Star t -> Kmb_grammar.Star (convert_token t)
  | Plus t -> Kmb_grammar.Plus (convert_token t)
  | Opt t -> Kmb_grammar.Opt (convert_token t)
  | Alternate (a1, a2) ->
    Kmb_grammar.Alternate (convert_token a1, convert_token a2)
  | Sequence (s1, s2) ->
    Kmb_grammar.Sequence (convert_token s1, convert_token s2)
  | Epsilon -> Kmb_grammar.Epsilon
  | Class cs -> Kmb_grammar.Class cs
  | Literal cs -> Kmb_grammar.Literal cs

  | Times (t, n) ->
    let _loc = Loc.ghost in
      Kmb_grammar.Action (<:expr< repeat true $`int:n$ >>, [convert_token t])
    
  | TimesVar (t, n) ->
    let _loc = Loc.ghost in
      Kmb_grammar.Action (<:expr< repeat true $lid:n$ >>, [convert_token t])

  | TimesLT (t, n) ->
    let _loc = Loc.ghost in
      Kmb_grammar.Action (<:expr< repeat false ($lid:n$ - 1) >>,
                          [convert_token t])

  | TimesLE (t, n) ->
    let _loc = Loc.ghost in
      Kmb_grammar.Action (<:expr< repeat false ($lid:n$ 1) >>,
                          [convert_token t])

  | Reject (t, ts) ->
    List.fold_right (fun t acc ->
      Kmb_grammar.Sequence (Kmb_grammar.PredicateNOT (convert_token t), acc)
    ) ts (convert_token t)

    
  | Function (name, params) ->
    Kmb_grammar.Epsilon
      (*
    let _loc = Loc.ghost in
      Kmb_grammar.Action (
        List.fold_left (fun acc p -> <:expr< $acc$ $lid:p$ >>)
        <:expr< $lid:name$ >> params, [])
      *)

  | Cases _ -> Kmb_grammar.Epsilon
  | Cmp _ -> Kmb_grammar.Epsilon


