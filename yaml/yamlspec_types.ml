open Camlp4.PreCast

open Kmb_lib
  
type param =
  | Fn of string * param list
  | N of string
  | Number of int

type tokenf =
  | Peg of Kmb_grammar.token
  | Function of string * param list
  | TimesVar of tokenf * string
  | Times of tokenf * int
  | TimesLT of tokenf * string
  | TimesLE of tokenf * string
  | Cases of (tokenf * tokenf) list
  | Cmp of string * tokenf
  | Alt of tokenf list * tokenf list list
  | Plus of tokenf
  | Opt of tokenf
  | Star of tokenf

type rule =
  | Rule of string * Kmb_grammar.token
  | RuleFun of (string * string list) * tokenf

let rec repeat fail n symbol input =
  if n = 0 then
    Parsed ((), input)
  else
    match symbol input with
      | Parsed (_, input) -> repeat fail (pred n) symbol input
      | Failed -> if fail then Failed else Parsed ((), input)

let make_reject (t, ts) =
  List.fold_right (fun t acc ->
    Kmb_grammar.Sequence (Kmb_grammar.PredicateNOT t, acc)) ts t

let rec peg_of_extension = function
  | Peg t -> t

  | Times (t, n) ->
    Kmb_grammar.Action ({Kmb_input.start = (0,0); stop = (0,0);
                         lexeme = Printf.sprintf "repeat true %d" n},
                        [peg_of_extension t])
      
  | _ -> failwith "functional things"

(*    

  | TimesVar (t, n) ->
      Kmb_grammar.Action (<:expr< repeat true $lid:n$ >>, [convert_token t])


  | TimesLT (t, n) ->
    let _loc = Loc.ghost in
      Kmb_grammar.Action (<:expr< repeat false ($lid:n$ - 1) >>,
                          [convert_token t])

  | TimesLE (t, n) ->
    let _loc = Loc.ghost in
      Kmb_grammar.Action (<:expr< repeat false ($lid:n$ 1) >>,
                          [convert_token t])

  | Function (name, params) ->
    let _loc = Loc.ghost in
      Kmb_grammar.Action (
        List.fold_left (fun acc p -> <:expr< $acc$ $lid:p$ >>)
        <:expr< $lid:name$ >> params, [])

  | Cases _ -> Kmb_grammar.Epsilon
  | Cmp _ -> Kmb_grammar.Epsilon


    *)

let alt_f (s1, s2) =
  Alt (s1, s2)
