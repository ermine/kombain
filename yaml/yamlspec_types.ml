open Camlp4.PreCast

open Kmb_lib
  

type tokenf =
  | Peg of token
  | Cmp of string * token
      
type casemap =
  | Match of (string * string * tokenf) list
  | Indicator of (token * tokenf) list

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

let make_match data =
  let e =
    Ast.mcOr_of_list
      List.map (fun ((_, str), expr) ->
        <:match_case< $uid:String.uppercase str$ -> $
