open Peg_grammar
open Camlp4.PreCast;;

module Caml =
  Camlp4.Printers.OCaml.Make
    (Camlp4OCamlParser.Make
       (Camlp4OCamlRevisedParser.Make(Syntax)))


open Syntax
    
let rule_prefix = "peg_grammar_"

let rec make_pattern _loc name expr exprs =
  <:expr<
    let $lid:"var_tmp_" ^ name$ =
      get_pattern $make_rule_expr _loc expr$ state input in
      match $lid:"var_tmp_" ^ name$ with
        | Parsed ((x, state), input) ->
          let $lid:rule_prefix ^ name$ =
            match_pattern x in
            peg_sequence $exprs$ state input
        | Failed -> Failed
    >>
        
and make_rule_expr _loc = function
  | Epsilon ->
    <:expr< peg_stub >>
  | Name name ->
    <:expr< $lid:rule_prefix ^ name$ >>

  | Sequence (t1, t2) ->
    let rec flat _loc acc = function
      | Sequence (x1, x2) -> (
        match x2 with
          | Pattern (name, expr) ->
            flat _loc <:expr< fun state input ->
              $make_pattern _loc name expr acc$ state input >> x1
          | _ ->
            flat _loc (<:expr< $make_rule_expr _loc x2$ :: $acc$ >>) x1
      )
      | Pattern (name, expr) ->
        <:expr< fun state input -> $make_pattern _loc name expr acc$ >>
      | x ->
        <:expr< peg_sequence ($make_rule_expr _loc x$ :: $acc$) >>
    in
      flat _loc (<:expr< [$make_rule_expr _loc t2$] >>) t1
        
  | Alternate (t1, t2) ->
    let rec flat _loc acc = function
      | Alternate (x1, x2) ->
        flat _loc (<:expr< $make_rule_expr _loc x2$ :: $acc$ >>) x1
      | x ->
        <:expr< $make_rule_expr _loc x$ :: $acc$ >>
    in
    let exprs = flat _loc (<:expr< [$make_rule_expr _loc t2$] >>) t1 in
      <:expr< peg_alternate $exprs$ >>

  | Tokenizer (t, fn) ->
    <:expr< get_lexeme $make_rule_expr _loc t$ $lid:fn$ >>
 
  | Action  ((line, col), code) ->
    let e =
      try Caml.AntiquotSyntax.parse_expr Loc.ghost code
      with exn ->
        Printf.printf "Bad action %d:%d %S\n" line col code;
        Printf.printf "Exception: %s\n" (Printexc.to_string exn);
        Pervasives.exit 1
    in
      <:expr< peg_action $e$ >>

  | Star t ->
    <:expr< peg_star $make_rule_expr _loc t$ >>

  | PredicateNOT t ->
    <:expr< peg_not $make_rule_expr _loc t$ >>

  | Any ->
    <:expr< test_any >>

  | Literal chars -> (
    match chars with
      | [] -> <:expr< >>
      | [x] -> <:expr< test_char $`chr:x$ >>
      | _ ->
        let exprs = List.map (fun c -> <:expr< $`chr:c$ >>) (List.rev chars) in
          <:expr< test_string [$Ast.exSem_of_list exprs$] >>
  )

  | Class classes ->
    let make_expr = function
      | [x1; x2] ->
        <:expr< c >= $`int:Char.code x1$ && c <= $`int:Char.code x2$ >>
      | [x] -> <:expr< c = $`int:Char.code x$ >>
      | _ -> assert false
    in
    let exprs =
      List.fold_left (fun acc s ->
        <:expr< $make_expr s$ || $acc$ >>
      ) <:expr< $make_expr (List.hd classes)$ >> (List.tl classes) in
      <:expr< test_f (fun c -> let c = Char.code c in $exprs$) >>

   | _ -> <:expr< () >>

let generate rules output_file =
  let start_rule, declaration =
    match List.rev rules with
      | Declaration str :: Rule (name, _) :: _ -> name, str
      | Rule (name, _) :: _ -> name, ""
      | other -> assert false in
    
  let _loc = Loc.ghost in
  let bindings = List.fold_right (fun token acc ->
    match token with
      | Rule (name, expr) ->
        <:binding< $lid:rule_prefix ^ name$ state input =
      Printf.printf "Trying %s... " $str:name$;
        match $make_rule_expr _loc expr$ state input with
          | Failed as result -> Printf.printf "Failed %s\n" $str:name$; result
          | Parsed _ as result -> Printf.printf "Success %s\n" $str:name$; result
        >> :: acc
      | Declaration _ -> acc
      | _ -> assert false
  ) rules [] in

    Caml.print_implem ~output_file
    <:str_item<
      $if declaration <> "" then
       Caml.parse_implem _loc (Stream.of_string declaration)
     else
       <:str_item< >>
      $
      let parse state string =
        let rec $Ast.biAnd_of_list bindings$ in
        let input = Peg_input.make_input string in
        let result = $lid:rule_prefix ^ start_rule$ state input in
          result
          >>
