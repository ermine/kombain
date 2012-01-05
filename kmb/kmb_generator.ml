open Kmb_grammar
open Camlp4.PreCast;;

module Caml =
  Camlp4.Printers.OCaml.Make
    (Camlp4OCamlParser.Make
       (Camlp4OCamlRevisedParser.Make(Syntax)))


open Syntax
    
let rule_prefix = "kmb_"

let rec should_export rules names = function
  | Epsilon -> false
  | Name name -> (
    if List.mem name names then
      false
    else
      try should_export rules (name :: names)
            (List.assoc name rules) with Not_found ->
        Printf.printf "Warning: Not found rule: %s\n" name;
        false
  )
  | Action _ -> true
  | Tokenizer _ -> true
  | Pattern (_, expr) -> should_export rules names expr
  | PredicateNOT _ -> false
  | PredicateAND _ -> false
  | Opt expr -> should_export rules names expr
  | Plus expr -> should_export rules names expr
  | Star expr -> should_export rules names expr
  | Literal _ -> false
  | Class _ -> false
  | Any -> false
  | Alternate (e1, e2) -> should_export rules names e1
  | Sequence (e1, e2) ->
    should_export rules names e1 || should_export rules names e2

let rec make_rule_expr _loc rules names = function
  | Epsilon ->
    <:expr< peg_stub >>

  | Name name ->
    <:expr< $lid:rule_prefix ^ name$ >>

  | Sequence (Pattern (name, expr), xs) ->
    let rules = (name, Epsilon) :: rules in
      <:expr< fun input ->
        match get_pattern $make_rule_expr _loc rules names expr$ input with
          | Parsed (r, input) ->
            let $lid:rule_prefix ^ name$ = match_pattern r in
              $make_rule_expr _loc rules names xs$ input
          | Failed as failed -> failed
            >>
            
  | Sequence (x1, x2) ->
    let export_left = should_export rules names x1
    and export_right = should_export rules names x2 in
    let seq =
      match export_left, export_right with
        | true, true -> "seq_b"
        | true, false -> "seq_l"
        | false, true -> "seq_r"
        | false, false -> "seq_n"
    in
      <:expr< $lid:seq$ 
        $make_rule_expr _loc rules names x1$
        $make_rule_expr _loc rules names x2$ >>

  | Alternate (x1, x2) ->
    <:expr< alt $make_rule_expr _loc rules names x1$
      $make_rule_expr _loc rules names x2$ >>

  | Tokenizer t ->
    <:expr< get_lexeme $make_rule_expr _loc rules names t$ >>
 
  | Action  ((line, col), code, expr) ->
    let code_expr =
      try Caml.AntiquotSyntax.parse_expr Loc.ghost code
      with exn ->
        Printf.printf "Bad action %d:%d %S\n" line col code;
        Printf.printf "Exception: %s\n" (Printexc.to_string exn);
        Pervasives.exit 1
    in
      <:expr< transform
        $make_rule_expr _loc rules names expr$
        $code_expr$
      >>
        
  | Opt t ->
    let export = should_export rules names t in
      if export then
        <:expr< opt_accu $make_rule_expr _loc rules names t$ >>
      else
        <:expr< opt $make_rule_expr _loc rules names t$ >>
      
  | Star t ->
    let export = should_export rules names t in
      if export then
        <:expr< star_accu $make_rule_expr _loc rules names t$ >>
      else
        <:expr< star $make_rule_expr _loc rules names t$ >>

  | Plus t ->
    let export = should_export rules names t in
      if export then
        <:expr< plus_accu $make_rule_expr _loc rules names t$ >>
      else
        <:expr< plus $make_rule_expr _loc rules names t$ >>
      
  | PredicateNOT t ->
    <:expr< predicate_not $make_rule_expr _loc rules names t$ >>

  | PredicateAND t ->
    <:expr< predicate_and $make_rule_expr _loc rules names t$ >>
      
  | Any ->
    <:expr< test_any >>
      
  | Literal chars -> (
    match chars with
      | [] -> <:expr< >>
      | [x] -> <:expr< test_char $`chr:x$ >>
      | _ ->
          let str = String.create (List.length chars) in
            ignore (List.fold_left (fun i c -> str.[i] <- c; succ i) 0 chars);
            <:expr< match_pattern $str:String.escaped str$ >>
  )
  | Class classes ->
    let make_expr = function
      | Range (x1, x2) ->
        <:expr< c >= $`int:Char.code x1$ && c <= $`int:Char.code x2$ >>
      | Char x -> <:expr< c = $`int:Char.code x$ >>
    in
    let exprs =
      List.fold_left (fun acc s ->
        <:expr< $make_expr s$ || $acc$ >>
      ) <:expr< $make_expr (List.hd classes)$ >> (List.tl classes) in
      <:expr< test_f (fun c -> let c = Char.code c in $exprs$) >>
        
  | t ->
    Printf.printf "Lack\n\n";
    print_token t;
    assert false
    
let generate declaration rules output_file =
  let (start_rule, _) = List.hd rules in
  let _loc = Loc.ghost in
  let bindings = List.map (fun (name, expr) ->
    Printf.printf "Generating for rule %s\n" name;
    <:binding< $lid:rule_prefix ^ name$ input =
      Printf.printf "Trying %s... " $str:name$;
    match $make_rule_expr _loc  rules [name] expr$ input with
        | Failed as result -> Printf.printf "Failed %s\n" $str:name$; result
        | Parsed _ as result -> Printf.printf "Success %s\n" $str:name$; result
          >>
  ) rules in

    Caml.print_implem ~output_file
    <:str_item<
      $match declaration with
        | Some dcl ->
          Caml.parse_implem _loc (Stream.of_string dcl)
        | None ->
          <:str_item< >>
      $
      let parse string =
        let rec $Ast.biAnd_of_list bindings$ in
        let input = Kmb_input.make_input string in
        let result = $lid:rule_prefix ^ start_rule$ input in
          result
          >>;

          Printf.printf "\n\nDone!\n"
