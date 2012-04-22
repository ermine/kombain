open Kmb_grammar
open Camlp4.PreCast;;
open Printf
open Kmb_util

module Caml =
  Camlp4.Printers.OCaml.Make
    (Camlp4OCamlParser.Make
       (Camlp4OCamlRevisedParser.Make(Syntax)))

open Syntax
    
let rec should_export (rules:((string*string list)*token) list) names = function
  | Epsilon | Any | Literal _ | Class _ -> false
  | PredicateNOT _ | PredicateAND _ -> false
  | Bind _ -> false
  | Star t | Plus t | Opt t | Pattern (_, t) | Fail (_, t) ->
    should_export rules names t
  | Sequence (t1, t2) ->
    should_export rules names t1 || should_export rules names t2
  | Alternate (t1, t2) ->
    should_export rules names t1
  | Tokenizer _ | Transform _ -> true
  | Name (n, ps) -> mem_rule n rules
    (*
    if List.mem n names then
      false
    else
      match try Some (find_rule n rules) with _ -> None with
        | None ->
          printf "Warning: Not found rule: %s\n" n;
          false
        | Some t -> should_export rules (n :: names) t
    *)
          
let rec make_rule_expr _loc rules names params verbose = function
  | Epsilon ->
    <:expr< peg_stub >>

  | Name (name, params) ->
    let rec aux_function name ps =
      let make_arg = function
        | Ident id -> <:expr< $lid:id$ >>
        | Func (name, ps) -> <:expr< $aux_function name ps$ >>
        | Value (t, v) ->
          match t with
            | "string" -> <:expr< $str:v$ >>
            | "int" -> <:expr< $int:v$ >>
            | "bool" -> <:expr< $`bool:bool_of_string v$ >>
      in
        List.fold_left (fun args arg -> <:expr< $args$ $make_arg arg$ >>)
        <:expr< $lid:name$ >> ps
    in
    let e =
      if params = [] then
        <:expr< $lid:name$ >>
      else
        <:expr< $aux_function name params$ >>
    in
      if verbose then
        <:expr< fun input ->
          $if mem_rule name rules then
            <:expr< Printf.printf "%s %sTrying %s %s   <- %s\\n"
              (Kmb_input.string_of_current input) (get_offset ())
              $str:name$ $str:String.escaped (string_of_params params)$
              $str:String.escaped (Kmb_grammar.string_of_token
                                     (Kmb_grammar.remove_transforms
                                        (find_rule name rules)))$
            >>
          else
            <:expr< Printf.printf "%s %sCalling external function %s %s\\n"
              (Kmb_input.string_of_current input) (get_offset ())
              $str:name$ $str:String.escaped (string_of_params params)$ >>
              $;
      incr offset;
      let r = $e$ input in
        decr offset;
        (match r with
          | Parsed (_, newinp) -> Printf.printf "%s %sSuccess %s\\n"
            (Kmb_input.string_of_current newinp) (get_offset ()) $str:name$
          | Failed -> Printf.printf "%s %sFailed %s\\n"
            (Kmb_input.string_of_current input) (get_offset ()) $str:name$
        ); r
            >>
            else e

  | Sequence (Pattern (name, expr), xs) ->
    <:expr< fun input ->
      match get_pattern $make_rule_expr _loc rules names params verbose expr$
        input with
          | Parsed (r, input) ->
            let $lid:name$ = match_pattern r in
              $make_rule_expr _loc rules names params verbose xs$ input
          | Failed as failed -> failed
            >>

  | Pattern _ -> assert false

  | Sequence (Bind (var, vars, t), xs) ->
    let ps =
      Ast.paCom_of_list (List.map (fun i -> <:patt< $lid:i$ >>) (var::vars)) in
      
      <:expr< fun input ->
        match $make_rule_expr _loc rules names params verbose t$ input with
          | Parsed (( $tup:ps$ ), input) ->
            $make_rule_expr _loc rules names params verbose xs$ input
          | Failed as failed -> failed
            >>
                     
  | Bind _ -> assert false

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
        $make_rule_expr _loc rules names params verbose x1$
        $make_rule_expr _loc rules names params verbose x2$ >>

  | Alternate (x1, x2) ->
    <:expr< alt $make_rule_expr _loc rules names params verbose x1$
      $make_rule_expr _loc rules names params verbose x2$ >>

  | Tokenizer t ->
    if verbose then
      <:expr< fun input ->
        Printf.printf "%s get_lexeme\\n"
          (Kmb_input.string_of_current input) ;
        match get_lexeme $make_rule_expr _loc rules names params verbose t$ input
        with
          | Parsed (r, input) as ok ->
            Printf.printf "%s get_lexeme result: %S\\n"
              (Kmb_input.string_of_current input)
              r.Kmb_input.lexeme; ok
          | Failed -> Failed
            >>
    else
      <:expr< get_lexeme $make_rule_expr _loc rules names params verbose t$ >>
 
  | Transform ({Kmb_input.start = (line, col); Kmb_input.lexeme = code}, expr) ->
    let code_expr =
      try Caml.AntiquotSyntax.parse_expr Loc.ghost code
      with Loc.Exc_located (_, exn) ->
        printf "Bad action %d:%d %S\n" line col code;
        printf "Exception: %s\n" (Printexc.to_string exn);
        Pervasives.exit 1
    in
      <:expr< transform
        $make_rule_expr _loc rules names params verbose expr$
        $code_expr$
      >>

  | Fail (msg, expr) ->
    <:expr< fail
      $make_rule_expr _loc rules names params verbose expr$
      $`str:msg$
    >>

  | Opt t ->
    let export = should_export rules names t in
      if export then
        <:expr< opt_accu $make_rule_expr _loc rules names params verbose t$ >>
      else
        <:expr< opt $make_rule_expr _loc rules names params verbose t$ >>
      
  | Star t ->
    let export = should_export rules names t in
      if export then
        <:expr< star_accu $make_rule_expr _loc rules names params verbose t$ >>
      else
        <:expr< star $make_rule_expr _loc rules names params verbose t$ >>

  | Plus t ->
    let export = should_export rules names t in
      if export then
        <:expr< plus_accu $make_rule_expr _loc rules names params verbose t$ >>
      else
        <:expr< plus $make_rule_expr _loc rules names params verbose t$ >>
      
  | PredicateNOT t ->
    <:expr< predicate_not $make_rule_expr _loc rules names params verbose t$ >>

  | PredicateAND t ->
    <:expr< predicate_and $make_rule_expr _loc rules names params verbose t$ >>
      
  | Any ->
    if verbose then
      <:expr< fun input ->
        Printf.printf "%s test_any\\n" (Kmb_input.string_of_current input);
        test_any input
        >>
    else
      <:expr< test_any >>
      
  | Literal chars -> (
    match chars with
      | [] -> assert false
      | [x] ->
        if verbose then
          <:expr< fun input ->
            Printf.printf "%s test_char %S\\n"
              (Kmb_input.string_of_current input)
              $str:Kmb_input.string_of_cslit [x]$;
            test_char $`int:x$ input
            >>
        else
          <:expr< test_char $`int:x$ >>
      | _ ->
        let clist =
          List.fold_right (fun c acc ->
            <:expr< $`int:c$ :: $acc$ >>) chars <:expr< [] >> in
          if verbose then
            <:expr< fun input ->
              Printf.printf "%s match_pattern %S\\n"
                (Kmb_input.string_of_current input)
                $str:Kmb_input.string_of_cslit chars$;
              match_pattern $clist$ input >>
          else
            <:expr< match_pattern $clist$ >>
  )
  | Class classes ->
    let make_expr = function
      | Range (x1, x2) ->
        <:expr< c >= $`int:x1$ && c <= $`int:x2$ >>
      | Char x -> <:expr< c = $`int:x$ >>
    in
      match List.rev classes with
        | [] -> assert false
        | [Char c] ->
          make_rule_expr _loc rules names params verbose (Literal [c])
        | x :: xs ->
          let exprs =
            List.fold_left (fun acc s ->
              <:expr< $make_expr s$ || $acc$ >>
            ) <:expr< $make_expr x$ >> xs in
            if verbose then
              <:expr< fun input ->
                Printf.printf "%s test_class %s\\n"
                  (Kmb_input.string_of_current input)
                  $str:String.escaped (Kmb_grammar.string_of_class classes)$;
                test_class (fun c -> $exprs$) input
                >>
            else
              <:expr< test_class (fun c -> $exprs$) >>
        
let make_rule_function _loc verbose (name, params) expr rules =
  printf "Generating for rule %s\n" name;
  let e =
    List.fold_left (fun expr arg -> <:expr< fun $lid:arg$ -> $expr$ >>)
    <:expr< $make_rule_expr _loc rules [name] params verbose expr$ input >>
      ("input" :: List.rev params)
  in
    <:str_item< let $lid:name$ = $e$ >>

let generate verbose declaration rules start_rule output_file =
  let rules = try_optimize rules in
  let rules = remove_unused_rules start_rule rules in
  let sorted = rearrange_grammar rules in
  let infered = Kmb_util.try_infer rules in
  let _loc = Loc.ghost in
  let bindings =
    List.fold_left (fun acc -> function
      | Simple simples ->
        List.fold_left (fun acc ((name, params), expr) ->
          make_rule_function _loc verbose (name, params) expr
            infered :: acc) acc simples
      | Recursive rs ->
        let bs =
          List.map (fun ((name, params), expr) ->
            printf "Generating for rule %s\n" name;
            let e =
              List.fold_left (fun expr arg -> <:expr< fun $lid:arg$ -> $expr$ >>)
                <:expr< $make_rule_expr _loc infered [name] params verbose expr$
                input >>
                ("input" :: List.rev params)
            in
              <:binding< $lid:name$ = $e$ >>
          ) rs in
          <:str_item< let rec $Ast.biAnd_of_list bs$ >> :: acc
    ) [] sorted
  in
    Caml.print_implem ~output_file
    <:str_item<
      open Kmb_lib

      $if verbose then
            <:str_item< let offset = ref 0
                        let get_offset () =
                          if !offset > 0 then String.make !offset ' ' else ""
                            >>
          else
            <:str_item< >>$
      
      $match declaration with
        | Some dcl ->
          Caml.parse_implem _loc (Stream.of_string dcl)
        | None ->
          <:str_item< >>
      $
            
$list:List.rev bindings$
            
let parse input =
  $lid:start_rule$ input
    >>;
    
  printf "\n\nDone!\n"


