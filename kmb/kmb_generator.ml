open Kmb_grammar
open Camlp4.PreCast;;
open Printf

module Caml =
  Camlp4.Printers.OCaml.Make
    (Camlp4OCamlParser.Make
       (Camlp4OCamlRevisedParser.Make(Syntax)))


open Syntax
    
let find_rule name (rules:((string*string list)*token) list) =
  let (_, token) = List.find (fun ((n, _), _) -> n = name) rules in
    token
    
let mem_rule name rules =
  List.exists (fun ((n, _), _) -> n = name) rules
  
let rec should_export (rules:((string*string list)*token) list) names = function
  | Epsilon -> false
  | Name (name, params) ->
    (* if params = [] then *)
      if List.mem name names then
        (* recursive definition like A <- A *)
        false
      else (
        try
          if not (should_export rules (name :: names) (find_rule name rules))
          then if params = [] then
              false
            else
              false
          else
            true
                (*
              List.find (function
                | Ident n ->
                  try should_export rules (n :: names) (find_rule n)
                  )
                *)
        with Not_found ->
          printf "Warning: Not found rule: %s\n" name;
          false
      )
  | Transform _ -> true
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
  | Bind (var, vars, t) -> false


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
      if params = [] then
        if verbose then
          <:expr< fun input ->
            Printf.printf "%s %sCalling %s... "
              (Kmb_input.string_of_current input)
              (String.make !offset ' ') $str:name$;
            $if mem_rule name rules then
              <:expr< Printf.printf "\\n"; $lid:name$ input >>
            else
              <:expr< match $lid:name$ input with
                | Parsed _ as ok -> Printf.printf "successed\\n"; ok
                | Failed as failed -> Printf.printf "failed\\n"; failed
                  >>
              $ >>
        else
          <:expr< $lid:name$ >>
      else
        if verbose then
          <:expr< fun input ->
            Printf.printf "%s %sCalling %s(%s)... "
              (Kmb_input.string_of_current input)
              (String.make !offset ' ') $str:name$
              $str:String.escaped (string_of_params params)$;
            $if mem_rule name rules then
              <:expr< Printf.printf "\\n"; $aux_function name params$ input >>
                else 
              <:expr< match $aux_function name params$ input with
                | Parsed _ as ok -> Printf.printf "successed\\n"; ok
                | Failed as failed -> Printf.printf "failed\\n"; failed
                  >>$ >>
        else
          <:expr< $aux_function name params$>>

  | Sequence (Pattern (name, expr), xs) ->
    let rules = ((name, []), Epsilon) :: rules in
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
      with exn ->
        printf "Bad action %d:%d %S\n" line col code;
        printf "Exception: %s\n" (Printexc.to_string exn);
        Pervasives.exit 1
    in
      <:expr< transform
        $make_rule_expr _loc rules names params verbose expr$
        $code_expr$
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
                Printf.printf "%s test_class\\n"
                  (Kmb_input.string_of_current input);
                test_class (fun c -> $exprs$) input
                >>
            else
              <:expr< test_class (fun c -> $exprs$) >>
        
let make_rule_body _loc verbose name params expr rules =
  if verbose then (
    <:expr<
      Printf.printf "%s %sTrying %s\\n"
      (Kmb_input.string_of_current input)
      (if !offset > 0 then String.make !offset ' ' else "")
      $str:String.escaped (string_of_rule ((name, params), expr))$;
    incr offset;
    let result = $make_rule_expr _loc  rules [name] params verbose expr$ input in
      (match result with
        | Failed -> decr offset; Printf.printf "%s %sFailed %s\\n"
          (Kmb_input.string_of_current input)
          (if !offset > 0 then String.make !offset ' ' else "")
          $str:name$
        | Parsed _ -> decr offset; Printf.printf "%s %sSuccess %s\\n"
          (Kmb_input.string_of_current input)
          (if !offset > 0 then String.make !offset ' ' else "")
          $str:name$
      );
      result
      >>
  )
  else 
  <:expr< $make_rule_expr _loc rules [name] params verbose expr$ input >>
  
let make_rule_function _loc verbose (name, params) expr rules =
  printf "Generating for rule %s\n" name;
  let e =
    List.fold_left (fun expr arg -> <:expr< fun $lid:arg$ -> $expr$ >>)
      (make_rule_body _loc verbose name params expr rules)
      ("input" :: List.rev params)
  in
    <:str_item< let $lid:name$ = $e$ >>


let  try_optimize rules =
  let concat_class = function
    | Literal [l1], Literal [l2] -> Some (Class [Char l1; Char l2])
    | Literal [l], Class cs -> Some (Class (Char l :: cs))
    | Class cs, Literal [l] -> Some (Class (cs @ [Char l]))
    | Class cs1, Class cs2 -> Some (Class (cs1 @ cs2))
    | _ -> None
  in
  let rec aux_optimize = function
    | Alternate (a1, Alternate (a2, tail)) -> (
      match concat_class (a1, a2) with
        | None ->
          Alternate (aux_optimize a1, aux_optimize (Alternate (a2, tail)))
        | Some r ->
          aux_optimize (Alternate (r, tail))
    )
    | Alternate (a1, a2) -> (
      match concat_class (a1, a2) with
        | None -> Alternate (aux_optimize a1, aux_optimize a2)
        | Some r -> r
    )
    | Sequence (Literal l1, Literal l2) ->
      Literal (l1 @  l2)
    | Sequence (Literal l1, Sequence (Literal l2, tail)) ->
      aux_optimize (Sequence (Literal (l1 @ l2), tail))
    | Sequence (s1, s2) ->
      Sequence (aux_optimize s1, aux_optimize s2)
    | Opt t -> Opt (aux_optimize t)
    | Plus t -> Plus (aux_optimize t)
    | Star t -> Star (aux_optimize t)
    | Tokenizer t -> Tokenizer (aux_optimize t)
    | Transform (f, t) -> Transform (f, aux_optimize t)
    | PredicateAND t -> PredicateAND (aux_optimize t)
    | PredicateNOT t -> PredicateNOT (aux_optimize t)
    | Bind (v, vs, t) -> Bind (v, vs, aux_optimize t)
    | other -> other
  in
    List.map (fun (name, expr) -> name, aux_optimize expr) rules
                      
let generate verbose declaration rules start_rule output_file =
  let rules = try_optimize rules in
  let sorted = rearrange_grammar rules in
  let _loc = Loc.ghost in
  let bindings =
    List.fold_left (fun acc -> function
      | Simple simples ->
        List.fold_left (fun acc ((name, params), expr) ->
          make_rule_function _loc verbose (name, params) expr
            rules :: acc) acc simples
      | Recursive rs ->
        let bs =
          List.map (fun ((name, params), expr) ->
            printf "Generating for rule %s\n" name;
            let e =
              List.fold_left (fun expr arg -> <:expr< fun $lid:arg$ -> $expr$ >>)
                (make_rule_body _loc verbose name params expr rules)                
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
      
      $match declaration with
        | Some dcl ->
          Caml.parse_implem _loc (Stream.of_string dcl)
        | None ->
          <:str_item< >>
      $
            
$list:List.rev bindings$
            
let parse string =
  let input = Kmb_input.make_input string in
    $lid:start_rule$ input
    >>;
    
    printf "\n\nDone!\n"


