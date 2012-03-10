open Kmb_lib
  
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

let simple_cmp s1 s2 input =
  if s1 = s2 then
    Parsed ((), input)
  else
    Failed

let is_production name =
  if String.length name > 2 && name.[1] = '-' then
    match name.[0] with
      | 'e' | 'c' | 'b' | 's' | 'l' -> true
      | _ -> false
  else if String.length name > 3 && name.[2] = '-' then
    match String.sub name 0 2 with
      | "nb" | "ns" -> true
      | _ -> false
  else
    false
    
let is_context = function
  | "block-out"
  | "block-in"
  | "flow-out"
  | "flow-in"
  | "block-key"
  | "flow-key"

  | "strip"
  | "keep"
  | "clip" -> true
  | _ -> false

let convert_name name =
  if not (is_context name) then
    let rec aux_convert pos c =
      let pos =
        try Some (String.index_from name pos c)
        with Not_found -> None in
        match pos with
          | Some pos ->
            name.[pos] <- '_';
            aux_convert pos c
          | None -> name
    in
      aux_convert 0 '-';
      aux_convert 0 '+'
  else
    name



(* 136 *)
let in_flow c =
  match c with
    | "flow-out" -> "flow-in"
    | "flow-in" -> "flow-in"
    | "block-key" -> "flow-key"
    | "flow-key" -> "flow-key"
    | _ -> assert false
(* 201 *)
let seq_spaces n c =
  match c with
    | "block-out" -> n - 1
    | "block-in" -> n
      

let auto_detect input = Parsed (0, input)
  

let na = 0 (* shoild fail *)

let minusodin = (-1)
  
let add n m = n + m
let neg n = (-n)

let sol input =
  let open Kmb_input in
        if input.col = 0 then Parsed ((), input) else Failed


open Kmb_grammar

let return m t input = Parsed ((m, t), input)

let rec rewrite_rule = function
  | Sequence (Name ("c_indentation_indicator", _),
              Name ("c_chomping_indicator",_)) ->
    Transform ({Kmb_input.start = (0,0); stop = (0,0);
                lexeme = "fun (m, t) -> (m, t)"},
               Sequence (Name ("c_indentation_indicator", []),
                         Name ("c_chomping_indicator",[])))
  | Sequence (Name ("c_chomping_indicator",_),
              Name ("c_indentation_indicator", _)) ->
    Transform ({Kmb_input.start = (0,0); stop = (0,0);
                lexeme = "fun (t, m) -> (m, t)"},
               Sequence (Name ("c_chomping_indicator",[]),
                         Name ("c_indentation_indicator", [])))
  | Name (name, ps) as ok ->
    if name = "c_b_block_header" then
      Bind ("m", ["t"], Name (name, []))
    else
      ok
  | Sequence (s1, s2) ->
    Sequence (rewrite_rule s1, rewrite_rule s2)
  | Alternate (s1, s2) ->
    Alternate (rewrite_rule s1, rewrite_rule s2)
  | ok -> ok

let tokens = ["ns_uri_char";
              "ns_word_char";
              "ns_tag_char";
             ]

let try_tokenize ((name, ps), expr) =
  if String.sub name 0 3 = "ns_" then
    ((name, ps), expr)
  else
    let is_token = function
      | Name (name, _) -> List.mem name tokens
      | _ -> false
    in
    let rec aux_tokenize = function
      | Name (name, _) as t ->
        if List.mem name tokens then
          Tokenizer t
        else
          t
      | Sequence (s1, s2) ->
        Sequence (aux_tokenize s1, aux_tokenize s2)
      | Alternate (s1, s2) ->
        Alternate (aux_tokenize s1, aux_tokenize s2)
      | Opt t ->
        if is_token t then Tokenizer (Opt t)
        else Opt (aux_tokenize t)
      | Star t ->
        if is_token t then Tokenizer (Star t)
        else Star (aux_tokenize t)
      | Plus t ->
        if is_token t then Tokenizer (Plus t)
        else Plus (aux_tokenize t)
      | Transform (c, t) ->
        Transform (c, aux_tokenize t)
      | Bind (a, b, t) ->
        Bind (a, b, aux_tokenize t)
      | t -> t
    in
      ((name, ps), aux_tokenize expr)


let tr body expr =
  Transform (
    { Kmb_input.start = (0,0); stop = (0,0); lexeme = body},
    expr)

type tag_handle =
  | PrimaryTagHandle
  | SecondaryTagHandle
  | NamedTagHandle of string

type tag_property =
  | VerbatimTag of string
  | ShorthandTag of tag_handle * string
  | NonSpecificTag

type tag_prefix =
  | LocalTagPrefix of string
  | GlobalTagPrefix of string
      
type directive =
  | YAMLVersion of string
  | TagDirective of tag_handle * tag_prefix
  | ReservedDirective of string * string list

type properties =
  | TagProperty of tag_property * string option
  | AnchorProperty of string * tag_property option

and content =
  | Scalar of string
  | Seq of seq_entry list

and flow_node =
  | Alias of string
  | Content of content
  | Properties of properties * content

and seq_entry =
  | Pair of seq_entry * seq_entry
  | Node of flow_node
  | Block of seq_entry list

let make_transform ((name, ps), expr) =
  let newexpr =
    match name with
      | "ns_global_tag_prefix" ->
        tr "fun s -> GlobalTagPrefix s.Kmb_input.lexeme" (Tokenizer expr)
      | "c_ns_local_tag_prefix" ->
        tr "fun s -> LocalTagPrefix s.Kmb_input.lexeme" expr
      | "c_primary_tag_handle" ->
        tr "fun () -> PrimaryTagHandle" expr
      | "c_secondary_tag_handle" ->
        tr "fun () -> SecondaryTagHandle" expr
      | "c_named_tag_handle" ->
        tr "fun s -> NamedTagHandle s.Kmb_input.lexeme" expr
      | "c_verbatim_tag" ->
        tr "fun s -> VerbatimTag s.Kmb_input.lexeme" expr
      | "c_ns_shorthand_tag" ->
        tr "fun (handle, s) -> ShorthandTag (handle, s.Kmb_input.lexeme)" expr
      | "c_non_specific_tag" ->
        tr "fun () -> NonSpecificTag" expr
      | "ns_reserved_directive" ->
        tr "fun (name, ps) -> ReservedDirective (name, ps)"
        expr
      | "ns_directive_parameter" ->
        tr "fun s -> s.Kmb_input.lexeme" (Tokenizer expr)
      | "ns_directive_name" ->
        tr "fun s -> s.Kmb_input.lexeme" (Tokenizer expr)
      | "ns_yaml_version" ->
        tr "fun s -> YAMLVersion s.Kmb_input.lexeme" (Tokenizer expr)
      | "ns_tag_directive" ->
        tr "fun (h,p) -> TagDirective (h, p)" expr
      | "c_ns_properties" -> (
        match expr with
          | Alternate (a1, a2) ->
            Alternate (tr "fun (p,a) -> TagProperty (p,a)" a1,
                       tr "fun (a,p) -> AnchorProperty (a,p)" a2)
          | _ -> assert false
      )
      | "ns_anchor_name" ->
        tr "fun s -> s.Kmb_input.lexeme" (Tokenizer expr)
      | "c_ns_alias_node" ->
        tr "fun s -> Alias s" expr
      | "e_node" ->
        tr "fun () -> Empty" expr
      | "ns_plain_one_line" ->
        tr "fun s -> s.Kmb_input.lexeme" (Tokenizer expr)
      | "ns_flow_pair" ->
        tr "fun (n1, n2) -> Pair (n1, n2)" expr
        
      | _ -> expr
  in
    ((name, ps), newexpr)

let cleanup_rule ((name, ps), expr) =
  if name = "c_indentation_indicator" ||
    name = "c_chomping_indicator" then
    try_tokenize ((name, []), expr)
  else if name = "c_b_block_header" then
    try_tokenize ((name, []), rewrite_rule expr)
  else
    make_transform (try_tokenize ((name, ps), expr))
    
let pmax n m input = Parsed ((max n m), input)
  

(*
type yaml =
  | Scalar of string
  | Pair of scalar * yaml
  | Block of yaml list
*)
