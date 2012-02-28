open Kmb_lib
  
let rec repeat fail n symbol input =
  if n = 0 then
    Parsed ((), input)
  else
    match symbol input with
      | Parsed (_, input) -> repeat fail (pred n) symbol input
      | Failed -> if fail then Failed else Parsed ((), input)

let repeat_ref flag n symbol input =
  repeat flag !n symbol input

let make_reject (t, ts) =
  List.fold_right (fun t acc ->
    Kmb_grammar.Sequence (Kmb_grammar.PredicateNOT t, acc)) ts t

let simple_cmp s1 s2 input =
  if s1 = s2 then
    Parsed ((), input)
  else
    Failed

let simple_cmp_ref s1 s2 input =
  if !s1 = s2 then
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
    | "block-out" -> ref (!n - 1)
    | "block-in" -> n
      

let auto_detect input = Parsed (0, input)
  

let na = ref 0 (* shoild fail *)

let minusodin = ref (-1)
  
let add n m = n + m
let succ n = ref (!n + 1)
let pred n = !n - 1

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


let try_tokenize ((name, ps), expr) =
  if String.sub name 0 3 = "ns_" then
    ((name, ps), expr)
  else
    let is_ns = function
      | Name (name, _) -> String.sub name 0 3 = "ns_"
      | _ -> false
    in
    let rec aux_tokenize = function
      | Name (name, _) as t ->
        if String.sub name 0 3 = "ns_" then
          Tokenizer t
        else
          t
      | Sequence (s1, s2) ->
        Sequence (aux_tokenize s1, aux_tokenize s2)
      | Alternate (s1, s2) ->
        Alternate (aux_tokenize s1, aux_tokenize s2)
      | Opt t ->
        if is_ns t then Tokenizer (Opt t)
        else Opt (aux_tokenize t)
      | Star t ->
        if is_ns t then Tokenizer (Star t)
        else Star (aux_tokenize t)
      | Plus t ->
        if is_ns t then Tokenizer (Plus t)
        else Plus (aux_tokenize t)
      | t -> t
    in
      ((name, ps), aux_tokenize expr)

let cleanup_rule ((name, ps), expr) =
  if name = "c_indentation_indicator" ||
    name = "c_chomping_indicator" then
    try_tokenize ((name, []), expr)
  else if name = "c_b_block_header" then
    try_tokenize ((name, []), rewrite_rule expr)
  else
    try_tokenize ((name, ps), rewrite_rule expr)
    
