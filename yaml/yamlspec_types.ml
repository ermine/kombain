open Kmb_lib
  
let na = ref 0 (* shoild fail *)

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
      

let auto_detect input = Parsed ((), input)
  

let m = ref 0
let t = ref ""
let minusodin = ref (-1)
  
let add n m = ref (!n + !m)
let succ n = ref (!n + 1)
let pred n = !n - 1

let sol input =
  let open Kmb_input in
        if input.col = 0 then Parsed ((), input) else Failed
  
