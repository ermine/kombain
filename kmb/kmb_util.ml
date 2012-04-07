open Kmb_grammar
  
let find_rule name (rules:((string*string list)*token) list) =
  let (_, token) = List.find (fun ((n, _), _) -> n = name) rules in
    token

let mem_rule name rules =
  List.exists (fun ((n, _), _) -> n = name) rules
        
let rec get_prefix acc = function
  | [], t2 -> List.rev acc, Literal t2
  | t1, [] -> List.rev acc, Alternate (Literal t1, Epsilon)
  | x :: xs, z :: zs ->
    if x = z then
      get_prefix (x :: acc) (xs, zs)
    else
      List.rev acc, Alternate (Literal (x::xs), Literal (z::zs))

let rec add_alternate t1 t2 =
  match t1 with
    | Alternate (a1, a2) -> Alternate (a1, add_alternate a2 t2)
    | _ -> Alternate (t1, t2)

let rec analyze_alts = function
  | Epsilon | Name _ | Any | Class _ | Literal _ as t -> t
  | Opt t -> Opt (analyze_alts t)
  | Star t -> Star (analyze_alts t)
  | Plus t -> Plus (analyze_alts t)
  | PredicateAND t -> PredicateAND (analyze_alts t)
  | PredicateNOT t -> PredicateNOT (analyze_alts t)
  | Tokenizer t -> Tokenizer (analyze_alts t)
  | Bind (v, vs, t) -> Bind (v, vs, analyze_alts t)
  | Pattern (n, t) -> Pattern (n, analyze_alts t)
  | Transform (f, t) -> Transform (f, analyze_alts t)
  | Sequence (s1, s2) -> Sequence (analyze_alts s1, analyze_alts s2)
  | Alternate (a1, a2) ->
    let z1 = analyze_alts a1 and z2 = analyze_alts a2 in
      match z1, z2 with
        | Sequence (s1, s2), Sequence (t1, t2) ->
          if s1 = t1 then
            Sequence (s1, Alternate (s2, t2))
          else
            Alternate (z1, z2)
        | Literal l1, Literal l2 ->
          let (prefix, tail) = get_prefix [] (l1, l2) in
            if prefix = [] then
              Alternate (Literal l1, Literal l2)
            else
              Sequence (Literal prefix, tail)
(*                
        | Literal l1, Alternate (Literal l2, t) ->
          let prefix, tail = get_prefix [] (l1, l2) in
            if prefix = [] then
              Alternate (Literal l1, Alternate (Literal l2, t))
            else
              Sequence (Literal prefix, add_alternate tail t)
*)                
        |  _, _ ->
          Alternate (z1, z2)
    
    

let  try_optimize rules =
  let resolve (name, _) =
    try Some (find_rule name rules) with Not_found -> None in
  let rec concat_class = function
    | Literal [l1], Literal [l2] -> Some (Class [Char l1; Char l2])
    | Literal [l], Class cs -> Some (Class (Char l :: cs))
    | Class cs, Literal [l] -> Some (Class (cs @ [Char l]))
    | Class cs1, Class cs2 -> Some (Class (cs1 @ cs2))
    | Name n1, Name n2 -> (
      match resolve n1, resolve n2 with
        | Some t1, Some t2 -> concat_class (t1, t2)
        | _ -> None)
    | Name n, t2 -> (
      match resolve n with
        | Some t -> concat_class (t, t2)
        | _ -> None)
    | t, Name n -> (
      match resolve n with
        | Some t2 -> concat_class (t, t2)
        | _ -> None
    )
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
    List.map (fun (name, expr) -> name, aux_optimize (analyze_alts expr)) rules
                      
