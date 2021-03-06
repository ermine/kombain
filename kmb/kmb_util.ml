open Kmb_grammar
  
let find_rule name (rules:((string*string list)*token) list) =
  let (_, token) = List.find (fun ((n, _), _) -> n = name) rules in
    token

let get_rule name rules =
  try Some(List.find (fun ((n, _), _) -> n = name) rules)
  with Not_found -> None

let mem_rule name rules =
  List.exists (fun ((n, _), _) -> n = name) rules
        
let resolve (name, _) rules =
  try Some (find_rule name rules) with Not_found -> None

let rec get_prefix acc = function
  | [], [] -> assert false
  | [], t2 -> List.rev acc, Epsilon, Literal t2
  | t1, [] -> List.rev acc, Literal t1, Epsilon
  | x :: xs, z :: zs ->
    if x = z then
      get_prefix (x :: acc) (xs, zs)
    else
      List.rev acc, Literal (x::xs), Literal (z::zs)

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
  | Fail (m, t) -> Fail (m, analyze_alts t)
  | Sequence (s1, s2) -> Sequence (analyze_alts s1, analyze_alts s2)
  | Alternate (a1, a2) ->
    let z1 = analyze_alts a1 and z2 = analyze_alts a2 in
      match z1, z2 with
        | Literal l1, Literal l2 ->
          let prefix, tail1, tail2 = get_prefix [] (l1, l2) in
            if prefix = [] then
              Alternate (z1, z2)
            else if tail1 = Epsilon then (
              Printf.printf "%s\n" (string_of_token a1);
              Printf.printf "%s\n" (string_of_token a2);
              failwith "unordered alternation"
              )
            else (* if tail2 = Epsilon then
                    Sequence (Literal prefix, Opt tail1)
                    else *)
              analyze_alts (Sequence (Literal prefix,
                                      Alternate (tail1, tail2)))

        | Alternate (a1, a2), _ ->
          analyze_alts (Alternate (a1, (Alternate (a2, z2))))

        | s1, Alternate (t1, t2) -> (
          match analyze_alts (Alternate (s1, t1)) with
            | Alternate _ -> Alternate (z1, z2)
            | other -> Alternate (other, t2)
        )
            
        | Literal l1, Sequence (Literal l2, t2) ->
          let prefix, tail1, tail2 = get_prefix [] (l1, l2) in
            if prefix = [] then
              Alternate (z1, z2)
            else if tail1 = Epsilon then
              failwith "unordered alternation"
            else if tail2 = Epsilon then
              Sequence (Literal prefix,
                        analyze_alts (Alternate (tail1, t2)))
            else
              Sequence (Literal prefix,
                        Alternate (tail1, Sequence (tail2, t2)))

        | Sequence (Literal l1, s2), Literal l2 ->
          if l1 = l2 then
            Sequence (Literal l1, Alternate (s2, Epsilon))
          else
            let prefix, tail1, tail2 = get_prefix [] (l1, l2) in
              if prefix = [] then
                Alternate (z1, z2)
              else if tail1 = Epsilon then
                Sequence (Literal prefix, Alternate (s2, tail2))
              else
                Sequence (Literal prefix,
                          Alternate (Sequence (tail1, s2), tail2))

        | Sequence (Literal l1, s2), Sequence (Literal l2, t2) ->
          if l1 = l2 then
            analyze_alts (Sequence (Literal l1, Alternate (s2, t2)))
          else
            let prefix, tail1, tail2 = get_prefix [] (l1, l2) in
              if prefix = [] then
                Alternate (z1, z2)
              else if tail1 = Epsilon then
                analyze_alts (Sequence (Literal prefix,
                                        Alternate (s2,
                                                   Sequence (tail2, t2))))
              else if tail2 = Epsilon then
                analyze_alts (Sequence (Literal prefix,
                                        Alternate (Sequence (tail1, s2), t2)))
              else
                analyze_alts (Sequence (Literal prefix,
                                        Alternate (Sequence (tail1, s2),
                                                 Sequence (tail2, t2))))
                  
        | Sequence (s1, s2), Sequence (t1, t2) ->
          if s1 = t1 then
            analyze_alts (Sequence (s1, Alternate (s2, t2)))
          else
            Alternate (z1, z2)


        |  t1, t2 ->
          if t1 = t2 then
            (* failwith "strange alternation" *)
            t1
          else
            Alternate (z1, z2)
    
    
let rec optimize_epsilon = function
  | PredicateNOT (PredicateNOT t) -> PredicateAND (optimize_epsilon t)
  | Epsilon -> failwith "epsilon"
  | Name _ | Any | Class _ | Literal _ as t -> t
  | Opt t -> Opt (optimize_epsilon t)
  | Star t -> Star (optimize_epsilon t)
  | Plus t -> Plus (optimize_epsilon t)
  | PredicateAND t -> PredicateAND (optimize_epsilon t)
  | PredicateNOT t -> PredicateNOT (optimize_epsilon t)
  | Tokenizer t -> Tokenizer (optimize_epsilon t)
  | Bind (v, vs, t) -> Bind (v, vs, optimize_epsilon t)
  | Pattern (n, t) -> Pattern (n, optimize_epsilon t)
  | Transform (f, Epsilon) as t -> t
  | Transform (f, t) -> Transform (f, optimize_epsilon t)
  | Fail (m, t) -> Fail (m, optimize_epsilon t)
  | Sequence (s1, Epsilon) -> optimize_epsilon s1
  | Sequence (s1, s2) -> Sequence (optimize_epsilon s1, optimize_epsilon s2)
  | Alternate (a1, Epsilon) -> Opt (optimize_epsilon a1)
  | Alternate (a1, a2) -> Alternate (optimize_epsilon a1, optimize_epsilon a2)
              
  let rec is_terminal = function
    | Epsilon | Any | Literal _ | Class _ -> true
    | Opt t | Plus t | Star t | PredicateNOT t | PredicateAND t -> is_terminal t
    | Sequence (s1, s2) -> is_terminal s1 && is_terminal s2
    | Alternate (a1, a2) -> is_terminal a1 && is_terminal a2
    | _ -> false

  let inline_token rules =
  let rec aux_inline names = function
    | Epsilon | Any | Literal _ | Class _ as t -> t
    | Name (n, []) -> (
      if List.mem n names then
        Name (n, [])
      else
        match resolve (n, []) rules with
          | Some t ->
          (* aux_inline (n :: names) t *)
            if is_terminal t then
              t
            else
              Name (n, [])
          | None -> Name (n, [])
            
    )
    | Name (n, ps) as t -> t
    | Opt t -> Opt (aux_inline names t)
    | Star t -> Star (aux_inline names t)
    | Plus t -> Plus (aux_inline names t)
    | PredicateAND t -> PredicateAND (aux_inline names t)
    | PredicateNOT t -> PredicateNOT (aux_inline names t)
    | Tokenizer t -> Tokenizer (aux_inline names t)
    | Transform (f, t) -> Transform (f, aux_inline names t)
    | Fail (m, t) -> Fail (m, aux_inline names t)
    | Pattern (n, t) -> Pattern (n, aux_inline names t)
    | Bind (v, vs, t) -> Bind (v, vs, aux_inline names t)
    | Sequence (s1, s2) -> Sequence (aux_inline names s1, aux_inline names s2)
    | Alternate (a1, a2) -> Alternate (aux_inline names a1, aux_inline names a2)
  in
    List.map (fun ((name, ps), expr) -> (name, ps), aux_inline [name] expr) rules

let  try_optimize rules =
  let rec aux_optimize = function
    | Epsilon | Any | Class _ | Literal _ | Name _ as t -> t
    | Opt t -> Opt (aux_optimize t)
    | Plus t ->Plus (aux_optimize t)
    | Star t -> Star (aux_optimize t)
    | PredicateNOT t -> PredicateNOT (aux_optimize t)
    | PredicateAND t -> PredicateAND (aux_optimize t)
    | Pattern (n, t) -> Pattern (n, aux_optimize t)
    | Tokenizer t -> Tokenizer (aux_optimize t)
    | Transform (f, t) -> Transform (f, aux_optimize t)
    | Fail (m, t) -> Fail (m, aux_optimize t)
    | Bind (v, vs, t) -> Bind (v, vs, aux_optimize t)
    | Alternate (a1, a2) -> (
      let z1 = aux_optimize a1 and z2 = aux_optimize a2 in
        match z1, z2 with
          | Literal [l1], Literal [l2] -> Class [Char l1; Char l2]
          | Literal [l], Class cs -> Class (Char l :: cs)
          | Class cs, Literal [l] -> Class (cs @ [Char l])
          | Class cs1, Class cs2 -> Class (cs1 @ cs2)
          | a1, Alternate (a2, a3) ->
            let r = aux_optimize (Alternate (a1, a2)) in
              if r = Alternate (a1, a2) then
                Alternate (z1, z2)
              else
                Alternate (r, a3)
                                       
          | _, _ -> Alternate (z1, z2)
    )
    | Sequence (s1, s2) ->
      let z1 = aux_optimize s1 and z2 = aux_optimize s2 in
        match z1, z2 with
          | Literal l1, Literal l2 -> Literal (l1 @ l2)
          | Literal l1, Sequence (Literal l2, s3) ->
            Sequence (Literal (l1 @ l2), s3)
          | _, _ -> Sequence (z1, z2)
  in
  let rules = inline_token rules in
    List.map (fun (name, expr) ->
      try name,
        optimize_epsilon (aux_optimize (analyze_alts expr))
      with exn ->
        Printf.printf "%s\n" (string_of_rule  (name, expr));
        raise exn
    ) rules
                      
let remove_unused_rules start rules =
  let rec check_rule n acc =
    if mem_rule n acc then
      acc
    else
      match get_rule n rules with
        | Some (n, t) -> aux_scan ((n, t) :: acc) t
        | None -> acc
  and check_params params acc =
    List.fold_left (fun acc -> function
      | Ident i -> check_rule i acc
      | Func (f, ps) -> check_params ps (check_rule f acc)
      | _ -> acc
    ) acc params
  and aux_scan acc = function
    | Name (n, ps) -> check_params ps (check_rule n acc)
    | Epsilon | Any | Literal _ | Class _ -> acc
    | Sequence (s1, s2) -> aux_scan (aux_scan acc s1) s2
    | Alternate (a1, a2) -> aux_scan (aux_scan acc a1) a2
    | PredicateNOT t | PredicateAND t | Opt t | Star t | Plus t
    | Tokenizer t | Transform (_, t) | Fail (_, t) | Pattern (_, t)
    | Bind (_, _, t) -> aux_scan acc t
  in
    match get_rule start rules with
      | Some (n, t) -> List.rev (aux_scan [n, t] t)
      | None -> assert false


let try_infer rules =
  let rec infer ts fs =
    let nts, nfs = List.fold_left (fun (ts, fs) (((n, ps), expr) as rule) ->
      let rec will_return = function
        | Epsilon | Any | Literal _ | Class _
        | PredicateAND _ | PredicateNOT _ | Bind _ -> false
        | Star t | Plus t | Opt t | Fail (_, t) | Pattern (_, t) -> will_return t
        | Sequence (t1, t2) -> will_return t1 || will_return t2
        | Alternate (t1, t2) -> will_return t1 || will_return t2
        | Tokenizer _ | Transform _ -> true
        | Name (n, ps) -> mem_rule n ts
      in
        if will_return expr then rule :: ts, fs else ts, rule :: fs
    ) (ts, []) fs
  in
    if List.rev nfs = fs then
      nts
    else
      infer nts (List.rev nfs)
  in
    infer [] rules
