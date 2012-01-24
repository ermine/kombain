
type defined_as =
  | Define
  | IncrAlt

type repeat =
  | CountRepeat of int
  | RangeRepeat of int option * int option


type token =
  | Alternation of token * token list
  | Concatenation of token * token list
  | Repeat of repeat * token
  | Name of string
  | Option  of token
  | Literal of string
  | Class of Kmb_grammar.class_t list
  | Prose of Kmb_lib.lexeme


let convert_name name =
  let rec aux_replace () =
    try
      let i = String.index name '-' in
        name.[i] <- '_';
        aux_replace ()
    with _ -> ()
  in
    aux_replace ();
    name
 
     
let rec make_peg = function
  | Concatenation (c1, cs) -> (
    match List.rev cs with
      | [] -> make_peg c1
      | x :: xs ->
        let t = List.fold_right (fun x acc ->
          Kmb_grammar.Sequence (make_peg x, acc)) xs (make_peg x) in
          Kmb_grammar.Sequence (make_peg c1, t)
  )
  | Alternation (c1, cs) -> (
    match List.rev cs with
      | [] -> make_peg c1
      | x :: xs ->
        let t = List.fold_right (fun x acc ->
          Kmb_grammar.Alternate (make_peg x, acc)) xs (make_peg x) in
          Kmb_grammar.Alternate (make_peg c1, t)
  )
  | Repeat (r, e) -> (
    let token = make_peg e in
      match r with
        | CountRepeat i ->
          let rec aux_repeat acc = function
            | 0 -> acc
            | i -> aux_repeat (Kmb_grammar.Sequence (token, acc)) (pred i)
          in
            aux_repeat token i
        | RangeRepeat (min, max) ->
          let min =
            match min with
              | None
              | Some 0 -> 0
              | Some v -> v in
          let rec aux_min acc = function
            | 0 -> acc
            | i -> aux_min (Kmb_grammar.Sequence (token, acc)) (pred i)
          in
            match max with
              | None ->
                if min = 0 then Kmb_grammar.Star token
                else if min = 1 then Kmb_grammar.Plus token
                else
                  aux_min token (min-1)
              | Some 0 ->
                if min > 0 then failwith "Invalid repeat range"
                else Kmb_grammar.PredicateNOT token
              | Some m ->
                let rec aux_max acc = function
                  | 0 -> acc
                  | i ->
                    aux_max (Kmb_grammar.Opt (Kmb_grammar.Sequence (token, acc)))
                    (pred i)
                in
                let r = aux_max (Kmb_grammar.Opt token) (m - 1) in
                  aux_min r (min - 1)

  )
  | Name name -> Kmb_grammar.Name (convert_name name)
  | Option e -> Kmb_grammar.Opt (make_peg e)
  | Literal str ->
    let len = String.length str in
    let rec aux_iter i =
      if i < len then
        Char.code str.[i] :: aux_iter (succ i)
      else
        []
    in
    let cs = aux_iter 0 in
      Kmb_grammar.Literal cs
  | Class cs ->
    Kmb_grammar.Class cs
  | Prose l ->
    Kmb_grammar.Transform
      (l.Kmb_lib.start, 
       (Printf.sprintf 
          "fun _ -> failwith \"Please implement prose-val %S\""
          l.Kmb_lib.lexeme),
       Kmb_grammar.Epsilon)

