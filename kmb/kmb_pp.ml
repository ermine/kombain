open Kmb_grammar
open Format

let pp_char c =
  if c < 255 then
    match Char.chr c with
      | '\n' -> "\\n"
      | '\r' -> "\\r"
      | '\b' -> "\\b"
      | '\t' -> "\\t"
      | '"' -> "\\\""
      | '\\' -> "\\\\"
      | c -> String.make 1 c
  else
    Printf.sprintf "\\x%X" c

let pp_range c =
  if c < 255 then
    match Char.chr c with
      | '[' -> "\\["
      | ']' -> "\\]"
      | '-' -> "\\-"
      | _ -> pp_char c
  else
    pp_char c

let rec pp_params ppf = function
  | [] -> ()
  | p :: ps ->
    let pp_param = function
      | Ident n -> fprintf ppf "%s" n
      | Value (t, v) -> (
        match t with
          | "bool" -> fprintf ppf "%s" v
          | "string" -> fprintf ppf "\"%s\"" (String.escaped v)
          | "int" -> fprintf ppf "%s" v
          | _ -> failwith "unknown value type"
      )
      | Func (name, ps) ->
        fprintf ppf "@[%s" name;
        pp_params ppf ps;
        fprintf ppf "@]"
    in
      fprintf ppf "(@[";
      pp_param p;
      List.iter (fun p ->
        fprintf ppf ",@ ";
        pp_param p
      ) ps;
      fprintf ppf "@])"

let pp_literal cs =
  "\"" ^ String.concat "" (List.map pp_char cs) ^ "\""
                   
let pp_class ppf cs =
  fprintf ppf "[";
  List.iter (function
    | Range (c1, c2) -> fprintf ppf "%s-%s" (pp_range c1) (pp_range c2)
    | Char c -> fprintf ppf "%s" (pp_range c)
  ) cs;
  fprintf ppf "]"

let rec pp_token ppf = function
  | Epsilon -> fprintf ppf "%s" " "
  | Any -> fprintf ppf "."
  | Literal cs -> fprintf ppf "%s" (pp_literal cs)
  | Class cs -> pp_class ppf cs
  | Name (name, params) ->
    fprintf ppf "@[<h 0>%s" name;
    pp_params ppf params;
    fprintf ppf "@]"
  | PredicateNOT t ->
    fprintf ppf "@[<h 0>!";
    groupped ppf t;
    fprintf ppf "@]"
  | PredicateAND t ->
    fprintf ppf "@[<h 0>&";
    groupped ppf t;
    fprintf ppf "@]"
  | Opt t ->
    fprintf ppf "@[<h 0>";
    groupped ppf t;
    fprintf ppf "?@]";
  | Star t ->
    fprintf ppf "@[<h 0>";
    groupped ppf t;
    fprintf ppf "*@]";
  | Plus t ->
    fprintf ppf "@[<h 0>";
    groupped ppf t;
    fprintf ppf "+@]";
  | Sequence (s1, s2) -> (
    match s1, s2 with
      | Alternate _, Alternate _ ->
        groupped ppf s1;
        fprintf ppf "@;<1 0>";
        groupped ppf s2;
      | Alternate _, _ ->
        groupped ppf s1;
        fprintf ppf "@;<1 0>";
        pp_token ppf s2
      | _, Alternate _ ->
        pp_token ppf s1;
        fprintf ppf "@;<1 0>";
        groupped ppf s2;
      | _, _ ->
        fprintf ppf "@[<hov 0>";
        pp_token ppf s1;
        fprintf ppf "@;<1 0>";
        pp_token ppf s2;
        fprintf ppf "@]"
  );
  | Alternate (a1, a2) ->
    pp_token ppf a1;
    fprintf ppf "@;<1 0>@[<h 0>/ ";
    pp_token ppf a2;
    fprintf ppf "@]"
  | Pattern (name, t) ->
    fprintf ppf "@[<h 0>%s@@" name;
    groupped ppf t;
    fprintf ppf "@]"
  | Transform (fn, t) ->
    fprintf ppf "@[<v 0>";
    pp_token ppf t;
    fprintf ppf "@ { %s }" fn.Kmb_input.lexeme;
    fprintf ppf "@]"
  | Tokenizer t ->
    fprintf ppf "< ";
    pp_token ppf t;
    fprintf ppf "%s" " >";
  | Bind (var, vars, t) ->
    fprintf ppf "@[<hov 1>";
    if vars <> [] then
      fprintf ppf "(";
    fprintf ppf "@[<hov 1>%s" var;
    List.iter (fun v -> fprintf ppf ",@ %s" v) vars;
    fprintf ppf "@]";
    if vars <> [] then
      fprintf ppf ")";
    fprintf ppf " =@ ";
    groupped ppf t;
    fprintf ppf "@]"
and groupped ppf t =
  let group =
    match t with
      | Any
      | Name _
      | Class _
      | Literal _
      | Tokenizer _
      | Transform _ -> false
      | _ -> true
  in
    if group then
      fprintf ppf "(";
    fprintf ppf "@[<hov 0>";
    pp_token ppf t;
    fprintf ppf "@]";
    if group then
      fprintf ppf ")"


 let pp_rule ppf((name, params), expr) =
   let args =
     match params with
       | [] -> ""
       | p :: ps -> List.fold_left (fun acc p -> acc ^ "," ^ p) p ps
   in
     fprintf ppf "%-20s <- @["
       (if params <> [] then name ^ "(" ^ args ^ ")" else name);
     pp_token ppf expr;
     fprintf ppf "@."

