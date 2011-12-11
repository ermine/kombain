open Peg_lib
  
let _ = Printexc.record_backtrace true
      
let end_of_line state input =
  peg_alternate [
    peg_sequence [test_char '\r'; test_char '\n'];
    test_char '\r';
    test_char '\n';
  ] state input

let space state input =
  peg_alternate [
    test_char ' ';
    test_char '\t';
    end_of_line
  ] state input

let comment state input =
  peg_sequence [
    test_char '#';
    peg_star (peg_sequence [peg_not end_of_line; test_any]);
    end_of_line
  ]state input
    
let spacing state input =
  peg_star (peg_alternate [space; comment]) state input

let begin_token state input =
  peg_sequence [test_char '<'; spacing] state input

let end_token state input =
  peg_sequence [test_char '>';  spacing] state input

let lparen state input =
  peg_sequence [test_char '('; spacing] state input

let rparen state input =
  peg_sequence [test_char ')'; spacing] state input
    
let dot_sign state input =
  peg_sequence [test_char '.'; spacing] state input

let dash_sign state input =
  test_char '-' state input

let quest_sign state input =
  peg_sequence [test_char '?'; spacing] state input

let star_sign state input =
  peg_sequence [test_char '*'; spacing] state input

let plus_sign state input =
  peg_sequence [test_char '+'; spacing] state input

let amp_sign state input =
  peg_sequence [test_char '&'; spacing] state input

let exlmn_sign state input =
  peg_sequence [test_char '!'; spacing] state input

let slash_siqn state input =
  peg_sequence [test_char '/'; spacing] state input

let identStart state input =
  test_f (function
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '_' -> true
    | _ -> false
  ) state input

let identCont state input =
 peg_alternate [
   identStart;
   test_f (function
     | '0' .. '9' -> true
     | _ -> false
   )] state input

let leftarrow state input =
  peg_sequence [test_char '<'; test_char '-'; spacing] state input
    
let identifier state input =
  peg_sequence [
    push_token (peg_sequence [identStart; peg_star identCont])
      (fun lexeme -> Name lexeme);
    spacing
  ] state input

let backslash_sign state input =
  test_char '\\' state input

let digit0_7 state input =
  test_f (fun c -> List.mem c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'])
    state input
    
let char_sign state input =
  peg_alternate [
    peg_sequence [
      backslash_sign;
      push_token (test_f (fun c -> List.mem c
        ['b'; 'n'; 'r'; 't'; ' '; '\''; '"'; '['; ']'; '\\']))
        (fun lexeme ->
          let c =
            match lexeme with
              | "b" -> '\b'
              | "n" -> '\n'
              | "r" -> '\r'
              | "t" -> '\t'
              | "\\" -> '\\'
              | _ -> lexeme.[0]
          in
            Char c
        )
    ];
    peg_sequence [
      backslash_sign;
      push_token (peg_sequence [
        test_f (fun c -> List.mem c ['0'; '1'; '2'; '3']);
        digit0_7;
        digit0_7
      ]) (fun lexeme -> Char (Char.chr (int_of_string ("0o" ^ lexeme))))
    ];
    peg_sequence [
      backslash_sign;
      push_token (peg_sequence [
        digit0_7;
        peg_alternate [digit0_7; peg_stub]
      ]) (fun lexeme -> Char (Char.chr (int_of_string ("0o" ^ lexeme))))
    ];
    peg_sequence [
      backslash_sign;
      dash_sign;
      peg_action (fun state -> Char '-' :: state)
    ];
    peg_sequence [
      peg_not backslash_sign;
      push_token test_any (fun lexeme -> Char lexeme.[0])
    ]
  ] state input

let range state input =
  peg_alternate [
    peg_sequence [
      char_sign;
      dash_sign;
      char_sign;
    ];
    char_sign;
  ] state input
    
let class_char state input =
  peg_sequence [
    test_char '[';
    peg_action (fun state -> Class [] :: state);
    peg_star (peg_sequence [
      peg_not (test_char ']');
      range;
      peg_action (function
        | Char x2 :: Char x1 :: Class rs :: xs -> Class ([x1;x2] :: rs) :: xs
        | Char x1 :: Class rs :: xs -> Class ([x1] :: rs) :: xs
        | _ -> assert false)]);
    test_char ']';
    spacing
  ] state input

let literal state input =
  peg_alternate [
    peg_sequence [
      test_char '\'';
      peg_action (fun state -> Literal [] :: state);
      peg_star (peg_sequence [
        peg_not (test_char '\'');
        char_sign;
        peg_action (function
          | Char x :: Literal cs :: xs -> Literal (x :: cs) :: xs
          | _ -> assert false)]);
      test_char '\'';
      spacing
    ];
    peg_sequence [
      test_char '"';
      peg_action (fun state -> Literal [] :: state);
      peg_star (peg_sequence [
        peg_not (test_char '"');
        char_sign;
        peg_action (function
          | Char x :: Literal cs :: xs -> Literal (x :: cs) :: xs
          | _ -> assert false)]);
      test_char '"';
      spacing
    ]
  ] state input

let action state input =
  peg_sequence [
    test_char '{';
    push_token (peg_star (peg_sequence [peg_not (test_char '}'); test_any]))
      (fun lexeme -> Action lexeme);
    test_char '}';
    spacing;
  ] state input
    
let tokenizer state input =
  peg_sequence [
    push_token (peg_sequence [
      identStart; peg_star (peg_alternate [identCont;
                                           test_char '\'']
      )]) (fun lexeme -> Name lexeme);
    spacing
  ] state input    

let rec primary state input =
  peg_alternate [
    peg_sequence [
      identifier; peg_not leftarrow];
    peg_sequence [lparen; expression; rparen];
    literal;
    class_char;
    peg_sequence [dot_sign; peg_action (fun state -> Any :: state)];
    action;
    peg_sequence [
      begin_token; expression; test_char ':'; spacing; tokenizer; end_token;
      peg_action (function
        | Name x :: expr :: xs -> Tokenizer (expr, x) :: xs
        | _ -> assert false)
    ]
  ] state input
    
and suffix state input =
  peg_sequence [
    primary;
    peg_alternate [
      peg_alternate [
        peg_sequence [
          quest_sign;
          peg_action
            (function
              | x :: xs -> Alternate (x, Epsilon) :: xs
              | _ -> assert false)];
        peg_sequence [
          plus_sign;
          peg_action
            (function
              | x :: xs -> Sequence (x, Star x) :: xs
              | _ ->  assert false)];
        peg_sequence [
          star_sign;
          peg_action
            (function
              | x :: xs -> Star x :: xs
              | _ -> assert false)];
      ];
      peg_stub]
  ] state input

and prefix state input =
  peg_alternate [
    peg_sequence [amp_sign; suffix; peg_action
      (function
        | x::xs -> PredicateNOT (PredicateNOT x) :: xs
        | _ -> assert false)];
    peg_sequence [exlmn_sign; suffix; peg_action
      (function
        | x::xs -> PredicateNOT x :: xs
        | _ -> assert false)];
    suffix
  ] state input
  
and sequence state input =
  peg_sequence [
    prefix;
    peg_star (peg_sequence [
      prefix;
      peg_action
        (function
          | x2 :: x1 :: xs -> Sequence (x1, x2) :: xs
          | _ -> assert false)])
  ] state input
                
and expression state input =
  peg_sequence [
    sequence;
    peg_star (peg_sequence [
      slash_siqn; sequence; peg_action
        (function
          | x2 :: x1 :: xs -> Alternate (x1, x2) :: xs
          | _ -> assert false)])
  ] state input

let definition state input =
  peg_sequence [
    identifier;
    leftarrow;
    expression;
    peg_action (function
      | expr :: (Name name) :: xs -> Rule (name, expr) :: xs
      | _ -> assert false)
  ] state input

let declaration state input =
  peg_alternate [
    peg_sequence [
      test_string ['%';'{'];
      push_token (peg_star (peg_sequence [peg_not (test_string ['%'; '}']);
                                          test_any]))
        (fun lexeme -> Declaration lexeme);
      test_string ['%';'}'];
      spacing
    ];
    peg_stub
  ] state input

let grammar state input =
  peg_sequence [
    spacing;
    declaration;
    definition;
    peg_star definition;
    test end_of_file
  ] state input

let parse_file file =
  let content = read_file file in
  let input = make_input content in
  let state = [] in
  let result = grammar state input in
    match result with
      | Failed ->
        Printf.printf "Remaining: %S\n"
          (String.sub input.buf input.pos (input.len - input.pos));
        failwith "Unparsed"
      | Parsed state ->
        (* List.iter print_token state *)
        state
  

open Camlp4.PreCast;;

module Caml =
  Camlp4.Printers.OCaml.Make
    (Camlp4OCamlParser.Make
       (Camlp4OCamlRevisedParser.Make(Syntax)))

open Syntax
    
let rec make_rule_expr _loc = function
  | Epsilon ->
    <:expr< peg_stub >>
  | Name name ->
    <:expr< $lid:rule_prefix ^ name$ >>

  | Sequence (t1, t2) ->
    let rec flat _loc acc = function
      | Sequence (x1, x2) ->
        flat _loc (<:expr< $make_rule_expr _loc x2$ :: $acc$ >>) x1
      | x ->
        <:expr< $make_rule_expr _loc x$ :: $acc$ >>
    in
    let exprs = flat _loc (<:expr< [$make_rule_expr _loc t2$] >>) t1 in
      <:expr< peg_sequence $exprs$ >>
        
  | Alternate (t1, t2) ->
    let rec flat _loc acc = function
      | Alternate (x1, x2) ->
        flat _loc (<:expr< $make_rule_expr _loc x2$ :: $acc$ >>) x1
      | x ->
        <:expr< $make_rule_expr _loc x$ :: $acc$ >>
    in
    let exprs = flat _loc (<:expr< [$make_rule_expr _loc t2$] >>) t1 in
      <:expr< peg_alternate $exprs$ >>

  | Tokenizer (t, fn) ->
    <:expr< push_token $make_rule_expr _loc t$ $lid:fn$ >>
 
  | Action  code ->
    Printf.printf "Action %S\n" code;
    let e = Caml.AntiquotSyntax.parse_expr _loc code in
      <:expr< peg_action $e$ >>

  | Star t ->
    <:expr< peg_star $make_rule_expr _loc t$ >>

  | PredicateNOT t ->
    <:expr< peg_not $make_rule_expr _loc t$ >>

  | Any ->
    <:expr< test_any >>

  | Literal chars -> (
    match chars with
      | [] -> <:expr< >>
      | [x] -> <:expr< test_char $`chr:x$ >>
      | _ ->
        let exprs = List.map (fun c -> <:expr< $`chr:c$ >>) (List.rev chars) in
          <:expr< test_string [$Ast.exSem_of_list exprs$] >>
  )

  | Class classes ->
    let make_expr = function
      | [x1; x2] ->
        <:expr< c >= $`int:Char.code x1$ && c <= $`int:Char.code x2$ >>
      | [x] -> <:expr< c = $`int:Char.code x$ >>
      | _ -> assert false
    in
    let exprs =
      List.fold_left (fun acc s ->
        <:expr< $make_expr s$ || $acc$ >>
      ) <:expr< $make_expr (List.hd classes)$ >> (List.tl classes) in
      <:expr< test_f (fun c -> let c = Char.code c in $exprs$) >>
      
   | _ -> <:expr< () >>

let () =
    let grammar_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  let rules = parse_file grammar_file in
  let start_rule, declaration =
    match List.rev rules with
      | Declaration str :: Rule (name, _) :: _ -> name, str
      | Rule (name, _) :: _ -> name, ""
      | other ->
        List.iter print_token other;
        assert false in
    
  let _loc = Loc.ghost in
  let bindings = List.fold_right (fun token acc ->
    match token with
      | Rule (name, expr) ->
        <:binding< $lid:rule_prefix ^ name$ state input =
      $make_rule_expr _loc expr$ state input >> :: acc
      | Declaration _ -> acc
      | _ -> assert false
  ) rules [] in

    Caml.print_implem ~output_file
    <:str_item<
      $if declaration <> "" then
       Caml.parse_implem _loc (Stream.of_string declaration)
     else
       <:str_item< >>
      $
      let parse string =
        let rec $Ast.biAnd_of_list bindings$ in
        let input = make_input string in
        let state = [] in
        let result = $lid:rule_prefix ^ start_rule$ state input in
          Printf.printf "Remaining: %S\n"
            (String.sub input.buf input.pos (input.len - input.pos));
          result
          >>
