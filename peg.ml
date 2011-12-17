open Peg_input
open Peg_lib
open Peg_grammar
  
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
    get_lexeme (peg_sequence [identStart; peg_star identCont]) make_name;
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
      get_lexeme (test_f (fun c -> List.mem c
        ['b'; 'n'; 'r'; 't'; ' '; '\''; '"'; '['; ']'; '\\'])) make_escaped_char
    ];
    peg_sequence [
      backslash_sign;
      get_lexeme (peg_sequence [
        test_f (fun c -> List.mem c ['0'; '1'; '2'; '3']);
        digit0_7;
        digit0_7
      ]) make_octet_char
    ];
    peg_sequence [
      backslash_sign;
      get_lexeme (peg_sequence [
        digit0_7;
        peg_alternate [digit0_7; peg_stub]
      ]) make_octet_char
    ];
    peg_sequence [
      backslash_sign;
      dash_sign;
      peg_action (fun state _ -> Char '-' :: state)
    ];
    peg_sequence [
      peg_not backslash_sign;
      get_lexeme test_any make_char
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
    peg_action (fun state _ -> Class [] :: state);
    peg_star (peg_sequence [
      peg_not (test_char ']');
      range;
      peg_action (fun state _ ->
        match state with
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
      peg_action (fun state _ -> Literal [] :: state);
      peg_star (peg_sequence [
        peg_not (test_char '\'');
        char_sign;
        peg_action (fun state _ ->
          match state with
            | Char x :: Literal cs :: xs -> Literal (x :: cs) :: xs
            | _ -> assert false)]);
      test_char '\'';
      spacing
    ];
    peg_sequence [
      test_char '"';
      peg_action (fun state _ -> Literal [] :: state);
      peg_star (peg_sequence [
        peg_not (test_char '"');
        char_sign;
        peg_action (fun state _ ->
          match state with
            | Char x :: Literal cs :: xs -> Literal (x :: cs) :: xs
            | _ -> assert false)]);
      test_char '"';
      spacing
    ]
  ] state input

let action state input =
  peg_sequence [
    test_char '{';
    get_lexeme (peg_star (peg_sequence [peg_not (test_char '}'); test_any]))
      make_action;
    test_char '}';
    spacing;
  ] state input
    
let tokenizer state input =
  peg_sequence [
    get_lexeme (peg_sequence [
      identStart; peg_star (peg_alternate [identCont;
                                           test_char '\'']
      )]) make_name;
    spacing
  ] state input    

let rec primary state input =
  peg_alternate [
    peg_sequence [
      identifier; peg_not leftarrow];
    peg_sequence [lparen; expression; rparen];
    literal;
    class_char;
    peg_sequence [dot_sign; peg_action (fun state _ -> Any :: state)];
    action;
    peg_sequence [
      begin_token; expression; test_char ':'; spacing; tokenizer; end_token;
      peg_action (fun state _ ->
        match state with
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
          peg_action (fun state _ ->
            match state with
              | x :: xs -> Alternate (x, Epsilon) :: xs
              | _ -> assert false)];
        peg_sequence [
          plus_sign;
          peg_action (fun state _ ->
            match state with
              | x :: xs -> Sequence (x, Star x) :: xs
              | _ ->  assert false)];
        peg_sequence [
          star_sign;
          peg_action (fun state _ ->
            match state with
              | x :: xs -> Star x :: xs
              | _ -> assert false)];
      ];
      peg_stub]
  ] state input

and prefix state input =
  peg_alternate [
    peg_sequence [amp_sign; suffix;
                  peg_action (fun state _ ->
                    match state with
                      | x::xs -> PredicateNOT (PredicateNOT x) :: xs
                      | _ -> assert false)];
    peg_sequence [exlmn_sign; suffix;
                  peg_action (fun state _ ->
                    match state with
                      | x::xs -> PredicateNOT x :: xs
                      | _ -> assert false)];
    suffix
  ] state input
  
and sequence state input =
  peg_sequence [
    prefix;
    peg_star (peg_sequence [
      prefix;
      peg_action (fun state _ ->
        match state with
          | x2 :: x1 :: xs -> Sequence (x1, x2) :: xs
          | _ -> assert false)])
  ] state input
                
and expression state input =
  peg_sequence [
    sequence;
    peg_star (peg_sequence [
      slash_siqn; sequence; peg_action (fun state _ ->
        match state with
          | x2 :: x1 :: xs -> Alternate (x1, x2) :: xs
          | _ -> assert false)])
  ] state input

let definition state input =
  peg_sequence [
    identifier;
    leftarrow;
    expression;
    peg_action (fun state _ ->
      match state with
        | expr :: (Name name) :: xs -> Rule (name, expr) :: xs
        | _ -> assert false)
  ] state input

let declaration state input =
  peg_alternate [
    peg_sequence [
      test_string ['%';'{'];
      get_lexeme (peg_star (peg_sequence [peg_not (test_string ['%'; '}']);
                                          test_any]))
        make_declaration;
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
    peg_not test_any
  ] state input

let parse_file state file =
  let content = read_file file in
  let input = make_input content in
  let result = grammar state input in
    match result with
      | Failed -> failwith "Unparsed"
      | Parsed (state, input) -> (state, input)
  

let _ =
  let grammar_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  let rules, rest = parse_file [] grammar_file in
    Peg_generator.generate rules output_file
