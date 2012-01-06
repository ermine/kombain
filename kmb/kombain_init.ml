open Kmb_input
open Kmb_lib
open Kmb_grammar
  
let _ = Printexc.record_backtrace true
      
let end_of_line input =
  alt
    (seq (test_char '\r') (test_char '\n'))
    (alt
       (test_char '\r')
       (test_char '\n'))
    input

let space input =
  alt 
    (test_char ' ')
    (alt
       (test_char '\t')
       end_of_line
    ) input

let comment input =
  seq
    (test_char '#')
    (seq
       (star (seq (predicate_not end_of_line) (test_any)))
       end_of_line
    ) input
    
let spacing input =
  star (alt space comment) input

let lparen input =
  seq (test_char '(') spacing input

let rparen input =
  seq (test_char ')') spacing input
    
let dot_sign input =
  seq (test_char '.') spacing input

let dash_sign input =
  test_char '-' input

let at_sign input =
  seq (test_char '@') spacing input

let quest_sign input =
  seq (test_char '?') spacing input

let star_sign input =
  seq (test_char '*') spacing input

let plus_sign input =
  seq (test_char '+') spacing input

let amp_sign input =
  seq (test_char '&') spacing input

let exlmn_sign input =
  seq (test_char '!') spacing input

let slash_siqn input =
  seq (test_char '/') spacing input

let identStart input =
  test_f (function
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '_' -> true
    | _ -> false
  ) input

let identCont input =
  alt
    identStart
    (test_f (function
      | '0' .. '9' | '\'' -> true
      | _ -> false
     )) input

let leftarrow input =
  Printf.printf "leftarrow";
  seq (test_string ['<'; '-']) spacing input

let leftangle input =
  seq (test_char '<') spacing input

let rightangle input =
  seq (test_char '>') spacing input
    
let def_identifier input =
  seq_l
    (get_lexeme (seq identStart (star identCont)))
    spacing
    input

let backslash_sign input =
  test_char '\\' input

let digit0_7 input =
  test_f (fun c -> List.mem c ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7']) input
    
let char_sign input =
  alt
    (transform
       (seq_r
          backslash_sign
          (get_lexeme (test_f (fun c -> List.mem c
            ['b'; 'n'; 'r'; 't'; ' '; '\''; '"'; '['; ']'; '\\']))))
       make_escaped_char)
    (alt
       (transform
          (seq
             backslash_sign
             (get_lexeme (seq 
                            (test_f (fun c -> List.mem c ['0'; '1'; '2'; '3']))
                            (seq digit0_7 digit0_7))))
          make_dec_char)
       (alt
          (transform
             (seq
                backslash_sign
                (get_lexeme (seq 
                               digit0_7
                               (alt digit0_7 peg_stub))))
             make_dec_char)
          (alt
             (transform
                (seq_n
                   backslash_sign
                   (test_char '-'))
                (fun () -> '-'))
             (transform
                (seq_r
                   (predicate_not backslash_sign)
                   (get_lexeme test_any))
                make_char)
          )
       )
    ) input

let range input =
  alt 
    (fun input ->
      match char_sign input with
        | Parsed (c1, input) ->
          seq
            dash_sign
            (fun input ->
              match char_sign input with
                | Parsed (c2, input) ->
                  Parsed (Range (c1, c2), input)
                | Failed as failed -> failed
            ) input
        | Failed as failed -> failed
    )
    (fun input ->
      match char_sign input with
        | Parsed (c, input) -> Parsed (Char c, input)
        | Failed as failed -> failed
    )
    input
    
let class_char input =
  transform
    (seq_r
       (test_char '[')
       (seq_l (star_accu (seq_r
                            (predicate_not (test_char ']'))
                            range))
          (seq_n (test_char ']') spacing)
       ))
    make_class
    input

let def_literal input =
  transform
    (alt
       (seq_r
          (test_char '\'')
          (seq_l
             (star_accu (seq (predicate_not (test_char '\'')) char_sign))
             (seq_n (test_char '\'') spacing)))
       (seq_r
          (test_char '"')
          (seq_l
             (star_accu (seq_r (predicate_not (test_char '"')) char_sign))
             (seq_n (test_char '"') spacing))))
    make_literal
    input

let rec def_action input =
  seq (test_char '{')
    (seq_l (get_lexeme
              (star (alt action'
                       (seq_n (predicate_not (test_char '}')) test_any))))
       (seq_n (test_char '}') spacing))
    input
and action' input =
  seq_n (test_char '{')
    (seq_n (star (alt action' 
                    (seq_n (predicate_not (test_char '}')) test_any)))
       (seq_n (test_char '}') spacing))
    input
    
    
let rec def_primary input =
  alt
    (transform (seq_l def_identifier (predicate_not leftarrow)) make_name)
    (alt
       (seq_r lparen (seq_l def_expression rparen))
       (alt
          def_literal
          (alt
             class_char
             (alt
                (seq dot_sign (return Any))
                (seq 
                   leftangle
                   (fun input ->
                     match def_expression input with
                       | Parsed (r, input) ->
                         seq
                           rightangle
                           (return (Tokenizer r))
                           input
                       | Failed as failed -> failed
                   )
                )
             )
          )
       )
    ) input
    
and def_suffix input =
  transform
    (seq_b def_primary
       (opt_accu
          (alt (transform quest_sign (fun _  r -> Opt r))
             (alt (transform plus_sign (fun _ r -> Plus r))
                (transform star_sign (fun _ r -> Star r))
             ))))
    (fun (r, f) -> match f with
      | None -> r
      | Some f -> f r)
    input

and def_prefix input =
  transform
    (seq_b
       (opt_accu
          (alt
             (transform amp_sign make_predicate_and)
             (transform exlmn_sign make_predicate_not)
          ))
       def_suffix)
    make_prefix
    input
  
and def_tokenizer input =
  transform (seq_r leftangle (seq_l def_suffix rightangle))
    (fun expr -> Tokenizer expr)
    input

and def_pattern input =
  transform (seq_b def_identifier (seq_r at_sign def_suffix))
    make_pattern
    input
  
and def_item input =
  alt def_pattern
    def_prefix
    input
  
and def_sequence input =
  transform
    (seq_b (star_accu def_item)
       (opt_accu def_action))
    make_sequence
    input
    
                
and def_expression input =
  transform
    (seq_b
       def_sequence
       (star_accu (seq_r slash_siqn def_sequence)))
    make_alternates
    input

          
let def_definition input =
  seq_b
    (transform def_identifier (fun {lexeme} -> lexeme))
    (seq_r leftarrow def_expression) input

let dcl_start input =
  seq (test_string ['%';'{']) spacing input

let dcl_end input =
  seq (test_string ['%';'}']) spacing input

let def_declaration input =
  opt_accu
    (transform
       (seq dcl_start
          (seq_l
             (get_lexeme (star (seq
                                  (predicate_not (test_string ['%'; '}']))
                                  (test_any))))
             dcl_end
          ))
       make_declaration)
    input

let grammar input =
  seq_r
    spacing
    (seq_b def_declaration
       (seq_l
          (star_accu def_definition)
          (predicate_not test_any))
    ) input
    

             

let parse_file file =
  let content = read_file file in
  let input = make_input content in
  let result = grammar input in
    match result with
      | Failed -> failwith "Unparsed"
      | Parsed ((dcl, rules), input) -> (dcl, rules), input
  

let _ =
  let grammar_file = Sys.argv.(1) in
  let output_file = Sys.argv.(2) in
  let (declaration, rules), rest = parse_file grammar_file in
    List.iter (fun (name, rule) ->
      Printf.printf "Rule %s " name; print_token rule) rules;
    Kmb_generator.generate false declaration rules output_file
