open Kmb_input
open Kmb_lib
open Kmb_grammar
open Printf

let _ = Printexc.record_backtrace true

let () =
  let file = Sys.argv.(1) in
  let input = Kmb_input.of_file file in
  let result = Xml_parser.parse input in
    match result with
      | Failed ->
        printf "failed\n"
      | Parsed (ast, rest) ->        
        printf "success\n";
        printf "Remaining input is: %S\n" (Kmb_input.get_remaining rest)
        
