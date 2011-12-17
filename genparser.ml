open Peg_input
open Peg_lib
open Peg_grammar

let () =
  let file = Sys.argv.(1) in
  let content = read_file file in
  let result = Peg_parser.parse [] content in
    match result with
      | Failed ->
        Printf.printf "failed";
      | Parsed (state, rest) -> List.iter print_token state;
        Peg_generator.generate state Sys.argv.(2)

