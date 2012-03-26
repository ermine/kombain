open Kmb_input
open Kmb_lib
open Kmb_grammar
open Printf

let _ = Printexc.record_backtrace true

let () =
  let file = Sys.argv.(1) in
  let input = of_file file in
  let result = Xmlebnf_parser.parse input in
    match result with
      | Failed ->
        printf "failed\n"
      | Parsed (ast, rest) ->
        printf "success\n";

        let out = open_out Sys.argv.(2) in

          List.iter (fun (number, (name, rule)) ->
            fprintf out "# %s\n" number.lexeme;
            fprintf out "%s <- %s\n\n" name.lexeme (string_of_token rule)
          ) ast;
          close_out out
