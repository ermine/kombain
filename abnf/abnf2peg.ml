open Kmb_input
open Kmb_lib
open Kmb_grammar
open Printf

let _ = Printexc.record_backtrace true

let () =
  let file = Sys.argv.(1) in
  let content = read_file file in
  let result = Abnf_parser.parse content in
    match result with
      | Failed ->
        printf "failed\n"
      | Parsed (ast, rest) ->
        printf "success\n";
        List.iter (fun (name, (defined_as, rule)) ->
          Printf.printf "%s <- %s\n\n"
            (Abnf_lib.convert_name name.Kmb_lib.lexeme)
            (Kmb_grammar.string_of_token (Abnf_lib.make_peg rule))
        ) ast
