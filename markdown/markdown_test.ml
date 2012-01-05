open Markdown_lib

let () =
  let file = Sys.argv.(1) in
  let content = Peg_input.read_file file in
  let result = Markdown_parser.parse content in
    match result with
      | Peg_lib.Failed ->
        Printf.printf "failed"
      | Peg_lib.Parsed (ast, rest) ->
        Printf.printf "Parsed\n";
        List.iter print_token ast
        
