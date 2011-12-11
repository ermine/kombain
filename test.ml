open Peg_lib

let () =
  let file = Sys.argv.(1) in
  let content = read_file file in
  let result = Peg_grammar.parse content in
    match result with
      | Failed ->
        Printf.printf "failed"
      | Parsed state -> List.iter print_token state
