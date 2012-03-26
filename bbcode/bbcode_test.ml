
let () =
  let file = Sys.argv.(1) in
  let input = Kmb_input.of_file file in
  let result = Bbcode_parser.parse input in
    match result with
      | Kmb_lib.Failed ->
        Printf.printf "failed"
      | Kmb_lib.Parsed (ast, rest) ->
        Printf.printf "Parsed\n";
        (* List.iter print_token ast *)
        
