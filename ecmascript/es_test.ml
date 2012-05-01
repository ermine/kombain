
let () =
  let file = Sys.argv.(1) in
  let input = Kmb_input.of_file file in
  let result = Ecmascript_parser.parse input in
    match result with
      | Kmb_lib.Failed ->
        Printf.printf "failed"
      | Kmb_lib.Parsed (ast, rest) ->
        Printf.printf "Parsed\n";
        Printf.printf "Remaining input is: %S\n"
          (Kmb_input.get_remaining rest)
        (* List.iter print_token ast *)
     
