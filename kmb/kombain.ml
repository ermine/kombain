open Kmb_input
open Kmb_lib
open Kmb_grammar

let () =
  let verbose =
    if Array.length Sys.argv > 2 &&
      (Sys.argv.(1) = "-v" || Sys.argv.(1) = "--verbose") then true else false in
  let file = Sys.argv.(if verbose then 2 else 1) in
  let outfile = Sys.argv.(if verbose then 3 else 2) in
  let input = Kmb_input.of_file file in
  let result = Peg_parser.parse input in
    match result with
      | Failed ->
        Printf.printf "failed";
      | Parsed ((dcl, (start, ast)), rest) ->
        let start_rule =
          match start with
            | Some name -> name.lexeme
            | None ->
              match ast with
                | [] -> assert false
                | ((name, _), _) :: _ -> name
        in
          Kmb_generator.generate verbose dcl ast start_rule outfile
