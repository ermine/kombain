open Kmb_input
open Kmb_lib
open Kmb_grammar

let _ = Printexc.record_backtrace true

let () =
  let file = Sys.argv.(1) in
  let content = read_file file in
  let result = Peg_parser.parse content in
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
          Kmb_generator.generate true dcl ast start_rule Sys.argv.(2)
