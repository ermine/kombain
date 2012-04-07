open Kmb_input
open Kmb_lib
open Kmb_grammar

type work =
  | Check of string
  | Generate of bool * string * string
  | Usage
  | Dump of string

let () =
  let arg_len = Array.length Sys.argv in
  let work =
    if arg_len > 2 then
      if Sys.argv.(1) = "--check" then
        Check Sys.argv.(2)
      else if Sys.argv.(1) = "--dump" then
        Dump Sys.argv.(2)
      else if (Sys.argv.(1) = "--verbose" || Sys.argv.(1) = "-V") &&
          arg_len > 3 then
        Generate (true, Sys.argv.(2), Sys.argv.(2))
      else
        Generate (false, Sys.argv.(1), Sys.argv.(2))
      else
        Usage
  in
    match work with
      | Usage ->
        Printf.printf "Usage: %s [options]" (Filename.basename Sys.argv.(0));
        Printf.printf "\n";
        Printf.printf "Three modes:\n";
        Printf.printf "--check file.peg     Check syntax of file.peg\n";
        Printf.printf "--dump file.peg      Dump PEG grammar with some optimizations\n";
        Printf.printf "[-V|--verbose] file.peg file.ml    Generate a parser from file.peg, with or without verbose debug\n"
      | Check file -> (
        let input = Kmb_input.of_file file in
        let result = Peg_parser.parse input in
          match result with
            | Failed ->
              Printf.printf "failed";
            | Parsed (_, rest) ->
              Printf.printf "Parsed\n";
              Printf.printf "Remaining input is: %S\n"
                (Kmb_input.get_remaining rest)
      )
        
      | Dump file -> (
        let input = Kmb_input.of_file file in
        let result = Peg_parser.parse input in
          match result with
            | Failed ->
              Printf.printf "failed";
            | Parsed ((_,(_, ast)), rest) ->
              Printf.printf "Parsed\n";
              Printf.printf "Remaining input is: %S\n\n"
                (Kmb_input.get_remaining rest);
              let newrules = Kmb_util.try_optimize ast in
                List.iter (fun r ->
                  Printf.printf "%s\n" (string_of_rule r)) newrules
      )
        
      | Generate (verbose, pegfile, mlfile) ->
        let input = Kmb_input.of_file pegfile in
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
                Kmb_generator.generate verbose dcl ast start_rule mlfile
