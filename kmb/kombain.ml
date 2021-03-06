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
        Generate (true, Sys.argv.(2), Sys.argv.(3))
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
            | Parsed ((_,(start, ast)), rest) ->
              Printf.printf "Parsed\n";
              Printf.printf "Remaining input is: %S\n\n"
                (Kmb_input.get_remaining rest);
              let start =
                match start with
                  | Some v -> v.lexeme
                  | None ->
                    let ((name, _), _) = List.hd ast in name
              in
              let newrules = Kmb_util.try_optimize ast in
              let newrules = Kmb_util.remove_unused_rules start newrules in
                List.iter (fun r ->
                  Kmb_pp.pp_rule Format.std_formatter r;
                  Format.print_newline ()
                ) newrules
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
