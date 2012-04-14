open Kmb_input
open Kmb_lib
open Kmb_grammar
open Printf

let _ = Printexc.record_backtrace true

let () =
  let file = Sys.argv.(1) in
  let input = of_file file in
  let result = Abnf_parser.parse input in
    match result with
      | Failed ->
        printf "failed\n"
      | Parsed (ast, rest) ->
        printf "success\n";
        List.iter (fun (name, (defined_as, rule)) ->
          Kmb_pp.pp_rule Format.std_formatter 
            ((Abnf_lib.convert_name name.Kmb_input.lexeme, []),
             Abnf_lib.make_peg rule)
        ) ast
