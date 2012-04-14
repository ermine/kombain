open Kmb_input
open Kmb_lib
open Kmb_grammar
open Printf

let _ = Printexc.record_backtrace true

let () =
  let file = Sys.argv.(1) in
  let input = of_file file in
  let result = Xmlebnf_parser.parse input in
    match result with
      | Failed ->
        printf "failed\n"
      | Parsed (ast, rest) ->
        printf "success\n";

        let out = open_out Sys.argv.(2) in
        let ppf = Format.formatter_of_out_channel out in

          List.iter (fun (number, (name, rule)) ->
            Format.fprintf ppf "@[<v 0># %s@," number.lexeme;
            Kmb_pp.pp_rule ppf ((name.lexeme, []), rule);
            Format.fprintf ppf "@."
          ) ast;
          close_out out
