open Kmb_input
open Kmb_lib
open Kmb_grammar
open Yamlspec_types
open Printf

let _ = Printexc.record_backtrace true

let () =
  let file = Sys.argv.(1) in
  let input = of_file file in
  let result = Yamlspec_parser.parse input in
    match result with
      | Failed ->
        printf "failed\n"
      | Parsed (ast, rest) ->
        printf "success\n";

        let out = open_out Sys.argv.(2) in
          fprintf out "# This PEG file was generated from yaml.bnf\n";
          fprintf out "%%{\n";
          fprintf out "open Yamlspec_types\n";
          fprintf out "%%}\n\n";
          fprintf out "%%start l_yaml_stream\n";
 
          List.iter (fun (number, rule) ->
            let num = int_of_string number.lexeme in
              fprintf out "# [%d]\n" num;
              fprintf out "%s\n\n" (string_of_rule (cleanup_rule rule));
          ) ast;
          close_out out
