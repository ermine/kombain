open Kmb_input
open Kmb_lib
open Kmb_grammar
open Yamlspec_types
open Printf

let _ = Printexc.record_backtrace true

let () =
  let file = Sys.argv.(1) in
  let content = read_file file in
  let result = Yamlspec_parser.parse content in
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
 

          List.fold_left (fun count (number, rule) ->
            let num = int_of_string number.lexeme in
              if num <> count + 1 then
                printf "Previous number: %d, current: %d\n" count num;
              fprintf out "# [%d]\n" num;
              fprintf out "%s\n\n" (string_of_rule rule);
              num
          ) 0 ast;
          close_out out
