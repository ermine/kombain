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

          List.fold_left (fun count (number, rule) ->
            let num = int_of_string number.lexeme in
              if num <> count + 1 then
                printf "Previous number: %d, current: %d\n" count num;
              fprintf out "# [%d]\n" num;
              match rule with
                | Rule (name, expr) ->
                  fprintf out "%s <- %s\n\n" name
                    (string_of_token expr);
                  num
                | RuleFun _ ->
                  num
          ) 0 ast;
          close_out out
