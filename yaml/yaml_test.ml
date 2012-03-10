open Kmb_input
open Kmb_lib
open Kmb_grammar
open Printf

open Yamlspec_types


let off n = String.make n ' '

let print_data (directives, doc) =
  let rec print_seq_entry n = function
    | Block d ->
      printf "[\n";
      List.iter (print_seq_entry (succ n)) d;
      printf "%s]" (off n);
    | Node node ->
      print_node n node
    | Pair (node1, node2) ->
      print_seq_entry n node1;
      printf " : ";
      print_seq_entry n node2;
      printf "\n";
  and print_node n = function
    | Alias str ->
      printf "%sAlias %s\n" (off n) str
    | Content content ->
      print_content n content;
    | Properties (p,c) ->
      print_properties n p;
      print_content n c
  and print_properties  n = function
    | TagProperty (tag_property, str) -> ()
    | AnchorProperty (str, tag_property) -> ()
  and print_content n = function
    | Scalar str ->
      printf "%s%S" (off n) str
    | Seq entries ->
      printf "%s(\n" (off n);
      List.iter (print_seq_entry (succ n)) entries;
      printf "%s)\n" (off n)
  in
    print_seq_entry 0 doc


let () =
  let file = Sys.argv.(1) in
  let content = read_file file in
  let result = Yaml_parser.parse content in
    match result with
      | Failed ->
        printf "failed\n"
      | Parsed (ast, rest) ->
        printf "success\n";
        List.iter (function
          | None -> printf "no data\n"
          | Some data -> print_data data
        ) ast
