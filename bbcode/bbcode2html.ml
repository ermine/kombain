open Bbcode_parser

module H = Html5.M

module Bbcode = Bbcode2html_converter.M(Xml)(Svg.M)(H)

let () =
  let input = Kmb_input.of_file Sys.argv.(1) in
  let result = Bbcode_parser.parse input in
    match result with
      | Kmb_lib.Failed ->
        failwith "failed to parse bbcode"
      | Kmb_lib.Parsed (ast, rest) ->
        Printf.printf "Parsed, remaining %S\n" (Kmb_input.get_remaining rest);
        let outfile = Sys.argv.(2) in
        let oc = open_out outfile in
        let html5 = Bbcode.make_html5 ast in
        let page =
          H.html (H.head (H.title (H.pcdata "abc")) [])
            (H.body html5) in
        let output str = output oc str 0 (String.length str) in
          Html5.P.print ~output page;
          close_out oc
          
