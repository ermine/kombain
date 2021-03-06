open Html5.M
open Html5.P
open Markdown_lib


let parse s =
  let result = Markdown_parser.parse s in
    match result with
      | Kmb_lib.Failed -> failwith "Failed to parse"
      | Kmb_lib.Parsed (ast, rest) -> ast

let collect_refs ast =
  List.fold_left (fun acc -> function
    | Reference (l, Src (s, t)) -> (l, (s, t)) :: acc
    | _ -> acc
  ) [] ast

let make_attrs attrs =
  List.map (fun (aname, avalue) ->
    Xml.string_attrib aname (match avalue with
      | Some v -> v
      | None -> ""))
    attrs

let rec render els =
  let r =
    List.fold_right (fun el acc ->
      match el with
        | Text s -> s :: acc
        | Strong xs -> render xs :: acc
        | Emph xs -> render xs :: acc
        | LineBreak -> acc;
        | Space -> " " :: acc
        | Link _ | Image _ -> acc
        | _ -> acc
    ) els [] in
    String.concat " " r

let lookup r refs =
  try Some (List.assoc r refs) with Not_found -> None


let make_image (l, (u, t)) =
  match t with
    | None -> img ~src:u ~alt:(render l) ()
    | Some t -> img ~src:u ~alt:(render l) ~a:[a_title t] ()
    
let rec link_inline refs els =
  List.fold_right (fun el acc ->
    match el with
      | Text s -> pcdata s :: acc
      | Entity s -> pcdata ("&" ^ s ^ ";") :: acc
      | Space -> pcdata " " :: acc
      | LineBreak -> br () :: acc
      | Emph xs -> em (link_inline refs xs) :: acc
      | Strong xs -> strong (link_inline refs xs) :: acc
      | Image (l, Src (u,t)) -> make_image (l,(u,t)) :: acc
      | Image (l, Nothing) -> (
        match lookup l refs with
          | Some (u, t) -> make_image (l, (u,t)) :: acc
          | None -> link_inline refs (Text "![" :: l @ [Text "]"]) @ acc
      )
      | Image (l, Ref (r, s)) -> (
        let r' = if r = [] then l else r in
          match lookup r' refs with
            | Some (u, t) -> make_image (l, (u, t)) :: acc
            | None -> link_inline refs (Text "![" :: l @
                                          (Text "]" :: Text s ::
                                             Text "[" :: r @ [Text "]"]))
              @ acc
      )
      | Code s -> print_endline "code in link"; acc
      | HtmlComment s -> tot (Xml.comment s) :: acc
      | Html (name, attrs, xs) ->
        tot (Xml.node ~a:(make_attrs attrs) name
               (toeltl (link_inline refs xs))) :: acc
      | Link (l, Nothing) -> link_inline refs (Text "[" :: l @ [Text "]"]) @ acc
      | Link (l, _) -> print_endline ("link in link " ^ render l) ; acc
  ) els []


and make_link refs (l, (u, t)) =
  match t with
    | None ->
      a ~a:[a_href u] (link_inline refs l)
    | Some t ->
      a ~a:[a_href u; a_title t] (link_inline refs l)

let rec html_of_inline refs els =
  List.fold_right (fun el acc ->
    match el with
      | Text s -> pcdata s :: acc
      | Entity s -> pcdata ("&" ^ s ^ ";") :: acc
      | Space -> pcdata " " :: acc
      | LineBreak -> br () :: acc
      | Code s -> code [pcdata s] :: acc
      | Emph xs -> em (html_of_inline refs xs) :: acc
      | Strong xs -> strong (html_of_inline refs xs) :: acc
      | HtmlComment s -> tot (Xml.comment s) :: acc
      | Html (name, attrs, xs) ->
        tot (Xml.node ~a:(make_attrs attrs) name
               (toeltl (html_of_inline refs xs))) :: acc
      | Image (l, Src (u,t)) ->
        make_image (l, (u,t)) :: acc
      | Image (l, Nothing) -> (
        match lookup l refs with
          | Some (u, t) -> make_image (l, (u,t)) :: acc
          | None -> html_of_inline refs (Text "![" :: l @ [Text "]"]) @ acc
      )
      | Image (l, Ref (r, s)) -> (
        let r' = if r = [] then l else r in
          match lookup r' refs with
            | Some (u, t) -> make_image (l, (u, t)) :: acc
            | None -> html_of_inline refs (Text "![" :: l @
                                             (Text "]" :: Text s ::
                                                Text "[" :: r @ [Text "]"]))
              @ acc
      )
      | Link (l, Src (u, t)) ->
        make_link refs (l, (u,t)) :: acc
      | Link (l, Nothing) -> (
        match lookup l refs with
          | Some (u, t) -> make_link refs (l, (u, t)) :: acc
          | None ->
            List.concat [[pcdata "["]; html_of_inline refs l;
                         (pcdata "]" :: acc)]
      )
      | Link (l, Ref (r, s)) -> (
        let r' = if r = [] then l else r in
          match lookup r' refs with
            | Some (u, t) -> make_link refs (l, (u, t)) :: acc
            | None ->
              List.concat [[pcdata "["]; html_of_inline refs l; [pcdata "]"];
                           [pcdata s]; [pcdata "["];
                           html_of_inline refs r; [pcdata "]"]; acc]
      )
  ) els []

let strip els =
  let rec drop_while = function
    | [] -> []
    | Space :: xs -> drop_while xs
    | xs -> xs
  in
    drop_while (List.rev (drop_while (List.rev els)))

let rec html_of_block refs els =
  List.fold_right (fun el acc ->
    match el with
      | HtmlBlock (name, attrs, els) -> (
        if name = "td" then
          match els with
            | [Plain inlines] ->
              tot (Xml.node ~a:(make_attrs attrs) "td"
                     (toeltl (html_of_inline refs (strip inlines)))) :: acc
            | _ ->
              tot (Xml.node ~a:(make_attrs attrs) "td"
                     (toeltl (html_of_block refs els))) :: acc
        else 
          tot (Xml.node ~a:(make_attrs attrs) name
                 (toeltl (html_of_block refs els))) :: acc
      )
      | Plain xs
      | Para xs ->
        p (html_of_inline refs (strip xs)) :: acc
                
      | Heading (lev, xs) -> (
        let xs = strip xs in
          match lev with
            | 1 -> h1 (html_of_inline refs xs) :: acc
            | 2 -> h2 (html_of_inline refs xs) :: acc
            | 3 -> h3 (html_of_inline refs xs) :: acc
            | 4 -> h4 (html_of_inline refs xs) :: acc
            | 5 -> h5 (html_of_inline refs xs) :: acc
            | 6 -> h6 (html_of_inline refs xs) :: acc
            | _ -> assert false
      )
      | HorizontalRule -> hr () :: acc
      | BlockQuote str ->
        let ast = parse (Kmb_input.make_input str) in
        let newrefs = collect_refs ast in
          blockquote (html_of_block (newrefs @ refs) ast) :: acc
      | CodeHighlight (name, lines) ->
        pre [code ~a:(match name with
          | Some name -> [a_class [name]]
          | None -> [])
                [pcdata lines]] :: acc
      | Verbatim s ->
        pre [code  [pcdata s]] :: acc
      | BulletList items ->
        ul (List.map (fun i ->
          match i with
            | [Plain _] as blocks
              (* TODO: type conflict
                 li (html_of_inline refs (strip p))
              *)
            | blocks -> li (html_of_block refs blocks)
        ) items) :: acc
      | OrderedList items ->
        ol (List.map (fun i -> li  (html_of_block refs i)) items) :: acc
      | Reference _ -> acc
  ) els []
    
let get_topic filename ast =
  try
    let t = List.find (function
      | Heading (1, _) -> true
      | _ -> false
    ) ast in
      match t with
        | Heading (_, inlines) -> render (strip inlines)
        | _ -> Filename.basename filename
  with Not_found -> Filename.basename filename

let make_html5 filename ast =
  let refs = collect_refs ast in
  let topic = get_topic filename ast in
    html (head (title (pcdata topic)) [
      script ~a:[a_src "http://yandex.st/highlightjs/6.1/highlight.min.js"]
        (pcdata "")
    ])
      (body (html_of_block refs ast))

let () =
  let mdfile = Sys.argv.(1) in
  let content = Kmb_input.read_file mdfile in
  let ast = parse (Kmb_input.make_input ~filename:mdfile content) in
  let outfile = Sys.argv.(2) in
  let oc = open_out outfile in
  let output str = output oc str 0 (String.length str) in
    print ~output  (make_html5 mdfile ast);
    close_out oc
  
