open Bbcode_parser

module M (Xml : Xml_sigs.Iterable)
  (Svg : Svg_sigs.T with type Xml.uri = Xml.uri
                    and type Xml.event_handler = Xml.event_handler
                    and type Xml.attrib = Xml.attrib
                    and type Xml.elt = Xml.elt
                    and type uri = Xml.uri)
  (H : Html5_sigs.T with type Xml.uri = Xml.uri
                    and type Xml.event_handler = Xml.event_handler
                    and type Xml.attrib = Xml.attrib
                    and type Xml.elt = Xml.elt
                    and module Svg := Svg) =
struct
  open H

  let rec make_phrasing is =
    List.fold_right (fun t acc ->
      match t with
        | Abbr (is, value) -> abbr ~a:[a_title value] (make_phrasing is) :: acc
        | Strong is -> strong (make_phrasing is) :: acc
        | Em is -> em (make_phrasing is) :: acc
        | Strike is ->
          span ~a:[a_style "text-decoration:line-through"]
            (make_phrasing is) :: acc
        | Subscript is -> sub (make_phrasing is) :: acc
        | Superscript is -> sup (make_phrasing is) :: acc
        | Underline is ->
          span ~a:[a_style "text-decoration:underline"] (make_phrasing is) :: acc
        | Color (value, is) ->
          span ~a:[a_style ("color:"^value)] (make_phrasing is) :: acc
        | Font (value, is) ->
          span ~a:[a_style ("font-family:"^value)] (make_phrasing is) :: acc
        | Sp -> space () :: acc
        | Code str -> (code [pcdata str]) :: acc
        | Image data ->
          let a = if data.align <> "" then
              [a_style ("float:" ^ data.align)]
            else
              []
          in
          let a =
            match data.dim with
              | None -> a
              | Some (width, height) -> a_width width :: a_height height :: a
          in
            img ~a ~src:(uri_of_string data.src)
              ~alt:(if data.alt = "" then "[IMG]" else data.alt)
              () :: acc
        | Break -> br () :: acc
        | Size (value, is) ->
          let size = if value < 6 then 6 else if value > 48 then 48 else value in
            span ~a:[a_style ("font-size:"^string_of_int size ^ "px")]
              (make_phrasing is)  :: acc
        | Anchor (anchor, is) ->
          span ~a:[a_id anchor] (make_phrasing is) :: acc
        | Entity e -> entity e :: acc
        | Str str ->
          pcdata str :: acc
        | Float (align, is) ->
          span ~a:[a_style ("float:" ^ align)] (make_phrasing is) :: acc
        | NewLine -> acc
        | _ -> assert false
    ) is []
      
      
  let rec inline_to_html is =
    List.fold_right (fun t acc ->
      match t with
        | Abbr (is, value) -> abbr ~a:[a_title value] (inline_to_html is) :: acc
        | Strong is -> strong (inline_to_html is) :: acc
        | Em is -> em (inline_to_html is) :: acc
        | Strike is ->
          span ~a:[a_style "text-decoration:line-through"]
            (inline_to_html is) :: acc
        | Subscript is -> sub (inline_to_html is) :: acc
        | Superscript is -> sup (inline_to_html is) :: acc
        | Underline is ->
          span ~a:[a_style "text-decoration:underline"]
            (inline_to_html is) :: acc
        | Color (value, is) ->
          span ~a:[a_style ("color:"^value)] (inline_to_html is) :: acc
        | Font (value, is) ->
          span ~a:[a_style ("font-family:"^value)] (inline_to_html is) :: acc
        | Sp -> space () :: acc
        | Code str -> (code [pcdata str]) :: acc
        | Image data ->
          let a = if data.align <> "" then
              [a_style ("float:" ^ data.align)]
            else
              []
          in
          let a =
            match data.dim with
              | None -> a
              | Some (width, height) -> a_width width :: a_height height :: a
          in
            img ~a ~src:(uri_of_string data.src)
              ~alt:(if data.alt = "" then "[IMG]" else data.alt)
              () :: acc
        | Break -> br () :: acc
        | Size (value, is) ->
          let size = if value < 6 then 6 else if value > 48 then 48 else value in
            span ~a:[a_style ("font-size:"^string_of_int size ^ "px")]
              (inline_to_html is)  :: acc
        | Anchor (anchor, is) ->
          span ~a:[a_id anchor] (inline_to_html is) :: acc
        | Entity e -> entity e :: acc
        | Str str ->
          pcdata str :: acc
        | Float (align, is) ->
          span ~a:[a_style ("float:" ^ align)] (inline_to_html is) :: acc
        | NewLine -> acc
        | Email (email, name) ->
          (a ~a:[a_href (uri_of_string ("mailto:" ^ email))]
             [(match name with None -> pcdata email | Some v -> pcdata v)] :
             Html5_types.phrasing elt
          ) :: acc
        | SimpleTag (key, content) -> (
          match key with
            | "google" ->
              (a ~a:[a_href (uri_of_string
                               ("http://www.google.com/search?q=" ^ content))]
                 [pcdata content]) :: acc
            | "wikipedia" ->
              (a ~a:[a_href (uri_of_string
                               ("http://www.wikipedia.org/wiki/" ^ content))]
                 [pcdata content]) :: acc
            | "youtube" ->
              (object_
                 ~params:[
                   param
                     ~a:[a_name "movie";
                         a_text_value ("http://www.youtube.com/v/" ^ content)]
                     ()]
                 ~a:[a_width 425; a_height 366]
                 [embed ~a:[a_src (uri_of_string
                                     ("http://www.youtube.com/v/" ^ content));
                            a_mime_type "application/x-shockwave-flash";
                            a_width 425; a_height 366] ()]) :: acc
            | _ -> failwith "unknown simpletag"
        )
        | Url (url, text) ->
          let url =
            if (String.length url > 7 && String.sub url 0 7 = "http://") ||
              (String.length url > 6 && String.sub url 0 6 = "ftp://") ||
              (String.length url > 8 && String.sub url 0 8 = "https://") then
              url
            else if (String.length url > 4 && String.sub url 0 4 = "ftp.") then
              "ftp://" ^ url
            else if String.length url > 1 && url.[0] = '/' then
              url
            else
              "http://" ^ url in
            (a ~a:[a_href (uri_of_string url)] [pcdata text]) :: acc
        | Flash ((w, h), url) ->
          (object_~params:[param ~a:[a_name "movie"; a_text_value url] ()]
             ~a:[a_data (uri_of_string url); a_width w; a_height h;
                 a_mime_type "application/x-shockwave-flash"] []) :: acc
        | other -> assert false
    ) is []
    
  let make_hn = function
    | 1 -> h1
    | 2 -> h2
    | 3 -> h3
    | 4 -> h4
    | 5 -> h5
    | 6 -> h6
    | _ -> h6
      
  let replace_bad_chars str =
    let len = String.length str in
    let str = String.copy str in
    let rec aux_replace i =
      if i < len then
        match str.[i] with
          | 'a' .. 'z' | 'A'..'Z' | '0'..'9' | '_' | '-' ->
            aux_replace (succ i)
          | _ -> str.[i] <- '_'; aux_replace (succ i)
      else
        str
    in
      aux_replace 0
        
  let rec make_heading_anchor is =
    let str =
      List.fold_right (fun t acc ->
        match t with
          | Abbr (is, value) -> make_heading_anchor is :: acc
          | Em is -> make_heading_anchor is :: acc
          | Code str -> replace_bad_chars str :: acc
          | Image img -> acc
          | Flash _ -> acc
          | Strong is -> make_heading_anchor is :: acc
          | Subscript is -> make_heading_anchor is :: acc
          | Superscript is -> make_heading_anchor is :: acc
          | Entity e -> acc
          | Str str -> replace_bad_chars str :: acc
          | _ -> acc
      ) is [] in
      String.concat "" str

  let rec collect_inlines acc = function
    | [] -> false, List.rev acc, []
    | Code str as t :: rest ->
      if String.contains str '\n' then
        if acc = [] then
          false, [Code str], rest
        else
          false, List.rev acc, (t :: rest)
      else
        collect_inlines (t :: acc) rest
    | (Strong _ | Em _ | Underline _ | Strike _ | Subscript _ | Superscript _
          | Color _ | Size _ | Font _ | Url _ | Anchor _ | Image _ | Flash _
          | Entity _ | Str _ | SimpleTag _ | Email _ | Abbr _ | Break | Sp
          | Float _ as t) :: rest ->
      collect_inlines (t :: acc) rest
    | NewLine:: NewLine :: rest ->
      true, List.rev acc, rest
    | NewLine :: rest ->
      false, List.rev acc, rest
    | t :: rest ->
      false, List.rev acc, (t :: rest)
        
  let make_index style data =
    let zz = if style = "ul" then ul else ol in
    let make_link is =
      a ~a:[a_href (uri_of_string ("#" ^ make_heading_anchor is))]
        (make_phrasing is :> Html5_types.flow5_without_interactive elt list) in
      
    let rec fold l acc = function
      | [] -> List.rev acc, []
      | x :: xs ->
        match x with
          | Heading (lev, is) ->
            if lev = l then
              let i = (make_link is :> Html5_types.flow5 elt) in
              let items,rest = fold (succ l) [] xs in
                if items = [] then
                  fold l (li [i] :: acc) rest
                else
                  fold l (li [i; zz items] :: acc) rest
            else if lev > l then
              fold lev [] (x::xs)
            else
              List.rev acc, (x::xs)
          | _ -> fold l acc xs
    in
    let rec scan_rest acc = function
      | [] -> acc
      | rest ->
        let items, rest = fold 1 [] rest in
          scan_rest (acc @ items) rest
    in
    let items = scan_rest [] data in
      zz items
        
  let is_next_block = function
    | [] -> false
    | x :: _ ->
      match x with
        | Index _ | Align _ | Heading _ | HorizontalRule | List _ | Table _
        | Indent _ | Quote _ -> true
        | _ -> false
          
  let rec block_to_html data =
    let rec aux_fold acc = function
      | [] -> List.rev acc
      | Heading (level, is) :: rest ->
        let hn = make_hn level in
          aux_fold (hn ~a:[a_id (make_heading_anchor is)]
                      (inline_to_html is) :: acc) rest
      | Indent is :: rest ->
        aux_fold ((div (block_to_html is)) :: acc) rest
      | HorizontalRule :: rest ->
        aux_fold (hr () :: acc) rest
      | Index style :: rest ->
        let index = make_index style data in
          aux_fold (index :: acc) rest
      | Quote (from, is) :: rest ->
        if from = "" then
          aux_fold (blockquote (block_to_html is) :: acc) rest
        else
          aux_fold (blockquote (em [pcdata from; pcdata " wrote:"] ::
                                  block_to_html is) :: acc) rest
      | List (st, ls) :: rest ->
        let zz =
          match st with
            | "c" -> ul ~a:[a_style "list-style-type:circle"]
            | "d" -> ul ~a:[a_style "list-style-type:disc"]
            | "s" -> ul ~a:[a_style "list-style-type:square"]
            | "1" -> ol ~a:[a_style "list-style-type:decimal"]
            | "a" -> ol ~a:[a_style "list-style-type:lower-alpha"]
            | "A" -> ol ~a:[a_style "list-style-type:upper-alpha"]
            | "i" -> ol ~a:[a_style "list-style-type:lower-roman"]
            | "I" -> ol ~a:[a_style "list-style-type:upper-roman"]
            | _ -> ul ~a:[a_style "list-style-type:circle"]
        in
        let res = List.map (fun l -> li (block_to_html l)) ls in
          aux_fold (zz res :: acc) rest
      | Table (head, (color, row), rows) :: rest ->
        let make_color color =
          match color with
            | None -> []
            | Some v -> [a_style ("background-color:"^v)] in
        let make_td col = td (block_to_html col) in
        let thead =
          if head = [] then None else
            Some (thead (List.map (fun (color, row) -> 
              tr ~a:(make_color color) (List.map make_td row)) head)) in
        let row = tr ~a:(make_color color) (List.map make_td row) in
        let rows =
          List.map (fun (color, row) ->
            tr ~a:(make_color color) (List.map make_td row)) rows in
          aux_fold (table ~a:[a_style "border:1px solid"]
                      ?thead row rows :: acc) rest
      | Align (align, smth) :: rest ->
        aux_fold ((div ~a:[a_style ("text-align:"^align)]
                     (block_to_html smth)) :: acc) rest
      | NewLine :: rest ->
        aux_fold acc rest
      | rest ->
        let is_para, is, rest = collect_inlines [] rest in
          match is with
            | [Code str] -> aux_fold (pre [code [pcdata str]] :: acc) rest
            | _ ->
              let res =
                if is_para then
                  p (inline_to_html is) :: acc
                else if is_next_block rest then
                  (inline_to_html is :> Html5_types.flow5 elt list) @ acc
                else
                  br () :: (List.rev (inline_to_html is :>
                                        Html5_types.flow5 elt list)) @acc
              in
                aux_fold res rest
    in
      aux_fold [] data
        
  let make_html5 data = block_to_html data
end    
    
