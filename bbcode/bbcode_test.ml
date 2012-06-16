open Format
open Bbcode_parser
  
let rec print_token = function
  | Strong is ->
    printf "Strong @[<v 0>";
    List.iter print_token is;
    printf "@]@ "
  | Em is ->
    printf "Em @[<v 0>";
    List.iter print_token is;
    printf "@]@ "
  | Underline is ->
    printf "Underline @[<v 0>";
    List.iter print_token is;
    printf "@]@ "
  | Strike is ->
    printf "Strike @[<v 0>";
    List.iter print_token is;
    printf "@]@ "
  | Subscript is ->
    printf "Subscript @[<v 0>";
    List.iter print_token is;
    printf "@]@ "
  | Superscript is ->
    printf "Superscript @[<v 0>";
    List.iter print_token is;
    printf "@]@ "    
  | Color (str, is) ->
    printf "Color @[<v 0>%s@ " str;
    List.iter print_token is;
    printf "@]@ "
  | Size (size, is) ->
    printf "Size @[<v 0>%d@ " size;
    List.iter print_token is;
    printf "@]@ "
  | Font (face, is) ->
    printf "Font @[<v 0>%S@ " face;
    List.iter print_token is;
    printf "@]@ "
  | Url (url, text) ->
    printf "Url @[<hov 0>%s@ %s@]@ " url text
  | Anchor (a, is) ->
    printf "Ahchor @[%s@ %s@ " a;
    List.iter print_token is;
    printf "@]"
  | Image img ->
    let width, height =
      match img.dim with
        | None -> 0, 0
        | Some (w, h) -> w, h
    in
      printf
        "Image {@[<hov 0>src=%S@ alt=%S@ align=%S@ width=%d@ height=%d}@]@ "
        img.src img.alt img.align width height;
  | Flash ((w, h), url) ->
    printf "Flash (%d,%d)@ %s@ " w h url
  | Indent ts ->
    printf "Indent @[<v 0>";
    List.iter print_token ts;
    printf "@]"
  | Entity e ->
    printf "Entity %S@ " e
  | Str str ->
    printf "Str %S@ " str
  | SimpleTag (tag, w) ->
    printf "SimpleTag @[<hov 0>%S@ %S@]@ " tag w
  | Email (email, name) ->
    printf "Email @[<hov 0>%S@ %s@]@ " email
      (match name with None -> "none" | Some v -> v)
  | Abbr (is, value) ->
    printf "Abbr @[<hov 0>%S@ %S@ " value;
    List.iter print_token is;
    printf "@]"
  | Break ->
    printf "Break@;"
  | Sp ->
    printf "Sp@;"

  | Heading (level, is) ->
    printf "Heading %d @[<v 0>" level;
    List.iter print_token is;
    printf "@]@;"
  | Quote (from, is) ->
    printf "Quote @[<hv 0>%S@ " from;
    List.iter print_token is;
    printf "@]@;"
  | Code str ->
    printf "Code %S@ " str
  | List (t, is) ->
    printf "List %S@[<v 0>@ " t;
    List.iter (fun b ->
      printf "@[<v 0>";
      List.iter print_token b;
      printf "@]"
    ) is;
    printf "@]@;"
  | Table (hsss, (c, bss), bsss) ->
    printf "Table@ @[<v 0>";
    printf "@[<v 0>";
    List.iter (fun (c, bss) ->
      printf "Header @[<v 0>color: %s@ " (match c with None -> "" | Some v -> v);
      List.iter (fun bs ->
        printf "@[<v 0>";
        List.iter print_token bs;
        printf "@]@;"
      ) bss;
      printf "@]@ "
    ) hsss;
    printf "@]@;";

    printf "@[<v 0>color: %s@ " (match c with None -> "none" | Some v -> v);
    List.iter (fun bs ->
      printf "@[<v 0>";
      List.iter print_token bs;
      printf "@]@ "
    ) bss;
    printf "@]@ ";
    
    List.iter (fun (c, bss) ->
      printf "@[<v 0>color: %s@ " (match c with None -> "none" | Some v -> v);
      List.iter (fun bs ->
        printf "@[<v 0>";
        List.iter print_token bs;
        printf "@]@ "
      ) bss;
      printf "@]@ "
    ) bsss;
    printf "@]@;"
  | Index style ->
    printf "Index style=%s@;" style
  | HorizontalRule ->
    printf "HorizontalRule@;"
  | Align (align, is) ->
    printf "Align %s@[@ " align;
    List.iter print_token is;
    printf "@]@;"
  | Float (v, ts) ->
    printf "Float@ %s@[<v 0>" v;
    List.iter print_token ts;
    printf "@]"
  | NewLine ->
    printf "NewLine@;"


let rec union_str acc = function
  | [] -> List.rev acc
  | Str x :: Str y :: rest ->
    union_str acc ((Str (x ^ y)) :: rest)
  | Str _ as x :: rest -> 
    union_str (x :: acc) rest
  | Entity e :: rest ->
    union_str (Entity e:: acc) rest
  | Strong is :: rest ->
    union_str ((Strong (union_str [] is)) :: acc) rest
  | Em is :: rest ->
    union_str ((Em (union_str [] is)) ::acc) rest
  | Underline is :: rest ->
    union_str ((Underline (union_str[] is)) :: acc) rest
  | Strike is :: rest ->
    union_str ((Strike (union_str [] is)) :: acc) rest
  | Align (v, is) :: rest ->
    union_str ((Align (v, union_str [] is)) :: acc) rest
  | Float (v, is) :: rest ->
    union_str ((Float (v, union_str [] is)) :: acc) rest
  | Size (v, is) :: rest ->
    union_str ((Size (v, union_str [] is)) :: acc) rest
  | Subscript is :: rest ->
    union_str ((Subscript (union_str [] is)) :: acc) rest
  | Superscript is :: rest ->
    union_str ((Superscript (union_str [] is)) :: acc) rest
  | Font (v, is) :: rest ->
    union_str ((Font (v, union_str [] is)) :: acc) rest
  | Color (v, is) :: rest ->
    union_str ((Color (v, union_str [] is)) :: acc) rest
  | Indent is :: rest ->
    union_str ((Indent (union_str [] is)) :: acc) rest
  | Heading (v, is) :: rest ->
    union_str ((Heading (v, union_str [] is)) :: acc) rest
  | Quote (v, is) :: rest ->
    union_str ((Quote (v, union_str [] is)) :: acc) rest
  | List (v, ls) :: rest ->
    union_str ((List (v, List.map (union_str []) ls)) :: acc) rest
  | Table (h, (c, r), rs) :: rest ->
    union_str ((Table
                  (List.map (fun (c, rows) ->
                    (c, List.map (union_str []) rows)) h,
                   (c, List.map (union_str []) r),
                   List.map (fun (c, rows) ->
                     (c, List.map (union_str []) rows)) rs)) :: acc)
      rest
  | (Index _ | NewLine | Image _ | Flash _ | Url _ | Code _
        | Abbr _ | Anchor _ | SimpleTag _ | Sp | HorizontalRule
        | Break | Email _) as t :: rest ->
    union_str (t :: acc) rest
    

let rec remove_newlines acc = function
  | [] -> acc
  | (Heading _ | List _ | Table _ | Align _ | Float _ | Break | HorizontalRule
        | Quote _ | Index _ | Indent _ as t) :: NewLine :: rest ->
    remove_newlines acc (t :: rest)
  | NewLine :: NewLine :: rest ->
    remove_newlines acc (NewLine :: rest)
  | x :: rest ->
    remove_newlines (x :: acc) rest
      
let () =
  let input = Kmb_input.of_file Sys.argv.(1) in
  let result = Bbcode_parser.parse input in
    match result with
      | Kmb_lib.Failed ->
        failwith "failed to parse bbcode"
      | Kmb_lib.Parsed (ast, rest) ->
        Printf.printf "Parsed, remaining %S\n" (Kmb_input.get_remaining rest);
        let data =
          remove_newlines [] (List.rev (union_str [] ast)) in
          printf "@[<v 0>";
          List.iter print_token data;
          printf "@]"
          
