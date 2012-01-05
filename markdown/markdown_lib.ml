open Printf

type target =
    Src of string * string option
  | Ref of inline list * string
  | Nothing

and inline =
  | Text of string
  | Entity of string
  | Space
  | LineBreak
  | Emph of inline list
  | Strong of inline list
  | Code of string
  | Link of inline list * target
  | Image of inline list * target
  | Html of string

type block =
  | Para of inline list
  | Plain of inline list
  | Heading of int * inline list
  | BlockQuote of block list
  | BulletList of block list list
  | OrderedList of block list list
  | HTMLBlock of string
  | Verbatim of string
  | HorizontalRule
  | Reference of inline list * target
  | Markdown of string
      
let rec print_inline = function
  | Text str ->
    printf "Text %S\n" str
  | Entity str ->
    printf "Entity %S\n" str
  | Space ->
    printf "Space\n"
  | LineBreak ->
    printf "LineBreak\n"
  | Emph inlines ->
    printf "Emb [\n";
    List.iter print_inline inlines;
    printf "]\n"
  | Strong inlines ->
    printf "Strong [\n";
    List.iter print_inline inlines;
    printf "]\n"    
  | Code str ->
    printf "Code %S\n" str
  | Link (inlines, target) ->
    printf "Link [\n";
    List.iter print_inline inlines;
    printf "]\n"      
  | Image (inlines, target) ->
    printf "Image [\n";
    List.iter print_inline inlines;
    printf "]\n"      
  | Html str ->
    printf "HTML %S\n" str

let rec print_token = function
  | Para inlines ->
    printf "Para [\n";
    List.iter print_inline inlines;
    printf "]\n"
  | Plain inlines ->
    printf "Plain [\n";
    List.iter print_inline inlines;
    printf "]\n"
  | Heading (level, inlines) ->
    printf "Heading %d [\n" level;
    List.iter print_inline inlines;
    printf "]\n"    
  | BlockQuote blocks ->
    printf "Blockquote [\n";
    List.iter print_token blocks;
    printf "]\n"
  | BulletList blocks ->
    printf "BulletList [\n";
    List.iter (fun block ->
      printf "* [\n";
      List.iter print_token block;
      printf "]\n"
    ) blocks;
    printf "]\n" 
  | OrderedList blocks ->
    printf "OrderedList [\n";
    List.iter (fun block ->
      printf "* [\n";
      List.iter print_token block;
      printf "]\n"
    ) blocks;
    printf "]\n"    
  | HTMLBlock string ->
    printf "HtmlBlock %S\n" string
  | Verbatim string ->
    printf "Verbatim %S\n" string
  | HorizontalRule ->
    printf "HorizontalRule\n"
  | Reference (inlines, target) ->
    printf "Reference [\n";
    List.iter print_inline inlines;
    printf "]\n"
  | Markdown string ->
    printf "Markdown %S\n" string
      


let add_verbatim state _ _ lexeme =
  match state with
    | Verbatim lines :: xs -> Verbatim (lines ^ lexeme) :: xs
    | state -> Verbatim lexeme :: state

let make_entity state _ _ lexeme =
  Entity lexeme :: state

let make_text state _ _ lexeme =
  Text lexeme :: state

let blockTags =
  List.fold_left (fun acc x -> x :: String.uppercase x :: acc) [] [
    "address"; "blockquote"; "center"; "dir"; "div"; "dl"; "fieldset"; "form";
    "h1"; "h2"; "h3"; "h4"; "h5"; "h6"; "hr"; "isindex"; "menu"; "noframes";
    "noscript"; "ol"; "p"; "pre"; "table"; "ul"; "dd"; "dt"; "frameset"; "li";
    "tbody"; "td"; "tfoot"; "th"; "thead"; "tr"; "script"
  ]
    
open Kmb_lib

let kmb_htmlBlockTag input =
  let rec aux_test = function
    | [] -> Failed
    | x :: xs ->
      match match_pattern x input with
        | Parsed _ as ok -> ok
        | Failed -> aux_test xs
  in
    aux_test blockTags

let make_blackquote ls =
  let txt = String.concat "" 
    (List.concat (List.map (fun (l1, l2) -> l1 @ 
      (List.map (fun {lexeme} -> lexeme) l2)) ls)) in
    BlockQuote [Markdown txt] 
