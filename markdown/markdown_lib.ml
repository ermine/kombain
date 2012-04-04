open Printf
open Kmb_input

type target =
  | Src of string * string option
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
  | HtmlComment of string
  | Html of string * (string * string option) list * inline list

and block =
  | Para of inline list
  | Plain of inline list
  | Heading of int * inline list
  | BlockQuote of string
  | BulletList of block list list
  | OrderedList of block list list
  | Verbatim of string
  | HorizontalRule
  | Reference of inline list * target
  | HtmlBlock of string * (string * string option) list * block list
      
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
  | HtmlComment s ->
    printf "HTML Comment %S\n" s
  | Html (name, attrs, inlines) ->
    printf "Html in inline\n"
(*    List.iter print_inline inlines *)

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
  | BlockQuote str ->
    printf "Blockquote\n%s\n" str
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
  | Verbatim string ->
    printf "Verbatim %S\n" string
  | HorizontalRule ->
    printf "HorizontalRule\n"
  | Reference (inlines, target) ->
    printf "Reference [\n";
    List.iter print_inline inlines;
    printf "]\n"

let add_verbatim state _ _ lexeme =
  match state with
    | Verbatim lines :: xs -> Verbatim (lines ^ lexeme) :: xs
    | state -> Verbatim lexeme :: state

let make_entity state _ _ lexeme =
  Entity lexeme :: state

let make_text state _ _ lexeme =
  Text lexeme :: state

open Kmb_lib

let rec repeat n symbol input =
  if n = 0 then
    Parsed ((), input)
  else
    match symbol input with
      | Parsed (_, input) -> repeat (pred n) symbol input
      | Failed -> Failed
  
