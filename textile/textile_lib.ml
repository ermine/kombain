open Printf

type content =
  | String of string
  | BreakLine
  | SQuoted of string
  | Dash
  | DDash

type block =
  | Paragraph of content list
  | Preformatted of string

let print_content = function
  | String str ->
    printf "String %S\n" str
  | BreakLine ->
    printf "BreakLine\n"
  | SQuoted str ->
    printf "SQuoted %S\n" str
  | DDash ->
    printf "DDash\n"
  | Dash ->
    printf "Dash\n"

let print_token = function
  | Paragraph content ->
    printf "Paragraph [\n";
    List.iter print_content content;
    printf "]\n"
  | Preformatted text ->
    printf "Preformatted\n%s\n" text
