type target =
    Src of string * string
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
  | Blockquote of block list
  | BulletList of block list list
  | OrderedList of block list list
  | HtmlBLock of string
  | Verbatim of string
  | HorizontalRule
  | Reference of inline list * target
  | Markdown of string
      
let print_token = function
  | Verbatim v ->
    Printf.printf "Verbatim %s\n" v
  | HorizontalRule ->
    Printf.printf "HorizontalRule\n"
  | _ ->
    Printf.printf "Not implemented\n"


let add_verbatim state _ _ lexeme =
  match state with
    | Verbatim lines :: xs -> Verbatim (lines ^ lexeme) :: xs
    | state -> Verbatim lexeme :: state

let make_entity state _ _ lexeme =
  Entity lexeme :: state

let make_text state _ _ lexeme =
  Text lexeme :: state
