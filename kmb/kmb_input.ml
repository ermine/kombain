type input = {
  buf : string;
  pos : int;
  len : int;
  line : int;
  col : int
}

type lexeme = {
  start : int * int;
  stop : int * int;
  lexeme : string
}


let end_of_file input =
  input.pos = input.len

let incr_pos input =
  (* no boundaries check due to incr_pos is called only
     after successful input tests *)
  if input.buf.[input.pos] = '\n' then
    {input with pos = input.pos + 1;
      line = input.line + 1;
      col = 0
    }
  else if input.buf.[input.pos] = '\t' then
    { input with pos = input.pos + 1;
      col = ((input.col + 8 - 1) / 8) * 8 + 1
    }
  else
    { input with pos = input.pos + 1;
      col = input.col + 1
    }

let make_input str =
  { pos = 0; len = String.length str; buf = str; line = 1; col = 0}

    
let read_file file =
  let f = open_in file in
  let rec aux_read acc =
    let line =
      try Some (input_line f)
      with _ -> None in
      match line with
        | None -> List.rev acc
        | Some line -> aux_read (line :: acc)
  in
  let lines = aux_read [] in
    close_in f;
    String.concat "\n" lines ^ "\n"

let get_current input =
  Printf.printf "get_current %d:%d %C\n" input.line input.col
    input.buf.[input.pos];
  Char.code (input.buf.[input.pos])

