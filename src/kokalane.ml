open Pprint
open Type

let _ =
  let ic = open_in "test.koka" in
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  Lexing.set_filename lexbuf "test.koka";
  let p =
    try
      Parser.file Lexer.lexer lexbuf
    with
      _ ->
      Error.errorstr_lexbuf lexbuf (Printf.sprintf "Unexpected token: \"%s\""
                            (Lexing.lexeme lexbuf))
  in
  List.iter (fun d -> print_endline (Syntax.show_decl d)) p
