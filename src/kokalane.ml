open Pprint
open Type
open Eval

let _ =
  let ic = open_in "test.koka" in
  let lexbuf = Lexing.from_channel ~with_positions:true ic in
  Lexing.set_filename lexbuf "test.koka";
  let p =
    try
      Parser.file Lexer.lexer lexbuf
    with
      _ ->
      Error.error_str_lexbuf lexbuf (Printf.sprintf "Unexpected token: \"%s\""
                            (Lexing.lexeme lexbuf))
  in
  List.iter (fun d -> print_endline (Syntax.show_decl_loc d)) p;
  let pt = List.map check_decl p in
  List.iter (fun d -> print_endline (Syntax.show_decl_type d)) pt
