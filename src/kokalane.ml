open Pprint
open Infer
open Eval

let _ =
  let parse_only = ref false in
  let type_only = ref false in
  let read_file f =
    let ic = open_in f in
    let lexbuf = Lexing.from_channel ~with_positions:true ic in
    Lexing.set_filename lexbuf f;
    let p =
      try
        Parser.file Lexer.next_token lexbuf
      with
        _ ->
          Error.error_str_lexbuf lexbuf (Printf.sprintf "Unexpected token: \"%s\""
                                           (Lexing.lexeme lexbuf))
    in
    List.iter (fun d -> print_endline (Syntax.show_decl_loc d)) p;
    if !parse_only then exit 0;
    let pt, main =
      try
        check_file p
      with
        NoMain -> Error.error_str_lexbuf lexbuf "Missing main function"
    in
    List.iter (fun d -> print_endline (Syntax.show_decl_type d)) pt;
    if !type_only then exit 0;
  in
  Arg.parse ["--parse-only", Arg.Set parse_only, "Stop after parsing";
             "--type-only", Arg.Set type_only, "Stop after type checking"]
    read_file ""
