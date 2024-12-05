open Infer
open Codegen

let _ =
  let parse_only = ref false in
  let type_only = ref false in
  let read_file f =
    let ic = open_in f in
    let prefix = if not (String.ends_with ~suffix:".koka" f) then
        (Printf.eprintf "usage: kokac infile.koka"; exit 1)
      else String.sub f 0 (String.length f - 5)
    in
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
    if !parse_only then exit 0;
    let pt =
      try
        check_file p
      with
        NoMain -> Error.error_str_lexbuf lexbuf "Missing main function"
    in
    if !type_only then exit 0;
    let a = Annot.annot_program pt in
    X86_64.print_in_file ~file:(prefix ^ ".s") (Codegen.gen_prog a)
  in
  Arg.parse ["--parse-only", Arg.Set parse_only, "Stop after parsing";
             "--type-only", Arg.Set type_only, "Stop after type checking"]
    read_file ""
