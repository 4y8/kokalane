open Lexing

let error bg nd msg =
    let b = bg.pos_cnum - bg.pos_bol in
    let e = nd.pos_cnum - nd.pos_bol in
    Printf.fprintf stderr "File \"%s\", line %d, characters %d-%d:\n%s\n"
      bg.pos_fname bg.pos_lnum b e msg;
    exit 1

let error_lexbuf lexbuf =
  error (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)
