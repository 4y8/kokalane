  let error lexbuf msg =
    let bg = Lexing.lexeme_start_p lexbuf in
    let nd = Lexing.lexeme_end_p lexbuf in
    let b = bg.pos_cnum - bg.pos_bol in
    let e = nd.pos_cnum - nd.pos_bol in
    Printf.printf "%s\n" (Lexing.lexeme lexbuf);
    Printf.fprintf stderr "File \"%s\", line %d, characters %d-%d:\n%s\n"
      bg.pos_fname bg.pos_lnum b e msg;
    exit 1
