{
  open Parser
  let ident_tbl = Hashtbl.create 63

  List.iter (fun (k, v) -> Hashtbl.add ident_tbl k v)
    ["elif", ELIF; "else", ELSE; "fn", FN; "fun", FUN; "if", IF;
     "return", RETURN; "then", THEN; "val", VAL; "var", VAR]

  let error lexbuf msg =
    let bg = Lexing.lexeme_start_p lexbuf in
    let nd = Lexing.lexeme_end_p lexbuf in
    Printf.fprintf stderr "File \"%s\", line %d, characters %d-%d:\n%s"
      bg.pos_fname bg.pos_lnum bg.pos_bol nd.pos_bol msg;
    exit 1
}

let digit = ['0'-'9']
let lower = ['a'-'z'] | '_'
let upper = ['A'-'Z']
let other = lower | upper | digit | '-'

rule lexer = parse
  | "//" [^'\n']* '\n' { Lexing.new_line lexbuf ; lexer lexbuf }
  | "++" { DPLUS }
  | "+" { PLUS }
  | "->" { ARR }
  | "-" { MINUS }
  | "*" { TIMES }
  | "//" { IDIV }
  | ":=" { WAL }
  | "||" { OR }
  | "&&" { AND }
  | "." { DOT }
  | ";" { SCOL }
  | ":" { DCOL }
  | "/*" { comment lexbuf }
  | ('-'? ('0' | ['1'-'9'] digit*)) as s { INT (inf_of_string s) }
  | (lower other* '\''*) as s { match Hashtbl.find_opt ident_tbl s with
    None -> IDENT s | Some t -> t }
  | '"' { STRING (string lexbuf) }
  | eof { EOF }

and comment = parse
  | "*/" { lexer lexbuf }
  | '\n' { Lexing.new_line lexbuf ; comment lexbuf }
  | _ { comment lexbuf }

and string acc = parse
  | '"' { List.rev acc |> List.to_seq |> String.of_seq }
  | '\\' (_ as c)
    { match c with
        '"' -> '"' :: string lexbuf
      | '\\' ->  '\\' :: string lexbuf
      | 't' ->  '\t' :: string lexbuf
      | 'n' ->  '\n' :: string lexbuf
      | c -> error lexbuf (Printf.sprintf "Unknown control sequence : %c" c) }
  | _ as c { c :: string lexbuf }
{

}
