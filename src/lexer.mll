{
  open Parser
  open Error

  let ident_tbl = Hashtbl.create 63

  let _ = List.iter (fun (k, v) -> Hashtbl.add ident_tbl k v)
    ["elif", ELIF; "else", ELSE; "fn", FN; "fun", FUN; "if", IF;
     "return", RETURN; "then", THEN; "val", VAL; "var", VAR;
     "True", TRUE; "False", FALSE]
}

let digit = ['0'-'9']
let lower = ['a'-'z'] | '_'
let upper = ['A'-'Z']
let other = lower | upper | digit | '-'

rule lexer = parse
  | [' ' '\t' '\r'] { lexer lexbuf }
  | '\n' { Lexing.new_line lexbuf; lexer lexbuf }
  | "//" [^'\n']* '\n' { Lexing.new_line lexbuf ; lexer lexbuf }
  | "++" { DPLUS }
  | "+" { PLUS }
  | "->" { ARR }
  | "-" { MINUS }
  | "*" { TIMES }
  | "//" { IDIV }
  | ":=" { WAL }
  | "==" { EQ }
  | "!=" { DIF }
  | "=" { ASS }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "<" { LT }
  | ">" { GT }
  | "||" { OR }
  | "&&" { AND }
  | "." { DOT }
  | "," { COMMA }
  | ";" { SCOL }
  | ":" { DCOL }
  | "(" { LPAR }
  | ")" { RPAR }
  | "{" { LCUR }
  | "}" { RCUR }
  | "[" { LSQU }
  | "]" { RSQU }
  | "/*" { comment lexbuf }
  | ('-'? ('0' | ['1'-'9'] digit*)) as s { INT (int_of_string s) }
  | (lower other* '\''*) as s { match Hashtbl.find_opt ident_tbl s with
    None -> IDENT s | Some t -> t }
  | '"' { STRING (string [] lexbuf) }
  | eof { EOF }
  | _ as c { error lexbuf (Printf.sprintf "Unknown character : %c" c) }

and comment = parse
  | "*/" { lexer lexbuf }
  | '\n' { Lexing.new_line lexbuf ; comment lexbuf }
  | _ { comment lexbuf }

and string acc = parse
  | '"' { List.rev acc |> List.to_seq |> String.of_seq }
  | '\\' (_ as c)
    { match c with
        '"' -> string ('"' :: acc) lexbuf
      | '\\' -> string ('\\' :: acc) lexbuf
      | 't' -> string ('\t' :: acc) lexbuf
      | 'n' -> string ('\n' :: acc) lexbuf
      | c -> error lexbuf (Printf.sprintf "Unknown control sequence : %c" c) }
  | '\n' { error lexbuf "Newline character in string" }
  | _ as c { string (c :: acc) lexbuf }

{

}
