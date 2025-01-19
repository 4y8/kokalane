{
  open Parser

  let ident_tbl = Hashtbl.create 63

  let _ = List.iter (fun (k, v) -> Hashtbl.add ident_tbl k v)
    ["else", ELSE; "fn", FN; "fun", FUN; "if", IF; "type", TYPE; "match", MATCH;
     "return", RETURN; "then", THEN; "val", VAL; "var", VAR]

  let error = Error.error_str_lexbuf

  module Token : sig
    type t = token
    val compare : t -> t -> int
  end = struct
    type t = token
    let compare = compare
  end
  module TSet = Set.Make(Token)

  let end_con =
    [PLUS; MINUS; TIMES; DIV; MOD; DPLUS; LANG;
     (*RANG;*)
     LEQ; GEQ; EQ;
     DIF; AND; OR; LPAR; LCUR] |> TSet.of_list

  let beg_con =
    [PLUS; MINUS; TIMES; DIV; MOD; DPLUS; LANG; RANG; LEQ; GEQ; EQ; DIF; AND;
    OR; THEN; ELSE; RPAR; RCUR; COMMA; ARR; LCUR; ASS; DOT; WAL] |> TSet.of_list

  let q = Queue.create ()

  let indent = Stack.create ()

  let _ = Stack.push 0 indent

  let last = ref DUMMY

  let rec loop c loc next t =
    let m = Stack.top indent in
      if c < m then begin
        ignore (Stack.pop indent);
        if next <> RCUR
        then RCUR :: loop c loc next t
        else loop c loc next t
      end
      else if c > m then Error.error_str loc "Wrong indentation"
      else
        if not TSet.(mem !last end_con) && not TSet.(mem next beg_con)
        then SCOL :: t
        else t

  let eof lexbuf =
    let loc = Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf in
    loop 0 loc EOF [EOF]

  let new_line lexer lexbuf c =
    let loc = Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf in
    let t = lexer lexbuf in
    let next = List.hd t in
    let m = Stack.top indent in
    if c > m
    then begin
      if not TSet.(mem !last end_con) && not TSet.(mem next beg_con)
      then (Stack.push c indent; LCUR :: t)
      else
        (if !last = LCUR then Stack.push c indent;
         t)
    end else
      loop c loc next t
}

let digit = ['0'-'9']
let lowerl = ['a'-'z']
let lower = lowerl | '_'
let upper = ['A'-'Z']
let other = lower | upper | digit
let ident =
  (lower | (lowerl '-' (upper | lowerl)))
  (other | (lowerl | upper | digit) '-' (lowerl | upper))*
 ((lowerl | upper | digit) '-' | '\''*)
 | lowerl '-'

let upident =
  (upper ('-' (upper | lowerl))?)
  (other | (lowerl | upper | digit) '-' (lowerl | upper))*
 ((lowerl | upper | digit) '-' | '\''*)
 | upper '-'

let blank_line = [' ' '\t' '\r']* ("//" [^'\n']*)? '\n'

rule lexer = parse
  | [' ' '\t' '\r'] { lexer lexbuf }
  | blank_line { Lexing.new_line lexbuf; blank_lines lexbuf }
  | "++" { [DPLUS] }
  | "+" { [PLUS] }
  | "->" { [ARR] }
  | "-" { [MINUS] }
  | "*" { [TIMES] }
  | "/" { [DIV] }
  | "%" { [MOD] }
  | ":=" { [WAL] }
  | "==" { [EQ] }
  | "!=" { [DIF] }
  | "=" { [ASS] }
  | "<=" { [LEQ] }
  | ">=" { [GEQ] }
  | "||" { [OR] }
  | "&&" { [AND] }
  | "." { [DOT] }
  | "," { [COMMA] }
  | ";" { [SCOL] }
  | ":" { [DCOL] }
  | "(" { [LPAR] }
  | ")" { [RPAR] }
  | "{" { [LCUR] }
  | "}" { [RCUR] }
  | "[" { [LSQU] }
  | "]" { [RSQU] }
  | "<" { [LANG] }
  | ">" { [RANG] }
  | "!" { [BANG] }
  | "~" { [TILDE] }
  | "/*" { comment lexbuf }
  | ('-'? ('0' | ['1'-'9'] digit*)) as s { [INT (int_of_string s)] }
  | "elif" { [ELSE; IF] }
  | "True" { [TRUE] }
  | "False" { [FALSE] }
  | ident as s { match Hashtbl.find_opt ident_tbl s with
    None -> [IDENT s] | Some t -> [t] }
  | upident as s { [CON s] }
  (* en modifiant lex_start_p on affiche la bonne position pour la chaine de
     caractères, mais s'il y a une erreur de syntaxe, le token affiché est faux
     (on n'affiche que le guillemet final) *)
  | '"'
      { let spos = lexbuf.lex_start_p in
        let s = string [] lexbuf in
        lexbuf.lex_start_p <- spos;
        [STRING s] }
  | eof { eof lexbuf }
  | _ as c { error lexbuf (Printf.sprintf "Unknown character : %c" c) }

and comment = parse
  | "*/" { lexer lexbuf }
  | '\n' { Lexing.new_line lexbuf ; comment lexbuf }
  | eof { error lexbuf "Unterminated comment" }
  | _ { comment lexbuf }

and string acc = parse
  | '"' { List.rev acc |> List.to_seq |> String.of_seq }
  | '\\' (_ as c)
    { match c with
        '"' -> string ('"' :: acc) lexbuf
      | '\\' -> string ('\\' :: acc) lexbuf
      | 't' -> string ('\t' :: acc) lexbuf
      | 'r' -> string ('\r' :: acc) lexbuf
      | 'n' -> string ('\n' :: acc) lexbuf
      | c -> error lexbuf (Printf.sprintf "Unknown control sequence : %c" c) }
  | '\n' { error lexbuf "Newline character in string" }
  | eof { error lexbuf "Unterminated string" }
  | _ as c { string (c :: acc) lexbuf }

and begin_line_comment = parse
  | "*/" blank_line { Lexing.new_line lexbuf; blank_lines lexbuf }
  | "*/" eof { eof lexbuf }
  | "*/" { error lexbuf "Comment disturbing indentation" }
  | eof { error lexbuf "Unterminated comment" }
  | '\n' { Lexing.new_line lexbuf ; begin_line_comment lexbuf }
  | _ { begin_line_comment lexbuf }

and blank_lines = parse
  | blank_line { Lexing.new_line lexbuf; blank_lines lexbuf }
  | ' '* "/*" { begin_line_comment lexbuf }
  | ' '* as s { new_line lexer lexbuf (String.length s) }
  | ' '* eof { eof lexbuf }

{
  let emited_scol = ref false

  let next_token lexbuf =
    if Queue.is_empty q then
      List.iter (fun t -> Queue.add t q) (lexer lexbuf);
    let t =
      if Queue.peek q = RCUR && not !emited_scol then
        (emited_scol := true; SCOL)
      else
        (emited_scol := false; Queue.take q)
    in
    last := t; t
}
