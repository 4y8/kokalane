{
  open Parser

  let ident_tbl = Hashtbl.create 63

  let _ = List.iter (fun (k, v) -> Hashtbl.add ident_tbl k v)
    ["else", ELSE; "fn", FN; "fun", FUN; "if", IF;
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
    [PLUS; MINUS; TIMES; DIV; MOD; DPLUS; LANG; RANG; LEQ; GEQ; EQ;
     DIF; AND; OR; LPAR; LCUR] |> TSet.of_list

  let beg_con =
    [PLUS; MINUS; TIMES; DIV; MOD; DPLUS; LANG; RANG; LEQ; GEQ; EQ; DIF; AND; OR;
     THEN; ELSE; RPAR; RCUR; COMMA; ARR; LCUR; ASS; DOT; WAL] |> TSet.of_list

  let q = Queue.create ()

  let indent = Stack.create ()

  let _ = Stack.push 0 indent

  let last = ref DUMMY

  let new_line lexer lexbuf c =
    let loc = Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf in
    let t = lexer lexbuf in
    let next = List.hd t in
    let c = if next = EOF then 0 else c in
    let m = Stack.top indent in
    if c > m
    then begin
      if not TSet.(mem !last end_con) && not TSet.(mem next beg_con)
      then (Queue.add LCUR q; Stack.push c indent)
      else if !last = LCUR then Stack.push c indent;
    end else begin
      let rec loop () =
        let m = Stack.top indent in
        if c < m then begin
          ignore (Stack.pop indent);
          (match t with
             SCOL :: RCUR :: _ -> ()
          | _ -> (Queue.add SCOL q; Queue.add RCUR q));
          loop ()
        end else if c > m then Error.error_str loc "Wrong indentation"
        else
          if not TSet.(mem !last end_con) && not TSet.(mem next beg_con)
            then Queue.add SCOL q
      in loop ()
    end;
    List.iter (fun t -> Queue.add t q) t;
    [Queue.take q]

  let insert_nl lexbuf =
    String.iter (fun c -> if c = '\n' then Lexing.new_line lexbuf)
}

let digit = ['0'-'9']
let lowerl = ['a'-'z']
let lower = lowerl | '_'
let upper = ['A'-'Z']
let other = lower | upper | digit
let ident = ((lower | (lowerl '-' (upper | lowerl))) (other | (lowerl | upper | digit) '-' (lowerl | upper))* ((lowerl | upper | digit) '-' | '\''*)) | lowerl '-'

let blank_line = [' ' '\t' '\r']* '\n' | "//" [^'\n']* '\n'

rule lexer = parse
  | [' ' '\t' '\r'] { lexer lexbuf }
  | (blank_line+ as nl) (' '*) "/*" { insert_nl lexbuf nl; begin_line_comment lexbuf }
  | (blank_line+ as nl) ((' '*) as s)
    { insert_nl lexbuf nl; new_line lexer lexbuf (String.length s) }
  | "//" [^'\n']* '\n' { Lexing.new_line lexbuf; lexer lexbuf }
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
  | "}" { [SCOL; RCUR] }
  | "[" { [LSQU] }
  | "]" { [RSQU] }
  | "<" { [LANG] }
  | ">" { [RANG] }
  | "!" { [BANG] }
  | "~" { [TILDE] }
  | "/*" { comment lexbuf }
  | ('-'? ('0' | ['1'-'9'] digit*)) as s { [INT (int_of_string s)] }
  | "elif" { [ELSE; IF] }
  | ident as s { match Hashtbl.find_opt ident_tbl s with
    None -> [IDENT s] | Some t -> [t] }
  | "True" { [TRUE] }
  | "False" { [FALSE] }
  | '"' { [STRING (string [] lexbuf)] }
  | eof { [EOF] }
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
      | 'n' -> string ('\n' :: acc) lexbuf
      | c -> error lexbuf (Printf.sprintf "Unknown control sequence : %c" c) }
  | '\n' { error lexbuf "Newline character in string" }
  | eof { error lexbuf "Unterminated string" }
  | _ as c { string (c :: acc) lexbuf }

and begin_line_comment = parse
  | "*/" (blank_line+ as nl) ((' '*) as s)
    { insert_nl lexbuf nl ; new_line lexer lexbuf (String.length s) }
  | "*/" eof { [EOF] }
  | "*/" { error lexbuf "Comment disturbing indentation" }
  | eof { error lexbuf "Unterminated comment" }
  | '\n' { Lexing.new_line lexbuf ; begin_line_comment lexbuf }
  | _ { begin_line_comment lexbuf }

{
  let print_token = function
    SCOL -> print_endline ";"
  | RCUR -> print_endline "}"
  | LCUR -> print_endline "{"
  | IDENT s -> print_endline s
  | INT n -> Printf.printf "%d\n" n
  | FUN -> print_endline "fun"
  | LPAR -> print_endline "("
  | RPAR -> print_endline ")"
  | IF -> print_endline "if"
  | DCOL -> print_endline ":"

  let next_token lexbuf =
    let t =
      if not Queue.(is_empty q) then
        Queue.take q
      else (
        let l = lexer lexbuf in
        List.iter (fun t -> Queue.add t q) (List.tl l);
        List.hd l
       )
    in
    last := t; t
}
