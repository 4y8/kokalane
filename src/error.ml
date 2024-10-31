open Lexing
open Pprint
open Format
open Syntax

let error (bg, nd) msg =
  let b = bg.pos_cnum - bg.pos_bol in
  let e = nd.pos_cnum - nd.pos_bol in
  Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n"
    bg.pos_fname bg.pos_lnum b e;
  msg err_formatter;
  exit 1

let error_str loc s =
  error loc (fun fmt -> fprintf fmt "%s" s)

let error_str_lexbuf lexbuf s =
  error (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)
    (fun fmt -> fprintf fmt "%s" s)

let type_mismatch loc t t' = 
  error loc (fun fmt -> Format.fprintf fmt "Type mismatch: expected an\
                                            expression of type %a, got an expression of type %a"
                Pprint.fmt_type t Pprint.fmt_type t')

let unknown_var loc var =
  error_str loc ("Unknown variable: " ^ var)
