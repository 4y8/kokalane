%{
    open Syntax
%}

%token ELIF ELSE FN FUN IF RETURN THEN VAL VAR
%token <string>STRING
%token <int>INT
%token <string>IDENT
%token BANG OR AND
%token PLUS MINUS TIMES DIV MOD IDIV LEQ GEQ EQ DIF ASS WAL ARR DPLUS
%token LPAR RPAR LCUR RCUR LANG RANG
%token SCOL DOT DCOL COMMA EOF
%start file
%type <decl list> file

%%

farg:
    { [] }
  | e=expr { [e] }
  | e=expr COMMA l=farg { e :: l }
;

lit:
    i=INT { Int i }
  | s=STRING { String s }
;

atom:
    x=IDENT { Var x }
  | f=atom LPAR l=farg RPAR { App(f, l) }
  | l=lit { Lit l }
;

bexpr:
    a=atom { a }
;

stmt:
    e=expr SCOL+ { SExpr e }
  | VAL x=IDENT ASS l=bexpr SCOL+ { SVal (x, l) }
  | VAR x=IDENT WAL l=bexpr SCOL+ { SVar (x, l) }
;

expr:
    e=bexpr { e }
  | LCUR SCOL* s=stmt* RCUR { Blk s }
;

funbody:
  LPAR RPAR e=expr { ([], e) }
;

decl:
  FUN name=IDENT fb=funbody SCOL+ { let arg, body = fb in { name ; arg ; body } }
;

file:
  SCOL* l = decl* EOF { l }
;
