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
%token SCOL DOT DCOL EOF
%start file
%type <decl list> file

%%

bexpr:

stmt:
    e=bexpr SCOL+ { SExpr e }
  | VAL x=ident ASS l=bexpr SCOL+ { SVal (x, l) }
  | VAR x=ident WAL l=bexpr SCOL+ { SVar (x, l) }

expr:
    e=bexpr { [e] }
  | LCUR SCOL* s=stmt* RCUR { s }

funbody:
  LPAR RPAR EOF { ([], []) }

decl:
  FUN name=IDENT fb=funbody SCOL+ { let arg, body = fb in { name ; arg ; body } }


file:
  SCOL* l = decl* EOF { l }
