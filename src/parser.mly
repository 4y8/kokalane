%{
    open Syntax
%}

%token ELIF ELSE FN FUN IF RETURN THEN VAL VAR
%token <string>STRING
%token <int>INT
%token <string>IDENT
%token BANG OR AND
%token PLUS MINUS TIMES DIV MOD IDIV LEQ GEQ LT GT EQ DIF ASS WAL ARR DPLUS
%token LPAR RPAR LCUR RCUR LANG RANG LSQU RSQU
%token SCOL DOT DCOL COMMA EOF
%token TRUE FALSE
%token TUNIT TBOOL TINT TSTRING TLIST TMAYBE
%start file
%type <decl list> file

%nonassoc RETURN
%nonassoc WAL
%left PLUS DPLUS MINUS
%left TIMES

%%

farg:
    { [] }
  | e=expr { [e] }
  | e=expr COMMA l=farg { e :: l }
;

lit:
    i=INT { LInt i }
  | s=STRING { LString s }
  | TRUE { LBool true }
  | FALSE { LBool false }
  | LPAR RPAR { LUnit }
;

atype:
    TUNIT { TUnit }
  | TBOOL { TBool }
;

atom:
    x=IDENT { Var x }
  | f=atom LPAR l=farg RPAR { App(f, l) }
  | l=lit { Lit l }
  | LPAR e=expr RPAR { e }
  | x=atom DOT f=IDENT { App(Var f, [x]) }
  | LSQU l=farg RSQU { Lst l }
;

bexpr:
    a=atom { a }
  | x=IDENT WAL e=bexpr { Wal (x, e) }
  | RETURN e=expr { Ret e }
  | e1=bexpr TIMES e2=bexpr { Bop (e1, Mul, e2) }
  | e1=bexpr PLUS e2=bexpr { Bop (e1, Add, e2) }
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
