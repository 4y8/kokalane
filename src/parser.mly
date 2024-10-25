%{
    open Syntax
%}

%token ELIF ELSE FN FUN IF RETURN THEN VAL VAR
%token <string>STRING
%token <int>INT
%token <string>IDENT
%token BANG OR AND
%token PLUS MINUS TIMES DIV MOD IDIV LEQ GEQ EQ DIF ASS WAL ARR DPLUS
%token LPAR RPAR LCUR RCUR LANG RANG LSQU RSQU
%token SCOL DOT DCOL COMMA EOF
%token TRUE FALSE
%start file
%type <decl list> file

%nonassoc WAL
%left PLUS DPLUS MINUS
%left TIMES

%%

lit:
    i=INT { LInt i }
  | s=STRING { LString s }
  | TRUE { LBool true }
  | FALSE { LBool false }
  | LPAR RPAR { LUnit }
;

atype:
    t=IDENT { TCon t }
  | v=IDENT LANG t=ty RANG { TApp (v, t) }
  | LPAR t=ty RPAR { t }
;

result:
    t=ty { ([], t) }
;

tlist_nonempty:
    t=ty { [t] }
  | t=ty COMMA l=tlist { t :: l }
;

(* cette règle ne considère que des listes de 0, 2 ou plus types (i.e. tout sauf
1) pour éviter des conflits avec le 3ème cas de atype *)
tlist: { [] } | t = ty COMMA l=tlist_nonempty { t :: l } ;

ty:
    t=atype { t }
  | t=atype ARR r=result { let e, t' = r in TFun ([t], t', e) }
  | LPAR t=tlist RPAR ARR r=result { let e, t' = r in TFun (t, t', e) }
;

atom:
    x=IDENT { Var x }
  | f=atom LPAR l=separated_list(COMMA, expr) RPAR { App(f, l) }
  | l=lit { Lit l }
  | LPAR e=expr RPAR { e }
  | x=atom DOT f=IDENT { App(Var f, [x]) }
  | LSQU l=separated_list(COMMA, expr) RSQU { Lst l }
;

bexpr:
    a=atom { a }
  | x=IDENT WAL e=bexpr { Wal (x, e) }
  | e1=bexpr TIMES e2=bexpr { Bop (e1, Mul, e2) }
  | e1=bexpr PLUS e2=bexpr { Bop (e1, Add, e2) }
  | e1=bexpr MINUS e2=bexpr { Bop (e1, Sub, e2) }
  | e1=bexpr DPLUS e2=bexpr { Bop (e1, Cat, e2) }
;

(* on crée cette règle pour éviter les conflits shift/reduce *)
rexpr:
    b=bexpr { b }
  | RETURN e=expr { Ret e }
  | FN fb=funbody { let arg, body = fb in Fun (arg, None, body) }
;

stmt:
    e=expr SCOL+ { SExpr e }
  | VAL x=IDENT ASS l=expr SCOL+ { SVal (x, l) }
  | VAR x=IDENT WAL l=expr SCOL+ { SVar (x, l) }
;

expr:
    e=rexpr { e }
  | LCUR SCOL* s=stmt* RCUR { Blk s }
;

arg:
    x=IDENT DCOL t=ty { x, t }
;

funbody:
  LPAR x=separated_list(COMMA, arg) RPAR e=expr { (x, e) }
;

decl:
  FUN name=IDENT fb=funbody SCOL+ { let arg, body = fb in { name ; arg ; body } }
;

file:
  SCOL* l = decl* EOF { l }
;
