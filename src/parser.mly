%{
    open Syntax
    let empty_block = {expr = Blk []; loc = Lexing.dummy_pos, Lexing.dummy_pos}
    let add_app {expr; loc} x = match expr with App (g, y) -> App (g, y @ [x]) | e -> App ({expr; loc}, [x])
%}

%token ELIF ELSE FN FUN IF RETURN THEN VAL VAR
%token <string>STRING
%token <int>INT
%token <string>IDENT
%token BANG OR AND
%token PLUS MINUS TIMES DIV MOD LEQ GEQ EQ DIF ASS WAL ARR DPLUS
%token LPAR RPAR LCUR RCUR LANG RANG LSQU RSQU
%token SCOL DOT DCOL COMMA EOF
%token TRUE FALSE
%start file
%type <decl_loc list> file

%%

lit:
    i=INT { LInt i }
  | s=STRING { LString s }
  | TRUE { LBool true }
  | FALSE { LBool false }
  | LPAR RPAR { LUnit }
;

string_loc:
     string=IDENT { {string; loc = $startpos, $endpos} }
;

atype_noloc:
    t=IDENT { TCon t }
  | v=string_loc LANG t=ty RANG { TApp (v, t) }
;

atype:
    ty=atype_noloc { {ty; loc = $startpos, $endpos} }
  | LPAR t=ty RPAR { t }
;

result:
    t=ty { ([], t) }
  | LANG l=separated_list(COMMA, string_loc) RANG t=ty { (l, t) } 
;

(* cette règle ne considère que des listes de 0, 2 ou plus types (i.e. tout sauf
1) pour éviter des conflits avec le 3ème cas de atype *)
tlist:
   { [] }
  | t=ty COMMA l=separated_nonempty_list(COMMA, ty) { t :: l }
;

ty_noloc:
    t=atype ARR r=result { let e, t' = r in TFun ([t], t', e) }
  | LPAR t=tlist RPAR ARR r=result { let e, t' = r in TFun (t, t', e) }
;

ty:
    t=atype { t }
  | ty=ty_noloc { {ty; loc = $startpos, $endpos} }
;

var:
  v=IDENT { {expr = Var v; loc = $startpos, $endpos} }
;

atom_noloc:
    l=lit { Lit l }
  | x=atom DOT f=var { App(f, [x]) }
;

atom:
    expr=atom_noloc { {expr; loc = $startpos, $endpos} }
  | LPAR e=expr RPAR { e }
  | e=var { e }
;

mul_expr_noloc:
    e1=mul_expr TIMES e2=atom { Bop (e1, Mul, e2) }
  | e1=mul_expr DIV e2=atom { Bop (e1, Div, e2) }
  | e1=mul_expr MOD e2=atom { Bop (e1, Mod, e2) }
;

mul_expr:
    expr=mul_expr_noloc { {expr; loc = $startpos, $endpos} }
  | a=atom { a }
;

add_expr_noloc:
    e1=add_expr PLUS e2=mul_expr { Bop (e1, Add, e2) }
  | e1=add_expr MINUS e2=mul_expr { Bop (e1, Sub, e2) }
;

add_expr:
    expr=add_expr_noloc { {expr; loc = $startpos, $endpos} }
  | e=mul_expr { e }
;

expr:
  e=add_expr { e }
;

stmt_noloc:
    e=expr SCOL+ { SExpr e }
  | VAL x=IDENT ASS l=expr SCOL+ { SVal (x, l) }
  | VAR x=IDENT WAL l=expr SCOL+ { SVar (x, l) }
;

stmt:
   stmt=stmt_noloc { {stmt; loc = $startpos, $endpos} }
;

arg:
    x=string_loc DCOL t=ty { x, t }
;

funbody:
  LPAR x=separated_list(COMMA, arg) RPAR e=expr { (x, e) }
;

decl:
  FUN name=string_loc fb=funbody SCOL+
    { let arg, body = fb in { name; arg; body; res = None } }
;

file:
  SCOL* l = decl* EOF { l }
;
