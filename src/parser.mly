%{
    open Syntax
    let empty_block = {expr = Blk []; loc = Lexing.dummy_pos, Lexing.dummy_pos}
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

%nonassoc THEN
%nonassoc ELSE
%nonassoc WAL
%left OR
%left AND
%nonassoc EQ DIF
%left PLUS DPLUS MINUS
%left TIMES MOD DIV

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

atype_nonpos:
    t=IDENT { TCon t }
  | v=string_loc LANG t=ty RANG { TApp (v, t) }
;

atype:
    ty=atype_nonpos { {ty; loc = $startpos, $endpos} }
  | LPAR t=ty RPAR { t }
;

result:
    t=ty { ([], t) }
  | LANG l=separated_list(COMMA, string_loc) RANG t=ty { (l, t) } 
;

tlist_nonempty:
    t=ty { [t] }
  | t=ty COMMA l=tlist { t :: l }
;

(* cette règle ne considère que des listes de 0, 2 ou plus types (i.e. tout sauf
1) pour éviter des conflits avec le 3ème cas de atype *)
tlist:
    { [] }
  | t = ty COMMA l=tlist_nonempty { t :: l }
;

ty_nonpos:
    t=atype ARR r=result { let e, t' = r in TFun ([t], t', e) }
  | LPAR t=tlist RPAR ARR r=result { let e, t' = r in TFun (t, t', e) }
;

ty:
    t=atype { t }
  | ty=ty_nonpos { {ty; loc = $startpos, $endpos} }
;

var:
  v=IDENT { {expr = Var v; loc = $startpos, $endpos} }
;

atom_nonpos:
    x=IDENT { Var x }
  | f=atom LPAR l=separated_list(COMMA, expr) RPAR { App(f, l) }
  | l=lit { Lit l }
  | x=atom DOT f=var { App(f, [x]) }
  | LSQU l=separated_list(COMMA, expr) RSQU { Lst l }
(* l'annotation de type permet de résoudre une ambigüité *)
;

atom:
    expr=atom_nonpos { {expr; loc = $startpos, $endpos} }
  | LPAR e=expr RPAR { e }
;

trailing_fn_noloc :
    a=atom f=fn { let a : expr_loc = a in match a.expr with App (g, x) -> App (g, x @ [f]) | e -> App (a, [f]) }
;

atom_fn:
   expr=trailing_fn_noloc { {expr; loc = $startpos, $endpos} }
;

bexpr_nonpos:
    x=string_loc WAL e=bexpr { Wal (x, e) }
  | e1=bexpr TIMES e2=bexpr { Bop (e1, Mul, e2) }
  | e1=bexpr DIV e2=bexpr { Bop (e1, Div, e2) }
  | e1=bexpr MOD e2=bexpr { Bop (e1, Mod, e2) }
  | e1=bexpr PLUS e2=bexpr { Bop (e1, Add, e2) }
  | e1=bexpr MINUS e2=bexpr { Bop (e1, Sub, e2) }
  | e1=bexpr DPLUS e2=bexpr { Bop (e1, Cat, e2) }
  | e1=bexpr OR e2=bexpr { Bop (e1, Or, e2) }
  | e1=bexpr AND e2=bexpr { Bop (e1, And, e2) }
  | e1=bexpr EQ e2=bexpr { Bop (e1, Eq, e2) }
  | e1=bexpr DIF e2=bexpr { Bop (e1, Dif, e2) }
;

ret_noloc:
    RETURN e=expr { Ret e }
;

ret:
    expr=ret_noloc { {expr; loc = $startpos, $endpos} }
;

bexpr:
    a=atom { a }
  | e=bexpr_nonpos { {expr=e; loc = $startpos, $endpos} }
;

fn_noloc:
  | FN fb=funbody { let arg, body = fb in Fun (arg, None, body) }
;

fn:
    expr=fn_noloc { {expr; loc = $startpos, $endpos} }
;

(* on crée cette règle pour éviter les conflits shift/reduce *)
rexpr_nonpos:
  | IF e=bexpr r=ret { If (e, r, empty_block) }
  | IF e=bexpr THEN t=expr { If(e, t, empty_block) }
  | IF e=bexpr THEN t=expr ELSE f=expr { If(e, t, f) }
;

rexpr:
    b=bexpr { b }
  | e=ret { e }
  | e=fn { e }
  | e=atom_fn { e }
  | e=rexpr_nonpos { {expr=e; loc = $startpos, $endpos} }
;

stmt_nonpos:
    e=expr SCOL+ { SExpr e }
  | VAL x=IDENT ASS l=expr SCOL+ { SVal (x, l) }
  | VAR x=IDENT WAL l=expr SCOL+ { SVar (x, l) }
;

stmt:
   stmt=stmt_nonpos { {stmt; loc = $startpos, $endpos} }
;

expr:
    e=rexpr { e }
  | LCUR SCOL* s=stmt* RCUR { {expr=Blk s; loc = $startpos, $endpos} }
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
