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

let loc_expr(expr) ==
  ~ = expr; { { expr; loc = $startpos, $endpos } }

let lit :=
  | ~ = INT; <LInt>
  | ~ = STRING; <LString>
  | TRUE; { LBool true }
  | FALSE; { LBool false }
  | LPAR; RPAR; { LUnit }

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
  | { [] }
  | t=ty COMMA l=separated_nonempty_list(COMMA, ty) { t :: l }
;

ty_noloc:
  | t=atype ARR r=result { let e, t' = r in TFun ([t], t', e) }
  | LPAR t=tlist RPAR ARR r=result { let e, t' = r in TFun (t, t', e) }
;

ty:
  | t=atype { t }
  | ty=ty_noloc { {ty; loc = $startpos, $endpos} }
;

var:
    v=IDENT { {expr = Var v; loc = $startpos, $endpos} }
;

let atom :=
  | LPAR; e = expr; RPAR; <>
  | e = var; <>
  | loc_expr(~ = lit; <Lit>)

let atom_app :=
  | ~ = atom; <>
  | loc_expr(a = atom; f = fn; { add_app a f })

mul_op:
  | TIMES { Mul }
  | DIV { Div }
  | MOD { Mod }
;

let mul_expr :=
  | ~ = atom; <>
  | loc_expr(
      ~ = mul_expr; ~ = mul_op; ~ = atom; <Bop>
  )

add_op:
  | PLUS { Add }
  | MINUS { Sub }
;

let add_expr :=
  | ~ = mul_expr; <>
  | loc_expr(
      ~ = add_expr; ~ = add_op; ~ = mul_expr; <Bop>
  )

let fin_add_expr :=
  | ~ = fin_mul_expr; <>
  | loc_expr(
      ~ = add_expr; ~ = add_op; ~ = fin_mul_expr; <Bop>
  )

let fin_mul_expr :=
  | ~ = atom_app; <>
  | loc_expr (
      ~ = mul_expr; ~ = mul_op; ~ = atom_app; <Bop>
  )

let fn :=
    loc_expr(
       FN; f = funbody; { let x, b = f in Fun (x, None, b) }
  )

fn_expr:
    e=fn { e }
  | e=fin_add_expr { e }
;

expr:
    e=fn_expr { e }
  | e=block { e }
;

stmt_noloc:
    e=expr SCOL+ { SExpr e }
  | VAL x=IDENT ASS l=expr SCOL+ { SVal (x, l) }
  | VAR x=IDENT WAL l=expr SCOL+ { SVar (x, l) }
;

stmt:
   stmt=stmt_noloc { {stmt; loc = $startpos, $endpos} }
;

block_noloc:
   LCUR SCOL* l=stmt* RCUR { Blk l }
;

block:
   expr=block_noloc { {expr; loc = $startpos, $endpos} }
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
