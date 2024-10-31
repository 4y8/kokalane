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
     string = IDENT { {string; loc = $startpos, $endpos} }
;

atype_noloc:
  | t = IDENT { TCon t }
  | v = string_loc LANG t=ty RANG { TApp (v, t) }
;

atype:
  | ty = atype_noloc { {ty; loc = $startpos, $endpos} }
  | LPAR t = ty RPAR { t }
;

result:
  | t = ty { ([], t) }
  | LANG l = separated_list(COMMA, string_loc) RANG t = ty { (l, t) }
;

(* cette règle ne considère que des listes de 0, 2 ou plus types (i.e. tout sauf
1) pour éviter des conflits avec le 3ème cas de atype *)
tlist:
  | { [] }
  | t=ty COMMA l=separated_nonempty_list(COMMA, ty) { t :: l }
;

ty_noloc:
  | t = atype ARR r = result { let e, t' = r in TFun ([t], t', e) }
  | LPAR t = tlist RPAR ARR r = result { let e, t' = r in TFun (t, t', e) }
;

ty:
  | t = atype { t }
  | ty = ty_noloc { {ty; loc = $startpos, $endpos} }
;

let var :=
    loc_expr (~=IDENT; <Var>)

let atom :=
  | LPAR; e = expr; RPAR; <>
  | e = var; <>
  | loc_expr(
      | ~ = lit; <Lit>
      | LSQU; ~ = separated_list(COMMA, expr); RSQU; <Lst>
      | ~ = atom; LPAR; ~ = separated_list(COMMA, expr); RPAR; <App>
  )

let atom_app(expr) :=
  | ~ = atom; <>
  | loc_expr(a = atom; f = fn(expr); { add_app a f })

mul_op:
  | TIMES { Mul }
  | DIV { Div }
  | MOD { Mod }
;

let mul_expr :=
  | ~ = atom; <>
  | loc_expr(~ = mul_expr; ~ = mul_op; ~ = atom; <Bop>)

add_op:
  | PLUS { Add }
  | MINUS { Sub }
  | DPLUS { Cat }
;

let add_expr :=
  | ~ = mul_expr; <>
  | loc_expr(~ = add_expr; ~ = add_op; ~ = mul_expr; <Bop>)

let fin_add_expr(expr) :=
  | ~ = fin_mul_expr(expr); <>
  | loc_expr(~ = add_expr; ~ = add_op; ~ = fin_mul_expr(expr); <Bop>)

let fin_mul_expr(expr) :=
  | ~ = atom_app(expr); <>
  | ~ = fn(expr); <>
  | ~ = return(expr); <>
  | loc_expr(~ = mul_expr; ~ = mul_op; ~ = atom_app(expr); <Bop>)

let fn(expr) :=
    loc_expr(
      FN; f = funbody(expr); { let x, b = f in Fun (x, None, b) }
  )

let return(expr) :=
    loc_expr(
      RETURN; ~ = expr; <Ret>
  )

let wal_expr(expr) :=
  | ~ = fin_add_expr(expr); <>
  | loc_expr(~ = string_loc; WAL; ~ = expr; <Wal>)

let no_dangling_expr :=
  | ~ = wal_expr(no_dangling_expr); <>
  | ~ = block; <>
  | loc_expr(IF; ~ = hi_expr; THEN; t = no_dangling_expr; ELSE; f = no_dangling_expr; <If>)

let if_expr :=
  | ~ = wal_expr(expr); <>
  | loc_expr(
    | IF; ~ = hi_expr; THEN; ~ = no_dangling_expr; ELSE; ~ = expr; <If>
    | IF; c = hi_expr; THEN; e = expr; { If (c, e, empty_block) }
  )

hi_expr:
  | e = if_expr { e }
;

expr:
  | e = hi_expr { e }
  | e = block { e }
;

stmt_noloc:
  | e = expr SCOL+ { SExpr e }
  | VAL x = IDENT ASS l = expr SCOL+ { SVal (x, l) }
  | VAR x = IDENT WAL l = expr SCOL+ { SVar (x, l) }
;

stmt:
    stmt = stmt_noloc { {stmt; loc = $startpos, $endpos} }
;

let block :=
    loc_expr(LCUR; SCOL*; ~ = stmt*; RCUR; <Blk>)

arg:
    x = string_loc DCOL t = ty { x, t }
;

let funbody(expr) :=
    LPAR; x = separated_list(COMMA, arg); RPAR; e = expr; <>

decl:
    FUN name = string_loc fb = funbody(expr) SCOL+
    { let arg, body = fb in { name; arg; body; res = None } }
;

file:
  SCOL* l = decl* EOF { l }
;
