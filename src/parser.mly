%{
    open Syntax
    let empty_block = {expr = Blk []; loc = Lexing.dummy_pos, Lexing.dummy_pos}
    let add_app {expr; loc} x =
      match expr with
        App (g, y) -> App (g, y @ [x])
      | e -> App ({expr; loc}, [x])
%}

%token ELSE FN FUN IF RETURN THEN VAL VAR
%token <string>STRING
%token <int>INT
%token <string>IDENT
%token OR AND
%token PLUS MINUS TIMES DIV MOD LEQ GEQ EQ DIF ASS WAL ARR DPLUS
%token LPAR RPAR LCUR RCUR LANG RANG LSQU RSQU
%token SCOL DOT DCOL COMMA EOF
%token BANG TILDE
%token TRUE FALSE
%token DUMMY
%start file
%type <decl_loc list> file
(* on se restreint à n'utiliser que peu les directives d'associativités car
elles obfusquent les messages d'erreur, on peut trouver au commit #af5f189 une
grammaire LR(1) sans directives pour petit koka *)
%left OR
%left AND
%left EQ RANG LANG LEQ GEQ DIF
%left PLUS MINUS DPLUS
%left TIMES DIV MOD

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
  | t=ty COMMA l=separated_nonempty_list(COMMA, ty) { t :: l }
;

ty_noloc:
  | t = atype ARR r = result { let e, t' = r in TFun ([t], t', e) }
  | LPAR t = tlist RPAR ARR r = result { let e, t' = r in TFun (t, t', e) }
  | LPAR RPAR { TCon "unit" }
  | LPAR RPAR ARR r = result { let e, t' = r in TFun ([], t', e) }
;

ty:
  | t = atype { t }
  | ty = ty_noloc { {ty; loc = $startpos, $endpos} }
;

let var := loc_expr (~=IDENT; <Var>)

let atom :=
  | LPAR; e = expr; RPAR; <>
  | e = var; <>
  | loc_expr(
      | ~ = lit; <Lit>
      | LSQU; ~ = separated_list(COMMA, expr); RSQU; <Lst>
      | ~ = atom; LPAR; ~ = separated_list(COMMA, expr); RPAR; <App>
      | x = atom; DOT; f = var; { App (f, [x]) }
      | f = atom; b = block;
        { let b = {expr = Fun ([], None, b); loc = b.loc} in add_app f b }
  )

let expr_blk :=
  | ~ = block; <>
  | loc_expr(
    | IF; e = hi_expr; THEN; b = expr_blk; { If (e, b, empty_block) }
    | IF; e = hi_expr; THEN; t = no_dangling_expr; ELSE; f = expr_blk;
    { If (e, t, f) }
    | IF; e = hi_expr; r = return(block); { If (e, r, empty_block) }
  )
  | ~ = return(block); <>

let atom_app_blk :=   
  | loc_expr(a = atom; f = fn(expr_blk); { add_app a f })
  | loc_expr(a = atom_app_blk; f = fn(expr_blk); { add_app a f })

let atom_app(expr) :=
  | ~ = atom; <>
  | loc_expr(a = atom; f = fn(expr); { add_app a f })
  | loc_expr(a = atom_app_blk; f = fn(expr); { add_app a f })

un_op:
  | BANG { Not }
  | TILDE { Neg }
;

let un_expr :=
  | loc_expr(~ = un_op; ~ = atom; <Uop>)
  | ~ = atom; <>

let op ==
  | TIMES; { Mul }
  | DIV; { Div }
  | MOD; { Mod }
  | PLUS; { Add }
  | MINUS; { Sub }
  | DPLUS; { Cat }
  | LANG; { Lt }
  | LEQ; { Leq }
  | RANG; { Gt }
  | GEQ; { Geq }
  | EQ; { Eq }
  | DIF; { Dif }
  | OR; { Or }
  | AND; { And }

let bop_expr :=
  | ~ = un_expr; <>
  | loc_expr(l = bop_expr; o = op; r = bop_expr; <Bop>)

let fin_expr(expr) :=
  | ~ = atom_app(expr); <>
  | ~ = fn(expr); <>
  | ~ = return(expr); <>
  | loc_expr(~ = un_op; ~ = atom; <Uop>)
  | loc_expr(~ = bop_expr; ~ = op; ~ = fin_expr(expr); <Bop>)

let fn(expr) ==
    loc_expr(FN; f = funbody(expr); { let x, r, b = f in Fun (x, r, b) })

let return(expr) := loc_expr(RETURN; ~ = expr; <Ret>)

let wal_expr(expr) :=
  | ~ = fin_expr(expr); <>
  | loc_expr(~ = string_loc; WAL; ~ = expr; <Wal>)

let no_dangling_expr :=
  | ~ = wal_expr(no_dangling_expr); <>
  | ~ = block; <>
  | loc_expr(
      IF; ~ = hi_expr; THEN; t = no_dangling_expr; ELSE; f = no_dangling_expr;
          <If>)

let if_expr :=
  | ~ = wal_expr(expr); <>
  | loc_expr(
    | IF; ~ = hi_expr; THEN; ~ = no_dangling_expr; ELSE; ~ = expr; <If>
    | IF; c = hi_expr; THEN; e = expr; { If (c, e, empty_block) }
    | IF; c = hi_expr; r = return(expr); { If (c, r, empty_block) }
  )

hi_expr:
    e = if_expr { e }
;

expr:
  | e = hi_expr { e }
  | e = block { e }
;

stmt_noloc:
  | VAL x = IDENT ASS l = expr SCOL+ { SVal (x, l) }
  | VAR x = IDENT WAL l = expr SCOL+ { SVar (x, l) }
;

sexpr:
  | e = expr SCOL+ { {stmt = SExpr e; loc = $startpos, $endpos} }
;

stmt:
  | stmt = stmt_noloc { {stmt; loc = $startpos, $endpos} }
  | s = sexpr { s }
;

stmt_list:
  | s = sexpr RCUR { [s] }
  | s = stmt l = stmt_list { s :: l }
;

let block :=
    loc_expr(
    | LCUR; SCOL*; l = stmt_list; { Blk l }
    | LCUR; SCOL*; RCUR; {Blk []}
  )

ann:
    DCOL r = result { r }
;

arg:
    x = string_loc DCOL t = ty { x, t }
;

let funbody(expr) :=
    LPAR; x = separated_list(COMMA, arg); RPAR; ~ = option(ann); e = expr; <>

decl:
    FUN name = string_loc fb = funbody(expr) SCOL+
    { let arg, res, body = fb in { name; arg; body; res } }
;

file:
    SCOL* l = decl* EOF { l }
;
