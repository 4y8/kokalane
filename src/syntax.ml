type op =
    Add | Sub | Mul | Div | Idiv | Mod | And | Or | Leq | Geq | Eq | Dif
  | Gt | Lt | Cat

type lit =
  LUnit | LInt of int | LBool of bool | LString of string

type eff = EDiv | EConsole

type ty =
    TCon of string | TApp of string * typos | TFun of typos list * typos * string list
and typos =
  {ty : ty; loc : Lexing.position * Lexing.position}

type expr
  = If  of exprpos * exprpos * exprpos
  | Bop of exprpos * op * exprpos
  | Ret of exprpos
  | Var of string
  | Lit of lit
  | App of exprpos * exprpos list
  | Wal of string * exprpos
  | Fun of (string * typos) list * typos option * exprpos
  | Blk of stmtpos list
  | Lst of exprpos list
and exprpos =
  {expr : expr; loc : Lexing.position * Lexing.position}

and stmt
  = SExpr of exprpos
  | SVar of string * exprpos
  | SVal of string * exprpos
and stmtpos =
  {stmt : stmt; loc : Lexing.position * Lexing.position}

type decl = { name : string ; arg : (string * typos) list ; body : exprpos }
