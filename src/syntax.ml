type op =
    Add | Sub | Mul | Div | Idiv | Mod | And | Or | Leq | Geq | Eq | Dif
  | Gt | Lt | Cat
[@@deriving show]

type lit =
  LUnit | LInt of int | LBool of bool | LString of string
[@@deriving show]

type loc = [%import: Lexing.position] [@@deriving show]

type ty =
    TCon of string | TApp of string * typos | TFun of typos list * typos * string list
and typos =
  {ty : ty; loc : loc * loc}
[@@deriving show]

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
  {expr : expr; loc : loc * loc}
[@@deriving show]

and stmt
  = SExpr of exprpos
  | SVar of string * exprpos
  | SVal of string * exprpos
and stmtpos =
  {stmt : stmt; loc : loc * loc}
[@@deriving show]

type decl = { name : string ; arg : (string * typos) list ; body : exprpos }
[@@deriving show]
