type op =
    Add | Sub | Mul | Div | Idiv | Mod | And | Or | Leq | Geq | Eq | Dif
  | Gt | Lt | Cat
[@@deriving show]

type lit =
  LUnit | LInt of int | LBool of bool | LString of string
[@@deriving show]

type loc = [%import: Lexing.position] [@@deriving show]

type stringpos = { string : string ; loc : loc * loc }
[@@deriving show]

type 'a ty  =
    TCon of string | TApp of stringpos * 'a | TFun of 'a list * 'a * stringpos list
and typos =
  {ty : typos ty; loc : loc * loc [@printer fun fmt t -> ()]}
[@@deriving show]

type 'a expr
  = If  of 'a * 'a * 'a
  | Bop of 'a * op * 'a
  | Ret of 'a
  | Var of string
  | Lit of lit
  | App of 'a * 'a list
  | Wal of string * 'a
  | Fun of (string * typos) list * typos option * 'a
  | Blk of stmtpos list
  | Lst of 'a list
and exprpos =
  {expr : exprpos expr; loc : loc * loc [@printer fun fmt t -> ()]}
[@@deriving show]

and stmt
  = SExpr of exprpos
  | SVar of string * exprpos
  | SVal of string * exprpos
and stmtpos =
  {stmt : stmt; loc : loc * loc [@printer fun fmt t -> ()]}
[@@deriving show]

type decl = { name : string ; arg : (string * typos) list ; body : exprpos }
[@@deriving show]
