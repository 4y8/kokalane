type op =
    Add | Sub | Mul | Div | Idiv | Mod | And | Or | Leq | Geq | Eq | Dif
  | Gt | Lt
[@@deriving show]

type lit =
  LUnit | LInt of int | LBool of bool | LString of string
[@@deriving show]

type eff = EDiv | EConsole
[@@deriving show]

type ty =
    TUnit | TBool | TInt | TString | TList of ty
  | TMaybe of ty | TFun of ty list * ty * eff list
[@@deriving show]

type expr
  = If  of expr * expr * expr
  | Bop of expr * op * expr
  | Ret of expr
  | Var of string
  | Lit of lit
  | App of expr * expr list
  | Wal of string * expr
  | Fun of (string * ty) list * ty option * expr
  | Blk of stmt list
  | Lst of expr list

and stmt
  = SExpr of expr
  | SVar of string * expr
  | SVal of string * expr
[@@deriving show]

type decl = { name : string ; arg : (string * ty) list ; body : expr }
[@@deriving show]
