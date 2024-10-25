type op =
    Add | Sub | Mul | Div | Idiv | Mod | And | Or | Leq | Geq | Eq | Dif | Gt | Lt
[@@deriving show]

type lit =
  Unit | Int of int | Bool of bool | String of string
[@@deriving show]

type eff = EDiv | EConsole
[@@deriving show]

type ty =
    TUnit | TBool | TInt | TString | TList of ty
  | TMaybe of ty | TFun of ty list * ty * eff list
[@@deriving show]

type expr
  = If  of expr * expr list * expr list
  | Bop of expr * op * expr
  | Ret of expr list
  | Var of string
  | Lit of lit
  | App of expr * expr list
  | Wal of string * expr
  | Fun of (string * ty) list * ty option * expr list
  | Blk of stmt list

and stmt
  = SExpr of expr
  | SVar of string * expr
  | SVal of string * expr
[@@deriving show]

type decl = { name : string ; arg : (string * ty) list ; body : expr }
[@@deriving show]
