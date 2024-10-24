type op =
    Add | Sub | Mul | Div | Idiv | Mod| And | Or | Leq | Geq | Eq | Dif | Gt | Lt

type lit =
  Unit | Inf of int | Bool of bool | String of string

type eff = EDiv | EConsole

type ty =
    TUnit | TBool | TInt | TString | TList of ty
  | TMaybe of ty | TFun of ty list * ty * eff list

type expr
  = If  of expr * expr list * expr list
  | Bop of expr * op * expr
  | Ret of expr list
  | Var of string
  | Lit of lit
  | App of expr * expr list
  | Wal of string * expr
  | Fun of (string * ty) list * ty option * expr list

type stmt
  = SExpr of expr
  | SVar of string * expr list
  | SVal of string * expr list

type decl = { name : string ; arg : (string * ty) list ; body : expr list }
