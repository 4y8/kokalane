type op =
    Add | Sub | Mul | Div | Idiv | Mod | And | Or | Leq | Geq | Eq | Dif
  | Gt | Lt | Cat
[@@deriving show]

type lit =
  LUnit | LInt of int | LBool of bool | LString of string
[@@deriving show]

type loc = [%import: Lexing.position] [@@deriving show]

type string_loc = { string : string ; loc : loc * loc }
[@@deriving show]

module SMap = Map.Make(String)
module SSet = Set.Make(String)

(* cette paramétrisation permet d'utiliser le même type avec ou sans les
   annotations de position : 'a désigne le type des types, 'b celui des
   constructeurs de type et 'c celui d'un ensemble d'effet *)
type ('a, 'b, 'c) ty  =
    TCon of string | TApp of 'b * 'a | TFun of 'a list * 'a * 'c
[@@deriving show]

type type_loc =
  {ty : (type_loc, string_loc, string_loc list) ty; loc : loc * loc [@printer fun fmt t -> ()]}
[@@deriving show]

type pure_type = (('a, string, SSet.t) ty) as 'a

type ('a, 'b, 'c, 'd) expr
  = If  of 'a * 'a * 'a
  | Bop of 'a * op * 'a
  | Ret of 'a
  | Var of string
  | Lit of lit
  | App of 'a * 'a list
  | Wal of 'b * 'a
  | Fun of (string * 'd) list * 'd option * 'a
  | Blk of 'c list
  | Lst of 'a list
and expr_loc =
  {expr : (expr_loc, string_loc, stmt_loc, type_loc) expr; loc : loc * loc [@printer fun fmt t -> ()]}

and 'a stmt
  = SExpr of 'a
  | SVar of string * 'a
  | SVal of string * 'a
and stmt_loc =
  {stmt : expr_loc stmt; loc : loc * loc [@printer fun fmt t -> ()]}
[@@deriving show]

type expr_type =
  {expr : (expr_type, string, stmt_type, pure_type) expr ; ty : pure_type}

and stmt_type = expr_type stmt

type decl = { name : string ; arg : (string * type_loc) list ; body : expr_loc }
[@@deriving show]

let is_arith_op = function
    Add | Sub | Mul | Div | Idiv | Mod -> true
  | _ -> false
