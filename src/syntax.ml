type op =
    Add | Sub | Mul | Div | Mod | And | Or | Leq | Geq | Eq | Dif
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

module Effect : sig
  type t = EDiv | EConsole
  val compare : t -> t -> int
end = struct
  type t = EDiv | EConsole
  let compare = compare
end
module ESet = Set.Make(Effect)
module EMap = Map.Make(Effect)

type constr = bool option ref list EMap.t

(* cette paramétrisation permet d'utiliser le même type avec ou sans les
   annotations de position : 'a désigne le type des types, 'b celui des
   constructeurs de type et 'c celui d'un ensemble d'effet *)
type ('a, 'b, 'c) ty
  = TCon of string | TApp of 'b * 'a
  | TFun of 'a list * 'a * 'c | TVar of 'a tvar ref
[@@deriving show]
and 'a tvar =
  TVUnbd of int | TVLink of 'a

type type_loc =
  {ty : (type_loc, string_loc, string_loc list) ty; loc : loc * loc [@printer fun fmt t -> ()]}
[@@deriving show]

type type_pure = (('a [@printer pp_type_pure], string, ESet.t * constr [@printer fun fmt t -> ()]) ty) as 'a
[@@deriving show]

type ('a, 'b, 'c, 'd) expr
  = If  of 'a * 'a * 'a
  | Bop of 'a * op * 'a
  | Ret of 'a
  | Var of string
  | Lit of lit
  | App of 'a * 'a list
  | Wal of 'b * 'a
  | Fun of ('b * 'd) list * 'd option * 'a
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
  {expr : (expr_type, string, stmt_type, type_pure) expr ; ty : type_pure}

and stmt_type = expr_type stmt
[@@deriving show]

type ('a, 'b, 'c, 'd) decl =
  { name : 'd ; arg : ('d * 'b) list; res : 'c
  ; body : 'a }
[@@deriving show]

type decl_loc = (expr_loc, type_loc, type_loc option, string_loc) decl
[@@deriving show]

type decl_type = (expr_type, type_pure, type_pure, string) decl
[@@deriving show]

let is_arith_op = function
    Add | Sub | Mul | Div | Mod -> true
  | _ -> false
