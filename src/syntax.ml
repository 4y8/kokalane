type bop =
    Add | Sub | Mul | Div | Mod | And | Or | Leq | Geq | Eq | Dif
  | Gt | Lt | Cat

type uop = Neg | Not

type lit =
    LUnit | LInt of int | LBool of bool | LString of string

type loc = Lexing.position

type string_loc = { string : string ; loc : loc * loc }

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

type surface_type_desc
  = STCon of string | STApp of string_loc * surface_type
  | STFun of surface_type list * surface_type * string_loc list
and surface_type = { stype : surface_type_desc ; loc : loc * loc }

type pure_type
  = TCon of string | TApp of string * pure_type
  | TFun of pure_type list * pure_type * (ESet.t * bool option ref option)
  | TVar of tvar ref
and tvar =
    TVUnbd of int | TVLink of pure_type

type result = string_loc list * surface_type

type surface_desc
  = SIf  of surface_expr * surface_expr * surface_expr
  | SBop of surface_expr * bop * surface_expr
  | SRet of surface_expr
  | SVar of string
  | SLit of lit
  | SApp of surface_expr * surface_expr list
  | SWal of string_loc * surface_expr
  | SFun of (string_loc * surface_type) list * result option * surface_expr
  | SBlk of surface_stmt list
  | SLst of surface_expr list
  | SUop of uop * surface_expr
and surface_expr =
  { sexpr : surface_desc; loc : loc * loc }

and surface_stmt_desc
  = SExpr of surface_expr
  | SDVar of string * surface_expr
  | SDVal of string * surface_expr
and surface_stmt =
  {stmt : surface_stmt_desc; loc : loc * loc}

type typed_desc
  = If  of typed_expr * typed_expr * typed_expr
  | Bop of typed_expr * bop * typed_expr
  | Ret of typed_expr
  | Var of string
  | Lit of lit
  | App of typed_expr * typed_expr list
  | Wal of string * typed_expr
  | Fun of (string * pure_type) list * typed_expr
  | Blk of typed_stmt list
  | Lst of typed_expr list
  | Uop of uop * typed_expr
  | Println of typed_expr * string

and typed_expr
  = { expr : typed_desc; ty : pure_type }

and typed_stmt
  = TExpr of typed_expr
  | TDVar of string * typed_expr
  | TDVal of string * typed_expr

type ('a, 'b, 'c, 'd) decl =
  { name : 'd ; arg : ('d * 'b) list; res : 'c
  ; body : 'a }

type surface_decl = (surface_expr, surface_type, result option, string_loc) decl

type decl_type = (typed_expr, typed_expr, pure_type, string) decl
