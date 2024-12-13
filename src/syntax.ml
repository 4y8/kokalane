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

type surface_type_desc
  = STCon of string | STApp of string_loc * surface_type
  | STFun of surface_type list * surface_type * string_loc list
and surface_type = { stype : surface_type_desc ; loc : loc * loc }

type pure_type
  = TCon of string | TApp of string * pure_type
  | TFun of pure_type list * pure_type * (ESet.t * bool option ref option)
  | TVar of tvar ref
and tvar = TVUnbd of int | TVLink of pure_type

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

(* on utilise des GADTs pour le cas CheckPredicate qui permet de différer
   certains tests (par exemple ceux s'apparentant à des classes de types, comme
   print, pour lesquels on attend la fin de toutes les unifications) *)

type typed_desc
  = If  : typed_expr * typed_expr * typed_expr -> typed_desc
  | Bop : typed_expr * bop * typed_expr -> typed_desc
  | Ret : typed_expr -> typed_desc
  | Var : string -> typed_desc
  | Lit : lit -> typed_desc
  | App : typed_expr * typed_expr list -> typed_desc
  | Wal : string * typed_expr -> typed_desc
  | Fun : (string * pure_type) list * typed_expr -> typed_desc
  | Blk : typed_stmt list -> typed_desc
  | Lst : typed_expr list -> typed_desc
  | Uop : uop * typed_expr -> typed_desc
  | CheckPredicate : 'b * ('b -> typed_expr) -> typed_desc
and typed_expr
  = { expr : typed_desc; ty : pure_type }

and typed_stmt
  = TExpr of typed_expr
  | TDVar of string * typed_expr
  | TDVal of string * typed_expr

type variable
  = VGlo of string
  | VLoc of int * bool
  | VClo of int * bool

type annot_desc
  = AIf  of annot_expr * annot_expr * annot_expr
  | ABop of annot_expr * bop * annot_expr
  | ARet of annot_expr
  | AVar of variable
  | ALit of lit
  | AApp of annot_expr * annot_expr list
  | AWal of variable * annot_expr
  | AClo of variable list * string
  | ABlk of annot_stmt list
  | ALst of annot_expr list
  | AUop of uop * annot_expr

and annot_expr
  = { aexpr : annot_desc; aty : pure_type }

and annot_stmt
  = AExpr of annot_expr
  | ADVar of int * annot_expr
  | ADVal of int * annot_expr

type ('a, 'b, 'c, 'd) decl =
  { name : 'd ; arg : ('d * 'b) list; res : 'c
  ; body : 'a }

type surface_decl = (surface_expr, surface_type, result option, string_loc) decl

type decl_type = (typed_expr, typed_expr, pure_type, string) decl
