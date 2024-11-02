open Effect
open Syntax
open Syntax.Effect
open Format

type ctx =
  { var : (type_pure * bool) SMap.t; ret_type : type_pure; rec_fun : string }

let valid_types =
  ["int", 0; "bool", 0; "unit", 0; "string", 0; "list", 1; "maybe", 1]
  |> List.to_seq |> SMap.of_seq

let int = TCon "int"
let unit = TCon "unit"
let bool = TCon "bool"
let string = TCon "string"
let list t = TApp ("list", t)
let maybe t = TApp ("maybe", t)

let builtin_fun =
  ["println"; "repeat"; "while"; "default"; "for"; "head"; "tail"]
  |> SSet.of_list

let is_builtin_fun {expr; loc} = match expr with
    Var x when SSet.mem x builtin_fun -> x
  | _ -> ""

let rec check_prop_type loc error pr = function
    TVar r as t ->
      begin match !r with
        TVLink t -> check_prop_type loc error pr t
      | _ -> error t
      end
  | t -> if not (pr t) then error t

let check_printable loc =
  let pr = function
      TCon "unit" | TCon "bool" | TCon "int" | TCon "string" -> true
    | _ -> false
  in
  let error t =
    print_endline (show_type_pure t);
    Error.error loc (fun fmt ->
        Format.fprintf fmt "Tried to print %a which can't be printed"
          Pprint.fmt_type t)
  in check_prop_type loc error pr

let rec check_concatenable loc =
  let pr = function
      TCon "string" | TApp ("list", _) -> true
    | _ -> false
  in
  let error t = Error.error loc (fun fmt ->
      fprintf fmt "Tried to concatenate %a which is can't be \
                   concatenated" Pprint.fmt_type t)
  in check_prop_type loc error pr

let check_comparable loc =
  let pr = (=) (TCon "int") in
  let error t = Error.error loc (fun fmt ->
      fprintf fmt "Tried to compare %a which can't be compared"
        Pprint.fmt_type t)
  in
  check_prop_type loc error pr

let check_equalable loc =
  let pr = function
      TCon "int" | TCon "bool" | TCon "string" -> true
    | _ -> false
  in
  let error t = Error.error loc (fun fmt ->
      fprintf fmt "Tried to check equality for %a which can't be compared"
        Pprint.fmt_type t)
  in
  check_prop_type loc error pr

exception Occurs

let rec check_occurs x = function
    TCon _ -> ()
  | TApp (_, t) -> check_occurs x t
  | TFun (arg, res, _) ->
      List.iter (check_occurs x) arg;
      check_occurs x res
  | TVar r ->
      match !r with
        TVLink t -> check_occurs x t
      | TVUnbd n when n = x -> raise Occurs
      | _ -> ()

let rec eqtype  ?(check_effect=true) t t' = match t, t' with
    TCon s, TCon s' -> s = s'
  | TApp (f, t), TApp (f', t') -> f = f' && eqtype t t' ~check_effect
  | TFun (arg, res, eff), TFun (arg', res', eff') ->
      begin try
        if check_effect then
          unify_effset eff eff';
        List.for_all2 (eqtype ~check_effect) arg arg'
        && eqtype ~check_effect res res'
      with
        Invalid_argument _ -> false
      end
  | TVar r, t' ->
      begin match !r with
        TVLink t -> eqtype t t'
      | TVUnbd n ->
          check_occurs n t'; r := TVLink t'; true
      end
  | _, TVar _ -> eqtype t' t
  | _, _ -> false

let tvar = ref 0

let new_tvar () =
  incr tvar; TVar (ref (TVUnbd !tvar))

let rec erase_type {ty; loc} = match ty with
    TCon s ->
      begin match SMap.find_opt s valid_types with
        Some 0 -> TCon s
      | None ->
          Error.error_str loc @@ sprintf "Unknown type constructor: %s" s
      | Some n ->
          Error.error_str loc @@
          sprintf "Type constructor %s expected %d constructors, got 0"
            s n
      end
  | TApp ({string; loc}, t) ->
      begin match SMap.find_opt string valid_types with
        Some 1 -> TApp (string, erase_type t)
      | None ->
          Error.error_str loc @@
          sprintf "Unknown type constructor: %s" string
      | Some n ->
          Error.error_str loc @@
          sprintf "Type constructor %s expected %d constructors, got 1"
            string n
      end
  | TFun (l, t, e) ->
      TFun (List.map erase_type l, erase_type t, erase_effects e)
  | TVar _ -> failwith "impossible" (* les types fournis par l'utilisateur ne contiennent pas de
                    variable de type *)

let type_of_lit = function
    LInt _ -> int
  | LBool _ -> bool
  | LString _ -> string
  | LUnit -> unit

exception Polymorphism

let rec remove_tvar = function
    | TCon s -> TCon s, false
    | TApp (s, t) ->
        let t, has_free = remove_tvar t in
        TApp (s, t), has_free
    | TFun (arg, res, eff) ->
        let res, has_free = remove_tvar res in
        let arg, has_free' = List.map remove_tvar arg |> List.split in
        TFun (arg, res, eff), List.fold_left (||) has_free has_free'
    | TVar r ->
        match !r with
          TVLink t -> remove_tvar t
        | TVUnbd _ -> TVar r, true

let rec remove_tvar_expr {expr; ty} =
  let remove_tvar_stmt = function
      SExpr e -> SExpr (remove_tvar_expr e)
    | SVal (x, e) -> SVal (x, remove_tvar_expr e)
    | SVar (x, e) -> SVar (x, remove_tvar_expr e)
  in
  let expr = match expr with
      If (e, b1, b2) ->
        If (remove_tvar_expr e, remove_tvar_expr b1, remove_tvar_expr b2)
    | Bop (e, op, e') ->
        Bop (remove_tvar_expr e, op, remove_tvar_expr e')
    | Ret e ->
        Ret (remove_tvar_expr e)
    | Var x -> Var x
    | Lit l -> Lit l
    | App (f, x) -> App (remove_tvar_expr f, List.map remove_tvar_expr x)
    | Wal (x, e) -> Wal (x, remove_tvar_expr e)
    | Fun (x, (), e) -> Fun (x, (), remove_tvar_expr e)
    | Blk l -> Blk (List.map remove_tvar_stmt l)
    | Lst l -> Lst (List.map remove_tvar_expr l)
    | Uop (o, e) -> Uop (o, remove_tvar_expr e)
  in
  {expr; ty = fst (remove_tvar ty)}
