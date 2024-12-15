open Effect
open Syntax
open Syntax.Effect
open Format

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

let is_builtin_fun {sexpr; _} = match sexpr with
  | SVar x when SSet.mem x builtin_fun -> x
  | _ -> ""

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

let check_printable loc ({ty; _} as e) =
    match fst (remove_tvar ty) with
    | TCon (("unit" | "bool" | "int" | "string") as s) ->
       let pr_type = TFun ([ty], unit, NoRec (ESet.singleton EConsole)) in
       let print = { texpr = Var ("println_" ^ s) ; ty = pr_type } in
       { texpr = App (print, [e]) ; ty = unit }
    | _ ->
       Error.error loc (fun fmt ->
           Format.fprintf fmt "Tried to print %a which can't be printed"
             Pprint.fmt_type ty)

let check_concatenable loc ({ty; _} as e, e') =
  match fst (remove_tvar ty) with
  | TCon "string" ->
      let cat_type = TFun ([ty; ty], ty, NoRec ESet.empty) in
      let cat = { texpr = Var "strcat" ; ty = cat_type } in
      { texpr = App (cat, [e; e']) ; ty }
  | TApp ("list", _) ->
      let cat_type = TFun ([ty; ty], ty, NoRec ESet.empty) in
      let cat = { texpr = Var "lstcat" ; ty = cat_type } in
      { texpr = App (cat, [e; e']) ; ty }
  | _ ->
       Error.error loc (fun fmt ->
           fprintf fmt "Tried to concatenate %a which is can't be \
concatenated" Pprint.fmt_type ty)

let check_comparable loc ({ty; _} as e, op, e') =
  if fst (remove_tvar ty) = int then
    { texpr = Bop (e, op, e') ; ty = bool }
  else
    Error.error loc (fun fmt ->
      fprintf fmt "Tried to compare %a which can't be compared"
        Pprint.fmt_type ty)

let check_equalable loc ({ty; _} as e, op, e') =
  match fst (remove_tvar ty) with
  | TCon "int" | TCon "bool" ->
      { texpr = Bop (e, op, e') ; ty = bool}
  | TCon "string" ->
      let eq_type = TFun ([ty; ty], bool, NoRec ESet.empty) in
      let eq = { texpr = Var "streq" ; ty = eq_type } in
      { texpr = App (eq, [e; e']) ; ty = bool }
  | _ ->
      Error.error loc (fun fmt ->
          fprintf fmt "Tried to check equality for %a which can't be compared"
            Pprint.fmt_type ty)

exception Occurs

let rec check_occurs x = function
  | TCon _ -> ()
  | TApp (_, t) -> check_occurs x t
  | TFun (arg, res, _) ->
      List.iter (check_occurs x) arg;
      check_occurs x res
  | TVar r ->
      match !r with
        TVLink t -> check_occurs x t
      | TVUnbd n when n = x -> raise Occurs
      | _ -> ()

let rec eqtype env ?(check_effect=true) t t' = match t, t' with
  | TCon s, TCon s' -> s = s'
  | TApp (f, t), TApp (f', t') -> f = f' && eqtype env t t' ~check_effect
  | TFun (arg, res, eff), TFun (arg', res', eff') ->
      begin try
        (not check_effect || unify_effset env eff eff')
        && List.for_all2 (eqtype env ~check_effect) arg arg'
        && eqtype env ~check_effect res res'
      with
        Invalid_argument _ -> false
      end
  | TVar r, t' ->
      begin match !r with
        TVLink t -> eqtype env ~check_effect t t'
      | TVUnbd n ->
          check_occurs n t'; r := TVLink t'; true
      end
  | _, TVar _ -> eqtype env ~check_effect t' t
  | _, _ -> false

let tvar = ref 0

let new_tvar () =
  incr tvar; TVar (ref (TVUnbd !tvar))

let rec erase_type {stype; tloc} = match stype with
  | STCon s ->
      begin match SMap.find_opt s valid_types with
        Some 0 -> TCon s
      | None ->
          Error.error_str tloc @@ sprintf "Unknown type constructor: %s" s
      | Some n ->
          Error.error_str tloc @@
          sprintf "Type constructor %s expected %d constructors, got 0"
            s n
      end
  | STFun (l, t, e) ->
      TFun (List.map erase_type l, erase_type t, erase_effects e)
  | STApp ({string; strloc}, t) ->
      match SMap.find_opt string valid_types with
      | Some 1 -> TApp (string, erase_type t)
      | None ->
          Error.error_str strloc @@
          sprintf "Unknown type constructor: %s" string
      | Some n ->
          Error.error_str strloc @@
          sprintf "Type constructor %s expected %d constructors, got 1"
            string n

let type_of_lit = function
  | LInt _ -> int
  | LBool _ -> bool
  | LString _ -> string
  | LUnit -> unit
