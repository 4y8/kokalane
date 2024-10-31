open Syntax
open Syntax.Effect
open Format

type ctx =
  { var : (type_pure * bool) SMap.t; ret_type : type_pure; rec_fun : string
  ; rec_eff : bool EMap.t }

let valid_types =
  ["int", 0; "bool", 0; "unit", 0; "string", 0; "list", 1; "maybe", 1]
  |> List.to_seq |> SMap.of_seq

let valid_effects_list = ["div", EDiv; "console", EConsole]

let valid_effects =
  valid_effects_list |> List.to_seq |> SMap.of_seq

let builtin_fun =
  ["println"; "repeat"; "while"; "default"; "for"; "head"; "tail"]
  |> SSet.of_list

let is_builtin_fun {expr; loc} = match expr with
    Var x when SSet.mem x builtin_fun -> x
  | _ -> ""

let rec check_prop_type loc error pr = function
    TVar r as t->
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

exception Occurs
exception EffectUnification

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

let (++) (eff, constr) (eff', constr') =
  ESet.union eff eff', if constr = None then constr' else constr

let add_effect (eff, constr) e =
  ESet.add e eff, constr

let unify_effset ((eff, constr) as e) ((eff', constr') as e') =
  if ESet.mem EDiv eff && not ESet.(mem EDiv eff') || ESet.mem EDiv eff' && not ESet.(mem EDiv eff) then
    raise EffectUnification
  else
    let has_console (eff, constr) =
     ESet.mem EConsole eff || match constr with
        None -> false
      | Some r -> match !r with
                    None -> false
                  | Some b -> b
    in
    let unify_console (eff, constr) (eff', constr') =
      if has_console (eff, constr) && not ESet.(mem EConsole eff') then
        begin match constr' with
          None -> raise EffectUnification
        | Some r ->
           match !r with
             Some false -> raise EffectUnification
           | _ -> r := Some true
        end;
      if not ESet.(mem EConsole eff') then
        match constr with
          None -> ()
        | Some r ->
           match !r with
             Some true -> raise EffectUnification
           | _ -> r := Some false
    in
    unify_console e e';
    unify_console e' e
                                      
  
let rec eqtype t t' = match t, t' with
    TCon s, TCon s' -> s = s'
  | TApp (f, t), TApp (f', t') -> f = f' && eqtype t t'
  | TFun (arg, res, eff), TFun (arg', res', eff') ->
    begin try
        unify_effset eff eff';
        List.for_all2 eqtype arg arg' && eqtype res res'
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

let rec erase_type {ty; loc} = match ty with
    TCon s -> TCon s
  | TApp (s, t) -> TApp (s.string, erase_type t)
  | TFun (arg, res, eff) ->
     TFun (List.map erase_type arg, erase_type res,
           (List.map (fun s -> SMap.find s.string valid_effects) eff
           |> ESet.of_list, None))
  | TVar r ->
     match !r with
       TVLink ty -> TVar (ref (TVLink (erase_type ty)))
     | TVUnbd n -> TVar (ref (TVUnbd n))

let tvar = ref 0

let new_tvar () =
  incr tvar; TVar (ref (TVUnbd !tvar))

let check_valid_effect {string; loc} =
  if not SMap.(mem string valid_effects) then
    Error.error_str loc @@ sprintf "Unknown effect: %s" string

let rec check_valid_type {ty; loc} = match ty with
    TCon s ->
     begin match SMap.find_opt s valid_types with
       Some 0 -> ()
     | None ->
        Error.error_str loc @@ sprintf "Unknown type constructor: %s" s
     | Some n ->
        Error.error_str loc @@
          sprintf "Type constructor %s expected %d constructors, got 0"
            s n
     end
  | TApp ({string; loc}, t) ->
     begin match SMap.find_opt string valid_types with
       Some 1 -> check_valid_type t
     | None ->
        Error.error_str loc @@
          sprintf "Unknown type constructor: %s" string
     | Some n ->
        Error.error_str loc @@
          sprintf "Type constructor %s expected %d constructors, got 1"
            string n
     end
  | TFun (l, t, e) ->
    List.iter check_valid_type l; check_valid_type t;
    List.iter check_valid_effect e
  | TVar _ -> () (* les types fournis par l'utilisateur ne contiennent pas de
                    variable de type *)

let type_of_lit = function
    LInt _ -> TCon "int"
  | LBool _ -> TCon "bool"
  | LUnit -> TCon "unit"
  | LString _ -> TCon "string"

let rec infer ctx {expr; loc} = match expr with
    Lit l -> {expr=Lit l; ty =type_of_lit l}, (ESet.empty, None)
  | Var x ->
    begin match SMap.find_opt x ctx.var with
        Some (t, _) -> {expr=Var x; ty=t}, (ESet.empty, None)
      | None -> Error.unknown_var loc x
    end
  | Wal (x, e) ->
     begin match SMap.find_opt x.string ctx.var with
         None -> Error.unknown_var x.loc x.string
       | Some (_, false) ->
         Error.error_str loc (sprintf "Variable %s is immutable" x.string)
       | Some (t, true) ->
         let e, eff = check ctx t e in
         {expr=Wal(x.string, e); ty = TCon "unit"}, eff
     end
  | App (f, x) when is_builtin_fun f <> "" ->
     let s = is_builtin_fun f in
     begin match s, x with
       "println", [e] ->
        let e, eff = infer ctx e in
        check_printable loc e.ty;
        let pr_type = TFun ([e.ty], TCon "unit", (ESet.singleton EConsole, None)) in
        {expr=App({expr=Var s; ty = pr_type}, [e]); ty = TCon "unit"},
        add_effect eff EConsole
     | _ -> Error.error_str loc (sprintf "Function %s, got the wrong \
number of arguments" s) (* pourrait être mieux, en signalant le nombre
                           d'arguments attendu *)
     end
  | App (f, x) ->
     let f, eff = infer ctx f in
     begin match f.ty with
       TFun (arg, res, eff') ->
        (try
           let x, eff'' = List.map2 (check ctx) arg x |> List.split in
           {expr = App (f, x); ty = res},
           List.fold_left (++) (eff ++ eff') eff''
         with Invalid_argument _ ->
           Error.error_str loc (sprintf "Function expected %d \
arguments, got %d" (List.length arg) (List.length x)))
     | _ -> Error.error loc (fun fmt -> fprintf fmt "Exepected a \
function, got an expression of type %a" Pprint.fmt_type f.ty)
     end
  | Lst l ->
     let ty = new_tvar () in
     let l, eff = List.split (List.map (check ctx ty) l) in
     {expr = Lst l; ty = TApp ("list", ty)}, List.fold_left (++) (ESet.empty, None) eff
  | If (e, b1, b2) ->
     let e, eff = check ctx (TCon "bool") e in
     let b1, eff1 = infer ctx b1 in
     let b2, eff2 = check ctx b1.ty b2 in
     {expr = If (e, b1, b2); ty = b1.ty}, eff ++ eff1 ++ eff2
  | Bop (e1, op, e2) when is_arith_op op ->
     let e1, eff1 = check ctx (TCon "int") e1 in
     let e2, eff2 = check ctx (TCon "int") e2 in
     {expr = Bop (e1, op, e2); ty = TCon "int"}, eff1 ++ eff2
  | Bop (e1, ((And | Or) as op), e2) ->
     let e1, eff1 = check ctx (TCon "bool") e1 in
     let e2, eff2 = check ctx (TCon "bool") e2 in
     {expr = Bop (e1, op, e2); ty = TCon "bool"}, eff1 ++ eff2
  | Bop (e1, Cat, e2) ->
     let e1, eff1 = infer ctx e1 in
     let e2, eff2 = check ctx e1.ty e2 in
     check_concatenable loc e1.ty;
     {expr = Bop (e1, Cat, e2); ty = e1.ty}, eff1 ++ eff2
  | Blk [] -> {expr=Blk[]; ty=TCon "unit"}, (ESet.empty, None)
  | Blk ([{stmt=SExpr e; _}]) ->
     let e, eff = infer ctx e in
     {expr = Blk([SExpr e]); ty = e.ty}, eff
  | Blk (hd :: tl) ->
     let tl = {expr=Blk tl; loc} in
     let get_tl_blk {expr; ty} = match expr with
         Blk l -> l, ty
       | _ -> failwith "internal error"
     in
     begin match hd.stmt with
       SExpr e ->
        let e, eff = infer ctx e in
        let tl, eff' = infer ctx tl in
        let tl, ty = get_tl_blk tl in
        {expr = Blk (SExpr e :: tl); ty}, eff ++ eff'
     | SVal (x, e) ->
        let e, eff = infer ctx e in
        let tl, eff' =
          infer {ctx with var=SMap.add x (e.ty, false) ctx.var} tl
        in
        let tl, ty = get_tl_blk tl in
        {expr = Blk ((SVal (x, e)) :: tl); ty}, eff ++ eff'
     | SVar (x, e) ->
        let e, eff = infer ctx e in
        let tl, eff' =
          infer {ctx with var=SMap.add x (e.ty, true) ctx.var} tl
        in
        let tl, ty = get_tl_blk tl in
        {expr = Blk ((SVal (x, e)) :: tl); ty}, eff ++ eff'
    end
  | Ret e ->
     let e, eff = check ctx ctx.ret_type e in
     {expr = Ret e; ty = new_tvar ()}, eff
  | _ -> failwith "faut finir l'inférence de type"

and check ctx t {expr; loc} =
  let e, eff = infer ctx {expr; loc} in
  try
    if eqtype t e.ty then e, eff else
      Error.type_mismatch loc t e.ty
  with
    Occurs ->
    Error.error loc (fun fmt -> fprintf fmt "Occurs check failed \
between types %a and %a" Pprint.fmt_type t Pprint.fmt_type e.ty)

exception Polymorphism

let rec remove_tvar = function
    TVar r ->
     begin match !r with
       TVLink t -> remove_tvar t
     | TVUnbd _ -> raise Polymorphism
     end
  | TCon s -> TCon s
  | TApp (s, t) -> TApp (s, remove_tvar t)
  | TFun (arg, res, eff) ->
     TFun (List.map remove_tvar arg, remove_tvar res, eff)

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
    | Fun (x, t, e) -> Fun (x, remove_tvar t, remove_tvar_expr e)
    | Blk l -> Blk (List.map remove_tvar_stmt l)
    | Lst l -> Lst (List.map remove_tvar_expr l)
    | Uop (o, e) -> Uop (o, remove_tvar_expr e)
  in
  {expr; ty = remove_tvar ty}

let check_decl var {name; arg; res; body} =
  let ret_type = match res with
      None -> new_tvar ()
    | Some (eff, t) -> erase_type t
  in

  let arg =
    List.map (fun (x, t) -> check_valid_type t; (x, erase_type t)) arg
  in
  let x, t = List.split arg in
  let has_console = ref None in
  let eff = ESet.singleton EDiv, Some has_console in
  let var =
    SMap.add name.string (TFun (t, ret_type, eff), false) var
  in
  let add_var mp (x, t) =
    if SMap.mem x.string mp then
      Error.error_str x.loc (sprintf "Argument %s of %s defined twice" x.string
                               name.string)
    else
      SMap.add x.string (t, false) mp
  in
  let var =
    List.fold_left add_var var arg
  in
  let body, eff = check {var; ret_type; rec_fun = name.string; rec_eff = EMap.empty} ret_type body in
  let ret_type =
    try
      remove_tvar ret_type
    with
      Polymorphism ->
      Error.error_str name.loc
        (sprintf "Function %s has polymporphic type" name.string)
  in
  let eff =
    if !has_console = None then eff
    else if !has_console = Some false then
      if ESet.mem EConsole (fst eff) then
        raise EffectUnification
      else eff
    else add_effect eff EConsole
  in
  let body = remove_tvar_expr body in
  let arg = List.map (fun (x, t) -> (x.string, t)) arg in
  {name = name.string; arg; body; res = TFun (t, ret_type, eff)}

exception NoMain

let check_file (p : decl_loc list) =
  let add_function mp (x, t) =
    if SMap.mem x.string mp then
      Error.error_str x.loc (sprintf "Function %s defined twice" x.string)
    else
      SMap.add x.string (t, false) mp
  in
  let rec check_file main var = function
    | hd :: tl ->
      let d = check_decl var hd in
      let main =
        if d.name = "main" then
          if d.arg = [] then
            Some d
          else
            Error.error_str hd.name.loc "Function name takes no argument"
        else main
      in
      let var = add_function var (hd.name, d.res) in
      let l, main = check_file main var tl in
      d :: l, main
    | [] ->
      match main with
        None -> raise NoMain
      | Some main -> [], main
  in
  check_file None SMap.empty p
