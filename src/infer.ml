open Effect
open Syntax
open Syntax.Effect
open Format
open Type
open Error

let rec infer ctx {expr; loc} = match expr with
    Lit l -> {expr = Lit l; ty = type_of_lit l}, no_effect
  | Var x ->
      begin match SMap.find_opt x ctx.var with
        Some (t, _) ->
          let eff =
            if x = ctx.rec_fun
            then add_effect no_effect EDiv
            else no_effect
          in
          {expr = Var x; ty=t}, eff
      | None -> Error.unknown_var loc x
      end
  | Wal (x, e) ->
      begin match SMap.find_opt x.string ctx.var with
        None -> unknown_var x.loc x.string
      | Some (_, false) ->
          error_str loc (sprintf "Variable %s is immutable" x.string)
      | Some (t, true) ->
          let e, eff = check ctx t e in
          {expr = Wal (x.string, e); ty = unit}, eff
      end
  | App (f, x) when is_builtin_fun f <> "" ->
      let s = is_builtin_fun f in
      begin match s, x with
        "println", [e] ->
          let e, eff = infer ctx e in
          check_printable loc e.ty;
          let pr_type = TFun ([e.ty], unit, add_effect eff EConsole) in
          {expr = App({expr = Var s; ty = pr_type}, [e]); ty = unit},
          add_effect eff EConsole
      | "default", [e; e'] ->
          let e', eff' = infer ctx e' in
          let e, eff = check ctx (maybe e'.ty) e in
          let def_type = TFun ([e.ty; e'.ty], e'.ty, eff) in
          {expr = App ({expr = Var s; ty = def_type}, [e; e']); ty = e'.ty},
          eff ++ eff'
      | "head", [e] ->
          let tv = new_tvar () in
          let e, eff = check ctx (list tv) e in
          let hd_type = TFun ([list tv], maybe tv, eff) in
          {expr = App ({expr = Var s; ty = hd_type}, [e]); ty = maybe tv}, eff
      | "tail", [e] ->
          let tv = list (new_tvar ()) in
          let e, eff = check ctx tv e in
          let tl_type = TFun ([tv], tv, no_effect) in
          {expr = App({expr = Var s; ty = tl_type}, [e]); ty = tv}, eff
      | "repeat", [n; b] ->
          let n, eff = check ctx int n in
          let b, eff' = check_fun ctx [] unit b in
          let rep_type = TFun ([int; b.ty], unit, eff ++ eff') in
          {expr = App ({expr = Var s; ty = rep_type}, [n; b]); ty = unit},
          eff ++ eff'
      | "while", [c; b] ->
          let c, eff = check_fun ctx [] bool c in
          let b, eff' = check_fun ctx [] unit b in
          let eff = add_effect (eff ++ eff') EDiv in
          let whi_type = TFun ([c.ty; b.ty], unit, eff) in
          {expr = App ({expr = Var s; ty = whi_type}, [c; b]); ty = unit},
          eff
      | "for", [m; n; b] ->
          let m, eff = check ctx int m in
          let n, eff' = check ctx int n in
          let b, eff'' = check_fun ctx [int] unit b in
          let eff = eff ++ eff' ++ eff'' in
          let for_type = TFun ([m.ty; n.ty; b.ty], unit, eff) in
          {expr = App({expr = Var s; ty = for_type}, [m; n; b]); ty = unit},
          eff
      | _ ->
          error_str loc @@
          sprintf "Function %s, got the wrong number of arguments" s
                (* pourrait être mieux, en signalant le nombre d'arguments attendu *)
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
             error_str loc @@
             sprintf "Function expected %d arguments, got %d" (List.length arg)
               (List.length x))
      | _ ->
          error loc (fun fmt ->
              fprintf fmt "Exepected a function, got an expression of type %a"
                Pprint.fmt_type f.ty)
      end
  | Lst l ->
      let ty = new_tvar () in
      let l, eff = List.split (List.map (check ctx ty) l) in
      {expr = Lst l; ty = list ty}, List.fold_left (++) no_effect eff
  | If (e, b1, b2) ->
      let e, eff = check ctx bool e in
      let b1, eff1 = infer ctx b1 in
      let b2, eff2 = check ctx b1.ty b2 in
      {expr = If (e, b1, b2); ty = b1.ty}, eff ++ eff1 ++ eff2
  | Bop (e1, ((Add | Sub | Mul | Div | Mod) as op), e2) ->
      let e1, eff1 = check ctx int e1 in
      let e2, eff2 = check ctx int e2 in
      {expr = Bop (e1, op, e2); ty = int}, eff1 ++ eff2
  | Bop (e1, ((And | Or) as op), e2) ->
      let e1, eff1 = check ctx bool e1 in
      let e2, eff2 = check ctx bool e2 in
      {expr = Bop (e1, op, e2); ty = bool}, eff1 ++ eff2
  | Bop (e1, Cat, e2) ->
      let e1, eff1 = infer ctx e1 in
      let e2, eff2 = check ctx e1.ty e2 in
      check_concatenable loc e1.ty;
      {expr = Bop (e1, Cat, e2); ty = e1.ty}, eff1 ++ eff2
  | Bop (e1, ((Lt | Gt | Leq | Geq) as op), e2) ->
      let e1, eff1 = infer ctx e1 in
      let e2, eff2 = check ctx e1.ty e2 in
      check_comparable loc e1.ty;
      {expr = Bop (e1, op, e2); ty = bool}, eff1 ++ eff2
  | Bop (e1, ((Eq | Dif) as op), e2) ->
      let e1, eff1 = infer ctx e1 in
      let e2, eff2 = check ctx e1.ty e2 in
      check_equalable loc e1.ty;
      {expr = Bop (e1, op, e2); ty = bool}, eff1 ++ eff2
  | Blk (l) ->
      infer_blk ctx loc l
  | Ret e ->
      let e, eff = check ctx ctx.ret_type e in
      {expr = Ret e; ty = new_tvar ()}, eff
  | Uop (Neg, e) ->
      let e, eff = check ctx int e in
      {expr = Uop (Neg, e); ty = int}, eff
  | Uop (Not, e) ->
      let e, eff = check ctx bool e in
      {expr = Uop (Neg, e); ty = bool}, eff
  | Fun (arg, t, b) ->
      let ret_type = match t with
          Some (_, t) -> erase_type t
        | None -> new_tvar ()
      in
      let arg = List.map (fun (x, t) -> x, erase_type t) arg in
      let x, tys = List.split arg in
      let add_var mp (x, t) =
        if SMap.mem x.string mp then
          error_str x.loc (sprintf "Argument %s defined twice" x.string)
        else
          SMap.add x.string (t, false) mp
      in
      let arg_map = List.fold_left add_var SMap.empty arg in
      let var = SMap.merge (fun _ v1 v2 ->
          match v1, v2 with
            None, None -> None
          | _, Some v -> Some v
          | Some v, _ -> Some v) ctx.var arg_map in
      let body, eff = check {ctx with var; ret_type = new_tvar ()} ret_type b in
      (match t with
        None -> ()
      | Some (e, _) ->
          if not ESet.(equal (fst eff) (fst (erase_effects e))) then
            error_str loc (sprintf "Anonymous function has ill defined effects."));
      let arg = List.map (fun (x, t) -> (x.string, t)) arg in
      {expr = Fun (arg, (), body); ty = TFun (tys, body.ty, eff)}, no_effect

and check ctx t {expr; loc} =
  let e, eff = infer ctx {expr; loc} in
  try
    if eqtype t e.ty then e, eff else
      Error.type_mismatch loc t e.ty
  with
    Occurs ->
      error loc (fun fmt ->
          fprintf fmt "Occurs check failed between types %a and %a"
            Pprint.fmt_type t Pprint.fmt_type e.ty)
  | EffectUnification ->
      error loc (fun fmt ->
          fprintf fmt "Effect mismatch between types %a and %a"
            Pprint.fmt_type t Pprint.fmt_type e.ty)

and infer_blk ctx loc = function
    [] -> {expr = Blk[]; ty = unit}, no_effect
  | [{stmt = SExpr e; _}] ->
      let e, eff = infer ctx e in
      {expr = Blk [SExpr e]; ty = e.ty}, eff
  | hd :: tl ->
      let tl = {expr = Blk tl; loc} in
      let get_tl_blk {expr; ty} = match expr with
          Blk l -> l, ty
        | _ -> failwith "internal error"
      in
      match hd.stmt with
        SExpr e ->
          let e, eff = infer ctx e in
          let tl, eff' = infer ctx tl in
          let tl, ty = get_tl_blk tl in
          {expr = Blk (SExpr e :: tl); ty}, eff ++ eff'
      | SVal (x, e) ->
          let e, eff = infer ctx e in
          let tl, eff' =
            infer {ctx with var = SMap.add x (e.ty, false) ctx.var} tl
          in
          let tl, ty = get_tl_blk tl in
          {expr = Blk ((SVal (x, e)) :: tl); ty}, eff ++ eff'
      | SVar (x, e) ->
          let e, eff = infer ctx e in
          let tl, eff' =
            infer {ctx with var = SMap.add x (e.ty, true) ctx.var} tl
          in
          let tl, ty = get_tl_blk tl in
          {expr = Blk (SVal (x, e) :: tl); ty}, eff ++ eff'

(* la fonction ne sera pas appelé avec des variables de types dans l, pas besoin
   de traiter l'exception occurs check *)
and check_fun ctx l rt e =
  let loc = e.loc in
  let e, eff = infer ctx e in
  let t = TFun (l, rt, no_effect) in
  (if not (eqtype ~check_effect:false e.ty t) then
    error loc (fun fmt ->
         fprintf fmt "Expected a function of type %a, got an expression of type %a"
           Pprint.fmt_type t Pprint.fmt_type e.ty));
  match fst (remove_tvar e.ty) with
    TFun (_, _, eff') -> e, eff ++ eff'
  | _ -> failwith "internal error" (* impossible normalement *)

let check_decl var {name; arg; res; body} =
  let has_console = ref None in
  let ret_type, ret_eff = match res with
      None -> new_tvar (), (ESet.singleton EDiv, Some has_console)
    | Some (eff, t) ->
        erase_type t, erase_effects eff
  in
  let arg = List.map (fun (x, t) -> x, erase_type t) arg in
  let x, t = List.split arg in
  let var = SMap.add name.string (TFun (t, ret_type, ret_eff), false) var in
  let add_var mp (x, t) =
    if SMap.mem x.string mp then
      error_str x.loc @@
      sprintf "Argument %s of %s defined twice" x.string name.string
    else
      SMap.add x.string (t, false) mp
  in
  let arg_map = List.fold_left add_var SMap.empty arg in
  let var = SMap.merge (fun _ v1 v2 ->
      match v1, v2 with
        None, None -> None
      | _, Some v -> Some v
      | Some v, _ -> Some v) var arg_map in
  let body, eff = check {var; ret_type; rec_fun = name.string } ret_type body in
  let ret_type, poly = remove_tvar ret_type in
  if poly then
    Error.error_str name.loc
      (sprintf "Function %s has polymporphic type" name.string);
  let eff =
    if !has_console = None then eff
    else if !has_console = Some false then
      if ESet.mem EConsole (fst eff) then
        error_str name.loc @@
        sprintf "Function %s has console effect while it shouldn't" name.string
      else eff
    else add_effect eff EConsole
  in
  let body = remove_tvar_expr body in
  let arg = List.map (fun (x, t) -> (x.string, t)) arg in
  if res <> None then
    if not ESet.(equal (fst eff) (fst ret_eff)) then
      error_str name.loc @@
      sprintf "Function %s has ill defined effects." name.string;
  {name = name.string; arg; body; res = TFun (t, ret_type, eff)}

exception NoMain

let check_file (p : decl_loc list) =
  let add_function mp (x, t) =
    if SMap.mem x.string mp then
      error_str x.loc (sprintf "Function %s defined twice" x.string)
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
              error_str hd.name.loc "Function name takes no argument"
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
