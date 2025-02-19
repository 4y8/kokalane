open Effect
open Syntax
open Syntax.Effect
open Format
open Type
open Error
open Context

let build_argmap =
  let add_var mp (x, t) =
    if SMap.mem x.string mp then
      error_str x.strloc @@
      sprintf "Argument %s defined twice" x.string
    else
      SMap.add x.string (t, false) mp
  in
  List.fold_left add_var SMap.empty

let rec infer ctx {sexpr; sloc} = match sexpr with
  | SLit l -> { texpr = Lit l ; ty = type_of_lit l }, NoRec ESet.empty
  | SVar x ->
      let t, eff = find_var ctx sloc x in
      { texpr = Var x ; ty = t }, eff
  | SWal (x, e) ->
      begin match SMap.find_opt x.string ctx.var with
      | None -> unknown_var x.strloc x.string
      | Some (_, false) ->
          error_str sloc (sprintf "Variable %s is immutable" x.string)
      | Some (t, true) ->
          let e, eff = check ctx t e in
          { texpr = Wal (x.string, e) ; ty = unit }, eff
      end
  | SApp (f, x) when is_builtin_fun f <> "" ->
      (* pourrait être mieux avec du polymorhisme pour default, head, et tail ;
         polymorphisme d'effet pour while, for et repeat ; et un système de type
         class pour println *)
      let s = is_builtin_fun f in
      begin match s, x with
      | "println", [e] ->
          let e, eff = infer ctx e in
          { texpr = CheckPredicate (e, check_printable sloc) ; ty = unit },
          eff ++ (NoRec (ESet.singleton EConsole))
      | "default", [e; e'] ->
          let e', eff' = infer ctx e' in
          let e, eff = check ctx (maybe e'.ty) e in
          let def_type = TFun ([e.ty; e'.ty], e'.ty, eff) in
          { texpr = App ({ texpr = Var s ; ty = def_type }, [e; e'])
          ; ty = e'.ty}, eff ++ eff'
      | "head", [e] ->
          let tv = new_tvar () in
          let e, eff = check ctx (list tv) e in
          let hd_type = TFun ([list tv], maybe tv, eff) in
          { texpr = App ({ texpr = Var s ; ty = hd_type }, [e])
          ; ty = maybe tv}, eff
      | "tail", [e] ->
          let tv = list (new_tvar ()) in
          let e, eff = check ctx tv e in
          let tl_type = TFun ([tv], tv, NoRec ESet.empty) in
          { texpr = App({ texpr = Var s ; ty = tl_type }, [e]) ; ty = tv }, eff
      | "repeat", [n; b] ->
          let n, eff = check ctx int n in
          let b, eff' = check_fun ctx [] unit b in
          let rep_type = TFun ([int; b.ty], unit, eff ++ eff') in
          { texpr = App ({ texpr = Var s ; ty = rep_type }, [n; b])
          ; ty = unit}, eff ++ eff'
      | "while", [c; b] ->
          let c, eff = check_fun ctx [] bool c in
          let b, eff' = check_fun ctx [] unit b in
          let eff = eff ++ eff' ++ NoRec (ESet.singleton EDiv) in
          let whi_type = TFun ([c.ty; b.ty], unit, eff) in
          { texpr = App ({ texpr = Var s ; ty = whi_type }, [c; b])
          ; ty = unit}, eff
      | "for", [m; n; b] ->
          let m, eff = check ctx int m in
          let n, eff' = check ctx int n in
          let b, eff'' = check_fun ctx [int] unit b in
          let eff = eff ++ eff' ++ eff'' in
          let for_type = TFun ([m.ty; n.ty; b.ty], unit, eff) in
          { texpr = App({ texpr = Var s ; ty = for_type }, [m; n; b])
          ; ty = unit}, eff
      | _ ->
          error_str sloc @@
          sprintf "Function %s, got the wrong number of arguments" s
          (* pourrait être mieux, en signalant le nombre d'arguments attendu *)
      end
  | SApp (f, x) ->
      let f, eff = infer ctx f in
      begin match f.ty with
        TFun (arg, res, eff') ->
          (try
             let x, eff'' = List.map2 (check ctx) arg x |> List.split in
             { texpr = App (f, x) ; ty = res},
             List.fold_left (++) (eff ++ eff') eff''
           with Invalid_argument _ ->
             error_str sloc @@
             sprintf "Function expected %d arguments, got %d" (List.length arg)
               (List.length x))
      | _ ->
          error sloc (fun fmt ->
              fprintf fmt "Exepected a function, got an expression of type %a"
                Pprint.fmt_type f.ty)
      end
  | SLst l ->
      let ty = new_tvar () in
      let l, eff = List.split (List.map (check ctx ty) l) in
      { texpr = Lst l ; ty = list ty },
      List.fold_left (++) (NoRec ESet.empty) eff
  | SIf (e, b1, b2) ->
      let e, eff = check ctx bool e in
      let b1, eff1 = infer ctx b1 in
      let b2, eff2 = check ctx b1.ty b2 in
      { texpr = If (e, b1, b2) ; ty = b1.ty }, eff ++ eff1 ++ eff2
  | SBop (e1, ((Add | Sub | Mul | Div | Mod) as op), e2) ->
      let e1, eff1 = check ctx int e1 in
      let e2, eff2 = check ctx int e2 in
      { texpr = Bop (e1, op, e2) ; ty = int}, eff1 ++ eff2
  | SBop (e1, ((And | Or) as op), e2) ->
      let e1, eff1 = check ctx bool e1 in
      let e2, eff2 = check ctx bool e2 in
      { texpr = Bop (e1, op, e2); ty = bool}, eff1 ++ eff2
  | SBop (e1, Cat, e2) ->
      let e1, eff1 = infer ctx e1 in
      let e2, eff2 = check ctx e1.ty e2 in
      { texpr = CheckPredicate ((e1, e2), check_concatenable sloc)
      ; ty = e1.ty}, eff1 ++ eff2
  | SBop (e1, ((Lt | Gt | Leq | Geq) as op), e2) ->
      let e1, eff1 = infer ctx e1 in
      let e2, eff2 = check ctx e1.ty e2 in
      { texpr = CheckPredicate((e1, op, e2), check_comparable sloc); ty = bool},
      eff1 ++ eff2
  | SBop (e1, ((Eq | Dif) as op), e2) ->
      let e1, eff1 = infer ctx e1 in
      let e2, eff2 = check ctx e1.ty e2 in
      { texpr = CheckPredicate ((e1, op, e2), check_equalable sloc)
      ; ty = bool}, eff1 ++ eff2
  | SBlk l ->
      infer_blk ctx sloc l
  | SRet e ->
      let e, eff = check ctx ctx.ret_type e in
      { texpr = Ret e ; ty = new_tvar () }, eff
  | SUop (Neg, e) ->
      let e, eff = check ctx int e in
      { texpr = Uop (Neg, e) ; ty = int }, eff
  | SUop (Not, e) ->
      let e, eff = check ctx bool e in
      { texpr = Uop (Neg, e) ; ty = bool }, eff
  (* on ne filtre que sur des variables pour éviter d'évaluer plusieurs fois
     l'expression *)
  | SMat (({sexpr = SVar _; _}) as e, l) ->
      let e, eff = infer ctx e in
      let infer_pattern (p, r) =
        let env, p = check_pattern ctx e.ty p in
        let r, eff = infer { ctx with var = merge_ctx ctx.var env } r in
        (p, r), eff
      in
      let rules, eff' = List.map infer_pattern l |> List.split in
      (* on ne supporte pas les filtrages sans motif *)
      let res_ty = (snd (List.hd rules)).ty in
      List.iter2
        (fun (_, {sloc; _}) (_, {ty; _}) -> check_type ctx ty res_ty sloc)
        l rules;
      { texpr = Mat (e, rules) ; ty = res_ty }, List.fold_left (++) eff eff'
  | SMat (e, l) ->
      incr tvar;
      let x  = string_of_int !tvar in
      let v = { sexpr = SVar x ; sloc = e.sloc } in
      infer ctx
        { sexpr =
            SBlk ([{ stmt = SDVal (x, e); stmloc = e.sloc}
                 ; { stmt = SExpr { sexpr = SMat(v, l) ; sloc }
                   ; stmloc = sloc}])
        ; sloc }
  | SFun (arg, t, b) ->
      let ret_type = match t with
          Some (_, t) -> erase_type ctx.tctx t
        | None -> new_tvar ()
      in
      let arg = List.map (fun (x, t) -> x, erase_type ctx.tctx t) arg in
      let _, tys = List.split arg in
      let arg_map = build_argmap arg in
      let var = merge_ctx ctx.var arg_map in
      let nctx = {ctx with var; ret_type = new_tvar ()} in
      let body, eff = check nctx ret_type b in
      let eff = match t with
        | None -> eff
        | Some (e, _) ->
            let ret_eff = erase_effects e in
            if ESet.subset (get_set ctx eff) (get_set ctx ret_eff) then
              ret_eff
            else
              error_str sloc "Anonymous function has ill defined effects"
      in
      let arg = List.map (fun (x, t) -> (x.string, t)) arg in
      { texpr = Fun (arg, body) ; ty = TFun (tys, body.ty, eff) },
      NoRec ESet.empty

and check_type ctx t t' ?(check_effect=true) sloc =
  try
    if not (eqtype ctx t t' ~check_effect) then
      Error.type_mismatch sloc t t'
  with
    Occurs ->
      error sloc (fun fmt ->
          fprintf fmt "Occurs check failed between types %a and %a"
            Pprint.fmt_type t Pprint.fmt_type t')

and check ctx t {sexpr; sloc} =
  let e, eff = infer ctx {sexpr; sloc} in
  check_type ctx t e.ty sloc;
  e, eff

and infer_blk ctx sloc = function
  | [] -> { texpr = Blk [] ; ty = unit }, NoRec ESet.empty
  | [{stmt = SExpr e; _}] ->
      let e, eff = infer ctx e in
      { texpr = Blk [TExpr e] ; ty = e.ty }, eff
  | hd :: tl ->
      let tl = { sexpr = SBlk tl ; sloc } in
      let get_tl_blk {texpr; ty} = match texpr with
          Blk l -> l, ty
        | _ -> failwith "internal error"
      in
      match hd.stmt with
        SExpr e ->
          let e, eff = infer ctx e in
          let tl, eff' = infer ctx tl in
          let tl, ty = get_tl_blk tl in
          { texpr = Blk (TExpr e :: tl) ; ty }, eff ++ eff'
      | SDVal (x, e) ->
          let e, eff = infer ctx e in
          let tl, eff' =
            infer {ctx with var = SMap.add x (e.ty, false) ctx.var} tl
          in
          let tl, ty = get_tl_blk tl in
          { texpr = Blk ((TDVal (x, e)) :: tl) ; ty }, eff ++ eff'
      | SDVar (x, e) ->
          let e, eff = infer ctx e in
          let tl, eff' =
            infer {ctx with var = SMap.add x (e.ty, true) ctx.var} tl
          in
          let tl, ty = get_tl_blk tl in
          { texpr = Blk (TDVar (x, e) :: tl) ; ty }, eff ++ eff'

and check_fun ctx l rt e =
  let e', eff = infer ctx e in
  let t = TFun (l, rt, NoRec ESet.empty) in
  check_type ctx ~check_effect:false e'.ty t e.sloc;
  match fst (remove_tvar e'.ty) with
  | TFun (_, _, eff') -> e', eff ++ eff'
  | _ -> failwith "internal error" (* impossible normalement *)

and check_pattern ctx t = function
  | CVar v -> SMap.singleton v.string (t, false), TCVar (v.string, t)
  | CCon (s, l) ->
      let n, sch = SMap.find s.string ctx.cons in
      let arg, res = inst_cons sch in
      check_type ctx res t s.strloc;
      let merge x t t' = match t, t' with
        | None, t | t, None -> t
        | Some _, Some _ ->
            error_str s.strloc
              (sprintf "Variable %s is defined twice in pattern" x)
      in
      let env, l = List.map2 (check_pattern ctx) arg l |> List.split in
      List.fold_left (SMap.merge merge) SMap.empty env, TCCon (n, t, l)

let check_decl (var, cons, tctx) {name; args; res; body} =
  let ret_type, ret_eff = match res with
      None -> new_tvar (), HasRec (ESet.singleton EDiv)
    | Some (eff, t) ->
        erase_type tctx t, erase_effects eff
  in
  let arg = List.map (fun (x, t) -> x, erase_type tctx t) args in
  let _, t = List.split arg in
  let var = SMap.add name.string (TFun (t, ret_type, ret_eff), false) var in
  let arg_map = build_argmap arg in
  let var = merge_ctx var arg_map in
  let ctx =
    { var ; ret_type ; rec_fun = name.string ; rec_has_console = ref None
    ; cons ; tctx }
  in
  let tbody, eff = check ctx ret_type body in
  let ret_type, poly = remove_tvar ret_type in
  if poly then
    error_str name.strloc
      (sprintf "Function %s has polymporphic type" name.string);
  let eff =
    match !(ctx.rec_has_console) with
    | None -> eff
    | Some false ->
      if ESet.mem EConsole (get_set ctx eff) then
        error_str name.strloc @@
        sprintf "Function %s has console effect while it shouldn't" name.string
      else eff
    | _ -> eff ++ NoRec (ESet.singleton EConsole)
  in
  let targs = List.map (fun (x, t) -> (x.string, t)) arg in
  let eff =
    if res = None then
      NoRec (get_set ctx eff)
    else if ESet.subset (get_set ctx eff) (get_set ctx ret_eff) then
      ret_eff
    else
      error_str name.strloc @@
      sprintf "Function %s has ill defined effects." name.string
  in
  { tname = name.string; targs; tbody; res_type = TFun (t, ret_type, eff)}

exception NoMain

let check_file p =
  let add_function mp (x, t) =
    if SMap.mem x.string mp then
      error_str x.strloc (sprintf "Function %s defined twice" x.string)
    else
      SMap.add x.string (t, false) mp
  in
  let rec check_file has_main (var, cons, tctx) = function
    | SDeclFun hd :: tl ->
        let d = check_decl (var, cons, tctx) hd in
        let has_main =
          if d.tname = "main" then
            if d.targs = [] then
              true
            else
              error_str hd.name.strloc "Function main takes no argument"
          else has_main
        in
        let var = add_function var (hd.name, d.res_type) in
        let tl, cons = check_file has_main (var, cons, tctx) tl in
        d :: tl, cons
    | SDeclType (n, tv, c) :: tl ->
        let fresh_vars =
          List.init (List.length tv) (fun _ -> incr tvar; !tvar)
        in
        let tv =
          List.map2 (fun s t -> (s, TVar (ref (TVUnbd t)))) tv fresh_vars
        in
        let tv_dic = tv |> List.to_seq |> SMap.of_seq in
        let ret_type = TApp (n, snd (List.split tv)) in
        let tctx = SMap.add n (List.length tv) tctx in
        let cons_map =
          List.mapi
            (fun i (s, t) ->
               s,
               (i, (fresh_vars, List.map (erase_type tctx ~tv:tv_dic) t,
                    ret_type)))
            c
          |> List.to_seq |> SMap.of_seq in
        check_file has_main
          (var, Context.merge_ctx cons cons_map,
           tctx) tl
    | [] ->
        if has_main then [], cons else
          raise NoMain
  in
  check_file false (SMap.empty, SMap.empty, base_types) p
