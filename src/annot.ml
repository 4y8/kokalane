open Syntax

(* on a beaucoup de mutabilité pour rien; peut être séparer le type en deux car
   clo et max_var sont destinées à être modifiées de manière persistente à
   l'échelle d'une fonction, mais pas loc (avec des modifications locales à
   l'échelle des blocs)*)
type env =
  { mutable loc : variable SMap.t ; par : variable SMap.t
  ; mutable clo : (int * bool) SMap.t ; mutable nvar : int
  ; mutable max_var : int ; mutable nclo : int }

let nvar = ref 0

let fresh_name () = incr nvar; Printf.sprintf "_l%d" !nvar

let is_mutable = function
  | VGlo _ -> false
  | VLoc (_, b) | VClo (_, b) -> b

let find_var env x =
  match SMap.find_opt x env.loc with
  | Some v -> v
  | None ->
      match SMap.find_opt x env.par with
      | None -> VGlo x
      | Some v ->
            match SMap.find_opt x env.clo with
            | Some (n, b) -> VClo (n, b)
            | None ->
                let n, b = env.nclo, is_mutable v in
                env.clo <- SMap.add x (n, b) env.clo;
                env.nclo <- env.nclo + 1;
                VClo (n, b)

let arg_env l =
  List.mapi (fun i (x, _) -> (x, VLoc (24 + 8 * i, false))) l
  |> List.fold_left (fun env (x, i) -> SMap.add x i env) SMap.empty

let (>>=) l r glob =
  let v, glob = l glob in
  r v glob

let (let*) = (>>=)

let return v glob = v, glob

let add_fun x glob =
  (), x :: glob

let rec mmap f = function
  | [] -> return []
  | hd :: tl ->
      let* hd = f hd in
      let* tl = mmap f tl in
      return @@ hd :: tl

let rec annot env {texpr; ty} =
  let aty = fst (Type.remove_tvar ty) in
  let* aexpr = match texpr with
    | Lit l -> return @@ ALit l
    | Bop (e1, op, e2) ->
        let* e1 = annot env e1 in
        let* e2 = annot env e2 in
        return @@ ABop (e1, op, e2)
    | If (e1, e2, e3) ->
        let* e1 = annot env e1 in
        let* e2 = annot env e2 in
        let* e3 = annot env e3 in
        return @@ AIf (e1, e2, e3)
    | Ret e ->
        let* e = annot env e in
        return @@ ARet e
    | App (e, l) ->
        let* e = annot env e in
        let* l = mmap (annot env) l in
        return @@ AApp (e, l)
    | Lst l ->
        let* l = mmap (annot env) l in
        return @@ ALst l
    | Uop (op, e) ->
        let* e = annot env e in
        return @@ AUop (op, e)
    | Blk l ->
        let nvar = env.nvar in
        let loc = env.loc in
        let rec annot_blk = function
          | [] -> return []
          | TExpr e :: tl ->
              let* e = annot env e in
              let* tl = annot_blk tl in
              return @@ (AExpr e) :: tl
          | TDVal (x, e) :: tl ->
              let* e = annot env e in
              let n = env.nvar in
              env.nvar <- n + 8;
              if env.max_var < env.nvar then
                env.max_var <- env.nvar;
              env.loc <- SMap.add x (VLoc (- n - 8, false)) env.loc;
              let* tl = annot_blk tl in
              return @@ ADVal (- n - 8, e) :: tl
          | TDVar (x, e) :: tl ->
              let* e = annot env e in
              let n = env.nvar in
              env.nvar <- n + 8;
              if env.max_var < env.nvar then
                env.max_var <- env.nvar;
              env.loc <- SMap.add x (VLoc (- n - 8, true)) env.loc;
              let* tl = annot_blk tl in
              return @@ ADVar (- n - 8, e) :: tl
        in
        let* l = annot_blk l in
        let blk = ABlk l in
        env.nvar <- nvar;
        env.loc <- loc;
        return blk
    | Wal (x, e) ->
        let* e = annot env e in
        return @@ AWal (find_var env x, e)
    | Var x ->
        return @@ AVar (find_var env x)
    | CheckPredicate (x, pred) ->
       let* e = annot env (pred x) in
       return e.aexpr
    | Fun (l, e) ->
        let nenv =
          { loc = arg_env l ; par = Context.merge_ctx env.par env.loc
          ; clo = SMap.empty ; nvar = 0 ; nclo = 0 ; max_var = 0 }
        in
        let* e = annot nenv e in
        let f = fresh_name () in
        let* _ = add_fun (f, e, nenv.max_var) in
        let clo = Array.make nenv.nclo (VLoc (-1, false)) in
        SMap.iter (fun x (i, _) -> clo.(i) <- find_var env x) nenv.clo;
        return @@
          AClo (Array.to_list clo |> List.filter ((<>) (VLoc (-1, false))), f)
  in return { aexpr ; aty }

let annot_program p =
  let rec annot_prog = function
      [] -> return ()
    | hd :: tl ->
        let env =
          { loc = arg_env hd.targs ; par = SMap.empty ; clo = SMap.empty ;
            nvar = 0 ; nclo = 0 ; max_var = 0 }
        in
        let* e = annot env hd.tbody in
        let* _ = annot_prog tl in
        add_fun (hd.tname, e, env.max_var)
  in
  let _, glob = annot_prog p [] in
  glob
