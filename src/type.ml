open Syntax

type ctx = { var : (type_pure * bool) SMap.t ; ret_type : type_pure }

let valid_types =
  ["int", 0; "bool", 0; "unit", 0; "string", 0; "list", 1; "maybe", 1]
  |> List.to_seq |> SMap.of_seq

let valid_effects =
  ["div"; "console"] |> SSet.of_list

let builtin_fun =
  ["println"; "repeat"; "while"; "default"; "for"; "head"; "tail"]
  |> SSet.of_list

let is_builtin_fun {expr; loc} = match expr with
    Var x when SSet.mem x builtin_fun -> x
  | _ -> ""

let is_printable loc = function
    TCon "unit" | TCon "bool" | TCon "int" | TCon "string" -> ()
  | t -> Error.error loc (fun fmt ->
             Format.fprintf fmt "Tried to print %a which can't be printed"
               Pprint.fmt_type t)

let is_concatenable loc = function
    TCon "string" | TApp ("list", _) -> ()
  | t ->
     Error.error loc (fun fmt ->
         Format.fprintf fmt "Tried to concatenate %a which is can't be \
concatenated" Pprint.fmt_type t)

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

let rec eqtype t t' = match t, t' with
    TCon s, TCon s' -> false
  | TApp (f, t), TApp (f', t') -> f = f' && eqtype t t'
  | TFun (arg, res, eff), TFun (arg', res', eff') ->
    begin try
        List.for_all2 eqtype arg arg' && eqtype res res' && SSet.equal eff eff'
      with
        Invalid_argument _ -> false
    end
  | TVar r, t' ->
     begin match !r with
       TVLink t -> eqtype t t'
     | TVUnbd n ->
        check_occurs n t'; r := TVLink t'; true
     end
  | _, _ -> false

let rec erase_type {ty; loc} = match ty with
    TCon s -> TCon s
  | TApp (s, t) -> TApp (s.string, erase_type t)
  | TFun (arg, res, eff) ->
     TFun (List.map erase_type arg, erase_type res,
           List.map (fun s -> s.string) eff |> SSet.of_list)
  | TVar r ->
     match !r with
       TVLink ty -> TVar (ref (TVLink (erase_type ty)))
     | TVUnbd n -> TVar (ref (TVUnbd n))

let tvar = ref 0

let new_tvar () =
  incr tvar; TVar (ref (TVUnbd !tvar))

let check_valid_effect {string; loc} =
  if not SSet.(mem string valid_effects) then
    Error.error_str loc @@ Format.sprintf "Unknown effect: %s" string

let rec check_valid_type {ty; loc} = match ty with
    TCon s ->
     begin match SMap.find_opt s valid_types with
       Some 0 -> ()
     | None ->
        Error.error_str loc @@ Printf.sprintf "Unknown type constructor: %s" s
     | Some n ->
        Error.error_str loc @@
          Format.sprintf "Type constructor %s expected %d constructors, got 0"
            s n
     end
  | TApp ({string; loc}, t) ->
     begin match SMap.find_opt string valid_types with
       Some 1 -> check_valid_type t
     | None ->
        Error.error_str loc @@
          Format.sprintf "Unknown type constructor: %s" string
     | Some n ->
        Error.error_str loc @@
          Format.sprintf "Type constructor %s expected %d constructors, got 1"
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

let (++) = SSet.union

let rec infer ctx {expr; loc} = match expr with
    Lit l -> {expr=Lit l; ty =type_of_lit l}, SSet.empty
  | Var x -> (match SMap.find_opt x ctx.var with
                Some (t, _) -> {expr=Var x; ty=t}, SSet.empty
              | None -> Error.unknown_var loc x)
  | Wal (x, e) ->
     begin match SMap.find_opt x.string ctx.var with
         None -> Error.unknown_var x.loc x.string
     | Some (_, false) ->
       Error.error_str loc (Format.sprintf "Variable %s is immutable" x.string)
     | Some (t, true) ->
       let e, eff = check ctx t e in
       {expr=Wal(x.string, e); ty = TCon "unit"}, eff
     end
  | App (f, x) when is_builtin_fun f <> "" ->
     let s = is_builtin_fun f in
     begin match s, x with
       "println", [e] ->
        let e, eff = infer ctx e in
        is_printable loc e.ty;
        let pr_type = TFun ([e.ty], TCon "unit", SSet.singleton "console") in
        {expr=App({expr=Var s; ty = pr_type}, [e]); ty = TCon "unit"},
        eff ++ SSet.singleton "console"
     | _ -> Error.error_str loc (Format.sprintf "Function %s, got the wrong \
number of arguments" s) (* pourrait être mieux, en signalant le nombre d'arguments attendu *)
     end
  | App (f, x) ->
     let f, eff = infer ctx f in
     begin match f.ty with
       TFun (arg, res, eff') ->
        (try
           let x, eff'' = List.map2 (check ctx) arg x |> List.split in
           {expr = App (f, x); ty = res}, List.fold_left (++) (eff ++ eff') eff''
         with Invalid_argument _ ->
           Error.error_str loc (Format.sprintf "Function expected %d arguments, got \
%d" (List.length arg) (List.length x)))
     | _ -> Error.error loc (fun fmt -> Format.fprintf fmt "Exepected a \
function, got an expression of type %a" Pprint.fmt_type f.ty)
     end
  | Lst l ->
     let ty = new_tvar () in
     let l, eff = List.split (List.map (check ctx ty) l) in
     {expr = Lst l; ty = TApp ("list", ty)}, List.fold_left (++) SSet.empty eff
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
  | Blk [] -> {expr=Blk[]; ty=TCon "unit"}, SSet.empty
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
    Error.error loc (fun fmt -> Format.fprintf fmt "Occurs check failed between \
types %a and %a" Pprint.fmt_type t Pprint.fmt_type e.ty)

let check_decl {name; arg; res; body} =
  let ret_type = match res with
      None -> new_tvar ()
    | Some t -> erase_type t
  in
  let arg = List.map (fun (x, t) -> check_valid_type t; (x, erase_type t)) arg in
  let x, t = List.split arg in
  let var =
    List.fold_left (fun mp (x, t) -> SMap.add x (t, false) mp) SMap.empty arg
  in
  let body, eff = check {var; ret_type} ret_type body in
  { name; arg; body; res = TFun (t, body.ty, eff)  }
