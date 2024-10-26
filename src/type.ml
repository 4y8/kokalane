open Syntax

module SMap = Map.Make(String)
module SSet = Set.Make(String)

let valid_types =
  ["int", 0; "bool", 0; "unit", 0; "string", 0; "list", 1; "maybe", 1]
  |> List.to_seq |> SMap.of_seq

let valid_effects =
  ["div"; "console"] |> SSet.of_list

let eqefflist l l' = SSet.equal (SSet.of_list l) (SSet.of_list l')

let rec eqtype t t' = match t, t' with
    TCon s, TCon s' -> s = s'
  | TApp (f, t), TApp (f', t') -> f = f' && eqtype t t'
  | TFun (arg, res, eff), TFun (arg', res', eff') ->
    List.for_all2 eqtype arg arg' && eqtype res res' && eqefflist eff eff'
  | _, _ -> false

let rec erase_type {ty; _} = match ty with
    TCon s -> TCon s
  | TApp (s, t) -> TApp (s.string, erase_type t)
  | TFun (arg, res, eff) ->
     TFun (List.map erase_type arg, erase_type res,
           List.map (fun s -> s.string) eff)

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

let type_of_lit = function
    LInt _ -> TCon "int"
  | LBool _ -> TCon "bool"
  | LUnit -> TCon "unit"
  | LString _ -> TCon "string"

let rec check ctx t {expr; loc} = match expr with
    Lst l -> (match t with
                TApp ("list", t) ->
                 List.map (check ctx t) l |> List.fold_left SSet.union SSet.empty
      | _ -> Error.error loc (fun fmt -> Format.fprintf fmt "Type mismatch: \
expected a value of type %a, got a list" Pprint.fmt_type t))
  | If (e, b1, b2) ->
     SSet.(union (check ctx (TCon "bool") e)
             (union (check ctx t b1) (check ctx t b2)))
  | Blk ([{stmt=SExpr e; _}]) ->
     check ctx t e
  | Blk (hd :: tl) ->
     let tl = {expr=Blk (tl); loc} in
     begin match hd.stmt with
       SExpr e ->
        let _, eff = infer ctx e in
        SSet.union eff @@ check ctx t tl
     | SVal (x, e) ->
        let t, eff = infer ctx e in
        check (SMap.add x (t, false) ctx) t tl
     | SVar (x, e) ->
        let t, eff = infer ctx e in
        check (SMap.add x (t, true) ctx) t tl
    end
  | _ ->
    let t', e = infer ctx {expr; loc} in
    if eqtype t t' then e else
      Error.type_mismatch loc t t'

and infer ctx {expr; loc} = match expr with
    Lit l -> type_of_lit l, SSet.empty
  | Var x -> (match SMap.find_opt x ctx with
                Some (t, _) -> t, SSet.empty
              | None -> Error.unknown_var loc x)
  | Ret e -> infer ctx e
  | Wal (x, e) ->
     begin match SMap.find_opt x.string ctx with
       None -> Error.unknown_var x.loc x
     | Some (_, false) ->
        Error.error_str loc (Format.sprintf "Variable %s is immutable" x.string)
     | Some (t, true) ->
        TCon "unit", check ctx t e
     end
  | Lst [] -> Error.error_str loc "Internal error: can't infer the type of an\
empty list" (* fix *)
  | Lst l ->
  (*  let rec get_inferable_elt = function
        [] -> Error.error_str loc "Internal error: list without an inferable"
      | _ -> failwith "todo"
    in
   *)
    TCon "", SSet.empty
  | Blk [] -> TCon "unit", SSet.empty
