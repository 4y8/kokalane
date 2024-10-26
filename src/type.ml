open Syntax

module SMap = Map.Make(String)
module SSet = Set.Make(String)

let valid_types =
  ["int", 0; "bool", 0; "unit", 0; "string", 0; "list", 1; "maybe", 1]
  |> List.to_seq |> SMap.of_seq

let valid_effects =
  ["div"; "console"] |> SSet.of_list

let effset_of_list l = List.map (fun s -> s.string) l |> SSet.of_list
let eqefflist l l' = SSet.equal (effset_of_list l) (effset_of_list l')

let rec eqtype t t' = match t.ty, t'.ty with
    TCon s, TCon s' -> s = s'
  | TApp (f, t), TApp (f', t') -> f.string = f'.string && eqtype t t'
  | TFun (arg, res, eff), TFun (arg', res', eff') ->
    List.for_all2 eqtype arg arg' && eqtype res res' && eqefflist eff eff'
  | _, _ -> false

let check_valid_effect {string; loc} =
  if not SSet.(mem string valid_effects) then
    Error.error_str loc @@ Format.sprintf "Unknown effect: %s" string

let rec check_valid_type {ty; loc} = match ty with
    TCon s -> (match SMap.find_opt s valid_types with
        Some 0 -> ()
      | None -> Error.error_str loc @@ Printf.sprintf "Unknown type constructor: %s" s
      | Some n ->
        Error.error_str loc @@
        Format.sprintf "Type constructor %s expected %d constructors, got 0" s n)
  | TApp ({string; loc}, t) -> (match SMap.find_opt string valid_types with
        Some 1 -> check_valid_type t
      | None -> Error.error_str loc
        @@ Format.sprintf "Unknown type constructor: %s" string
      | Some n ->
        Error.error_str loc @@
        Format.sprintf "Type constructor %s expected %d constructors, got 1"
          string n)
  | TFun (l, t, e) ->
    List.iter check_valid_type l; check_valid_type t;
    List.iter check_valid_effect e

let type_of_lit = function
    LInt _ -> TCon "int"
  | LBool _ -> TCon "bool"
  | LUnit -> TCon "unit"
  | LString _ -> TCon "string"

let rec check ctx t {expr; loc} = match expr with
  | _ ->
    let t', e = infer ctx {expr; loc} in
    if eqtype t t' then e else
      Error.error loc (fun fmt -> Format.fprintf fmt "Type mismatch: expected a value of type %a\
, got an expression of type %a" Pprint.fmt_type t Pprint.fmt_type t')

and infer ctx {expr; loc} = match expr with
  Lit l -> type_of_lit l, SSet.empty
