open Syntax

module SMap = Map.Make(String)
module SSet = Set.Make(String)

let valid_types =
  ["int", 0; "bool", 0; "unit", 0; "string", 0; "list", 1; "maybe", 1]
  |> List.to_seq |> SMap.of_seq

let valid_effects =
  ["div"; "console"] |> SSet.of_list

let rec is_valid_type = function
    TCon s -> (match SMap.find_opt s valid_types with
        Some 0 -> true | None | Some _ -> false)
  | TApp (s, t) -> (match SMap.find_opt s valid_types with
        Some 1 -> is_valid_type t | None | Some _ -> false)
  | TFun (l, t, e) ->
    List.for_all is_valid_type l && is_valid_type t &&
    List.for_all (fun e -> SSet.mem e valid_effects) e

let type_of_lit = function
    LInt _ -> TCon "int"
  | LBool _ -> TCon "bool"
  | LUnit -> TCon "unit"
  | LString _ -> TCon "string"

let rec check ctx t = function
    Lit l -> if type_of_lit l = t then [] else failwith ""
  | Var s -> (try SMap.find s ctx = t with Not_found -> false)

and infer ctx {expr; _} = match expr with
  Lit l -> type_of_lit l, []
