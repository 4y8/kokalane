open Syntax

module VSet = Map.Make(String)

let type_of_lit = function
    LInt _ -> TCon "int"
  | LBool _ -> TCon "bool"
  | LUnit -> TCon "unit"
  | LString _ -> TCon "string"

let rec check ctx t = function
    Lit l -> type_of_lit l = t
  | Var s -> (try VSet.find s ctx = t with Not_found -> false)
