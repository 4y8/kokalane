open Syntax

type value
  = VInt of int
  | VString of string
  | VBool of bool
  | VUnit
  | VList of value list
  | VNone
  | VSome of value
  | VClo of (value SMap.t -> value)

exception Return of value

let op_assoc = [Add, (+); Mul, ( * );]

let int = function VInt n -> n | _ -> failwith "impossible"

let rec eval ctx {expr; ty} = match expr with
    Lit (LInt n) -> VInt n
  | Lit (LString s) -> VString s
  | Lit (LBool b) -> VBool b
  | Lit (LUnit) -> VUnit
  | If (e, b1, b2) ->
     if eval ctx e = VBool true then eval ctx b1
     else eval ctx b2
  | Wal (x, e) ->
     failwith "mutation not supported yet"
  | Ret e -> raise (Return (eval ctx e))
  | Bop (e1, op, e2) when is_arith_op op ->
     VInt ((List.assoc op op_assoc) (int @@ eval ctx e1) (int @@ eval ctx e2))
