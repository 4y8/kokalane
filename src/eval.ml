open Syntax
open Type

type value
  = VInt of int
  | VString of string
  | VBool of bool
  | VUnit
  | VList of value list
  | VNone
  | VSome of value
  | VClo of (value list -> value)

exception Return of value

let op_assoc = [Add, (+); Mul, ( * ); Mod, (mod); Div, (/)]

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
  | Var x -> SMap.find x ctx
  | App ({expr=Var x; ty=_}, arg) when SSet.mem x builtin_fun ->
     begin match x, arg with
       "println", [e] ->
       begin match eval ctx e with
         VInt n -> Printf.printf "%d\n" n
       | VString s -> Printf.printf "%s\n" s
       | VUnit -> Printf.printf "()\n"
       | VBool b -> Printf.printf "%s\n" (if b then "True" else "False")
       | _ -> failwith "impossible"
       end; VUnit
     | _ -> failwith "impossible"
     end
  | App (f, x) ->
     begin match eval ctx f with
       VClo f -> f (List.map (eval ctx) x)
     | _ -> failwith "impossible"
     end
  | Lst l -> VList (List.map (eval ctx) l)
  | Fun (arg, _, body) ->
     VClo (fun l ->
         let ctx = List.fold_left2 (fun ctx (x, _) v -> SMap.add x v ctx) ctx arg l in
         eval ctx body)
  | _ -> failwith "impossible"
