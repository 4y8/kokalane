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

let int = function VInt n -> n | _ -> failwith "impossible"

let arith_op f v1 v2 =
  match v1, v2 with
    VInt n1, VInt n2 -> VInt (f n1 n2)
  | _ -> failwith "impossible"

let add = arith_op ( + )
let sub = arith_op ( - )
let mul = arith_op ( * )
let div v1 v2 =
  if v2 = VInt 0 then VInt 0 else
    let i = int v1 in
    let j = int v2 in
    let d = i / j in
    let m = i mod j in
    VInt (if i < 0 && m < 0 then d - (if j < 0 then -1 else 1) else i / j)

let md v1 v2 =
  if v2 = VInt 0 then v1 else
    let i = int v1 in
    let j = int v2 in
    let m = i mod j in
    VInt (if i < 0 && m < 0 then m + (if j < 0 then -j else j) else m)

let cmp_op f v1 v2 =
  match v1, v2 with
    VInt n1, VInt n2 -> VBool (f n1 n2)
  | _ -> failwith "impossible"

let lt = cmp_op ( < )
let gt = cmp_op ( > )
let leq = cmp_op ( <= )
let geq = cmp_op ( >= )

let eq v1 v2 = VBool (v1 = v2)
let dif v1 v2 = VBool (v1 <> v2)

let cat v1 v2 =
  match v1, v2 with
    VList l1, VList l2 -> VList (l1 @ l2)
  | VString v1, VString v2 -> VString (v1 ^ v2)
  | _ -> failwith "impossible"

let bop_assoc =
  [Add, add; Mul, mul; Mod, md; Div, div; Cat, cat; Lt, lt; Gt, gt; Leq, leq;
   Geq, geq; Eq, eq; Dif, dif; Sub, sub]

let nt = function
    VBool b -> VBool (not b)
  | _ -> failwith "impossible"

let neg = function
    VInt n -> VInt (-n)
  | _ -> failwith "impossible"

let uop_assoc = [Neg, neg; Not, nt]

let rec eval ctx {expr; ty} = match expr with
    Lit (LInt n) -> VInt n
  | Lit (LString s) -> VString s
  | Lit (LBool b) -> VBool b
  | Lit (LUnit) -> VUnit
  | If (e, b1, b2) ->
      if eval ctx e = VBool true then eval ctx b1
      else eval ctx b2
  | Wal (x, e) ->
      let r = SMap.find x ctx in
      r := (eval ctx e);
      VUnit
  | Ret e -> raise (Return (eval ctx e))
  | Bop (e1, And, e2) ->
      if eval ctx e1 = VBool true then eval ctx e2 else VBool false
  | Bop (e1, Or, e2) ->
      if eval ctx e1 = VBool false then eval ctx e2 else VBool true
  | Bop (e1, op, e2) ->
      List.assoc op bop_assoc (eval ctx e1) (eval ctx e2)
  | Var x -> !(SMap.find x ctx)
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
      | "head", [e] ->
          begin match eval ctx e with
            VList [] -> VNone
          | VList (hd :: _) -> VSome hd
          | _ -> failwith "impossible"
          end
      | "tail", [e] ->
          begin match eval ctx e with
            VList [] -> VList []
          | VList (_ :: tl) -> VList tl
          | _ -> failwith "impossible"
          end
      | "repeat", [e; b] ->
          let n = int (eval ctx e) in
          let f = match eval ctx b with
              VClo f -> f
            | _ -> failwith "impossible"
          in
          for _ = 1 to n do
            ignore (f [])
          done; VUnit
      | "for", [e; e'; b] ->
          let m = int (eval ctx e) in
          let n = int (eval ctx e') in
          let f = match eval ctx b with
              VClo f -> f
            | _ -> failwith "impossible"
          in
          for i = m to n do
            ignore (f [VInt i])
          done; VUnit
      | "default", [e; d] ->
          begin match eval ctx e with
            VNone -> eval ctx d
          | VSome v -> v
          | _ -> failwith "impossible"
          end
      | "while", [c; b] ->
          let f = match eval ctx b with
              VClo f -> f
            | _ -> failwith "impossible"
          in
          let c = match eval ctx c with
              VClo f -> f
            | _ -> failwith "impossible"
          in
          while (c []) = VBool true do
            ignore (f [])
          done; VUnit
      | _ -> failwith "impossible"
      end
  | App (f, x) ->
      begin match eval ctx f with
        VClo f -> f (List.map (eval ctx) x)
      | _ -> failwith "impossible"
      end
  | Lst l -> VList (List.map (eval ctx) l)
  | Fun (arg, _, body) ->
      VClo
        (fun l ->
           let ctx = List.fold_left2 (fun ctx (x, _) v -> SMap.add x (ref v) ctx) ctx arg l in
           try
             eval ctx body
           with Return v -> v)
  | Blk l -> eval_blk ctx l
  | Uop (op, e) ->
      List.assoc op uop_assoc (eval ctx e)

and eval_blk ctx = function
    [] -> VUnit
  | [SExpr e] -> eval ctx e
  | SExpr e :: tl -> ignore (eval ctx e); eval_blk ctx tl
  | SVal (x, e) :: tl
  | SVar (x, e) :: tl ->
      eval_blk (SMap.add x (ref (eval ctx e)) ctx) tl

let eval_file pt main =
  let rec loop ctx = function
      [] -> ignore (eval ctx main.body)
    | hd :: tl ->
        let r = ref VUnit in
        let ctx = SMap.add hd.name r ctx in
        let v = eval ctx {expr = Fun (hd.arg, (), hd.body); ty = TCon "unit"} in
        r := v;
        loop ctx tl
  in
  loop SMap.empty pt
