open Syntax
open X86_64

type code_env = { mutable data : data }

let nfresh = ref 0

let new_int () = incr nfresh; !nfresh
let new_label () = Printf.sprintf "L%d" (new_int ())

let gen_lit env = function
    LUnit -> movq (imm 0) !%rax
  | LInt n -> movq (imm n) !%rax
  | LString s ->
     let l = new_label () in
     env.data <- env.data ++ label l ++ string s;
     movq (ilab l) !%rax
  | LBool b -> movq (imm (if b then 1 else 0)) !%rax

let arith_op = [Add, addq; Sub, subq; Mul, imulq]

let rec gen_expr env {expr; ty} = match expr with
  | Lit l ->
    gen_lit env l
  | Bop (e1, ((Add | Sub | Mul) as op), e2) ->
     let a1 = gen_expr env e1 in
     let a2 = gen_expr env e2 in
     a1 ++ pushq !%rax ++ a2 ++ (List.assoc op arith_op) (ind ~ofs:8 rsp) !%rax
  | Println (e, s) ->
     let a = gen_expr env e in
     a ++ pushq !%rax ++ call ("println_" ^ s)

let gen_fun f =
  label ("_kk_" ^ f.name) ++ pushq !%rbp ++ movq !%rsp !%rbp ++ popq rbp ++ ret
