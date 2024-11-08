open Syntax
open X86_64

type code_env =
  { mutable data : data ; mutable nvar : int
  ; mutable vars : int SMap.t}

let empty_env () = { data = nop ; nvar = 0 ; vars = SMap.empty }

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
     a1 ++ pushq !%rax ++ a2 ++ popq rsi ++ (List.assoc op arith_op) !%rsi !%rax
  | Println (e, s) ->
     let a = gen_expr env e in
     a ++ pushq !%rax ++ call ("println_" ^ s) ++ popq rsi ++ movq (imm 0) !%rax
  | Var x ->
     let ofs = -8 * (1 + SMap.find x env.vars) in
     movq (ind ~ofs rbp) !%rax
  | Blk l -> gen_blk env l

and gen_blk env l =
  let nvar, vars = env.nvar, env.vars in
  let rec gen_stmts = function
    [] -> Printf.printf "%d\n" env.nvar;  nop
  | TExpr e :: tl ->
     gen_expr env e ++ gen_stmts tl
  | TDVal (x, e) :: tl 
  | TDVar (x, e) :: tl ->
     env.vars <- SMap.add x env.nvar env.vars;
     env.nvar <- env.nvar + 1;
     let a = gen_expr env e in
     a ++ pushq !%rax ++ gen_stmts tl
  in
  Printf.printf "%d\n" env.nvar;
  let code = gen_stmts l in
  let free = addq (imm (env.nvar * 8)) !%rsp in
  env.nvar <- nvar;
  env.vars <- vars;
  code ++ free

let gen_fun env f =
  let code = gen_expr env f.body in
  label f.name ++ pushq !%rbp ++ movq !%rsp !%rbp ++ code ++ popq rbp ++ ret

let gen_prog (pt, main) =
  let env = empty_env () in
  let text = globl "main" ++ gen_fun env main in
  { text; data = env.data }
