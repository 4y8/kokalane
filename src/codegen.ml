open Syntax
open X86_64

let nfresh = ref 0

let new_int () = incr nfresh; !nfresh
let new_label () = Printf.sprintf "L%d" (new_int ())

let gen_lit = function
  | LUnit -> movq (imm 0) !%rax, nop
  | LInt n -> movq (imm n) !%rax, nop
  | LString s ->
     let l = new_label () in
     movq (ilab l) !%rax, label l ++ string s
  | LBool b -> movq (imm (if b then 1 else 0)) !%rax, nop

let arith_op = [Add, addq; Sub, subq; Mul, imulq]

let cmp_op = [Lt, setl; Gt, setg; Leq, setle; Geq, setge]

let rec gen_expr ret {aexpr; aty} = match aexpr with
  | ALit l ->
      gen_lit l
  | ABop (e1, ((Add | Sub | Mul) as op), e2) ->
      let a1, d1 = gen_expr ret e1 in
      let a2, d2 = gen_expr ret e2 in
      a1 ++ pushq !%rax ++ a2 ++ popq rsi ++
      (List.assoc op arith_op) !%rsi !%rax,
      d1 ++ d2
  | ABop (e1, ((Lt | Gt | Leq | Geq) as op), e2) ->
      let a1, d1 = gen_expr ret e1 in
      let a2, d2 = gen_expr ret e2 in
      a1 ++ pushq !%rax ++ a2 ++ popq rsi ++ movq !%rax !%rdi ++
      xorl !%eax !%eax ++ cmpq !%rdi !%rsi ++ (List.assoc op cmp_op) !%al,
      d1 ++ d2
  | ABlk l -> gen_blk ret l
  | AIf (c, t, f) ->
      let ac, dc = gen_expr ret c in
      let at, dt = gen_expr ret t in
      let af, df = gen_expr ret f in
      let false_label = new_label () in
      let end_label = new_label () in
      ac ++ cmpq (imm 1) !%rax ++ jne false_label ++ at ++ jmp end_label ++
      label false_label ++ af ++ label end_label, dc ++ dt ++ df
  | ARet e ->
      let a, d = gen_expr ret e in
      a ++ jmp ret, d
  | AApp (f, l) ->
      let af, df = gen_expr ret f in
      let al, dl = List.map (gen_expr ret) l |> List.split in
      let d = List.fold_left (++) df dl in
      let al = List.fold_left (fun code aarg -> aarg ++ pushq !%rax ++ code) nop al in
      pushq !%r12 ++ af ++ movq !%rax !%r12 ++ al ++ call_star (ind r12) ++
      addq (imm (8 * List.length l)) !%rsp ++ popq r12, d
  | AWal (x, e) ->
      let a, d = gen_expr ret e in
      let a = match x with
        | VClo (n, _) ->
            a ++ movq (ind ~ofs:(n + 8) r12) !%r13 ++ movq !%rax (ind r13)
        | VLoc (ofs, _) ->
            a ++ movq (ind ~ofs rbp) !%r13 ++ movq !%rax (ind r13)
        | _ -> failwith "impossible"
      in 
      a, d
  | AClo (l, f) ->
      let n = List.length l in
      let load_var i = function
        | VClo (n, _)  ->
            movq (ind ~ofs:(n + 8) r12) (ind ~ofs:(8 * i + 1) r13)
        | VLoc (ofs, _) ->
            movq (ind ~ofs rbp) (ind ~ofs:(8 * i + 1) r13)
        | _ -> failwith "impossible"
      in
      let la = List.mapi load_var l |> List.fold_left (++) nop in
      movq (imm (n * 8)) !%rdi ++ call "kokalloc" ++ movq !%rax !%r13 ++
      movq (lab f) (ind r13) ++ la, nop
  | AVar x ->
      let a = 
        match x with
        | VGlo l -> movq (lab l) !%rax
        | VLoc (ofs, false) ->
            movq (ind ~ofs rbp) !%rax
        | VLoc (ofs, true) ->
            movq (ind ~ofs rbp) !%r13 ++ movq (ind r13) !%rax
        | VClo (n, false) ->
            movq (ind ~ofs:(n + 8) r12) !%rax
        | VClo (n, true) ->
            movq (ind ~ofs:(n + 8) r12) !%rax ++ movq (ind rax) !%rax
      in a, nop

and gen_blk ret = function
  | [] -> nop, nop
  | s :: tl ->
      let atl, dtl = gen_blk ret tl in
      let a, d = match s with
        | AExpr e ->
            gen_expr ret e
        | ADVal (ofs, e) ->
            let a, d = gen_expr ret e in
            a ++ movq !%rax (ind ~ofs rbp), d
        | ADVar (ofs, e) ->
            let a, d = gen_expr ret e in
            a ++ movq !%rax !%r13 ++ movq (imm 8) !%rdi ++ call "kokalloc" ++
            movq !%r13 (ind rax) ++ movq !%rax (ind ~ofs rbp), d
      in
      a ++ atl, d ++ dtl

let gen_fun (f, e, max_var) =
  let e, data = 
