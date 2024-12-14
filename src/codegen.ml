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
      a2 ++ pushq !%rax ++ a1 ++ popq r13 ++
      (List.assoc op arith_op) !%r13 !%rax,
      d1 ++ d2
  | ABop (e1, ((Lt | Gt | Leq | Geq) as op), e2) ->
      let a1, d1 = gen_expr ret e1 in
      let a2, d2 = gen_expr ret e2 in
      a1 ++ pushq !%rax ++ a2 ++ popq r13 ++ movq !%rax !%r14 ++
      xorl !%eax !%eax ++ cmpq !%r14 !%r13 ++ (List.assoc op cmp_op) !%al,
      d1 ++ d2
  | ABop (e1, And, e2) ->
      let a1, d1 = gen_expr ret e1 in
      let a2, d2 = gen_expr ret e2 in
      let lazy_branch = new_label () in
      a1 ++ testq !%rax !%rax ++ jz lazy_branch ++ a2
        ++ label lazy_branch, d1 ++ d2
  | ABop (e1, Or, e2) ->
      let a1, d1 = gen_expr ret e1 in
      let a2, d2 = gen_expr ret e2 in
      let lazy_branch = new_label () in
      a1 ++ testq !%rax !%rax ++ jnz lazy_branch ++ a2
        ++ label lazy_branch, d1 ++ d2
  | ABop (e1, Eq, e2) ->
      let a1, d1 = gen_expr ret e1 in
      let a2, d2 = gen_expr ret e2 in
      a1 ++ pushq !%rax ++ a2 ++ popq rsi ++ movq !%rax !%rdi ++
      xorl !%eax !%eax ++ cmpq !%rdi !%rsi ++ sete !%al,
      d1 ++ d2
  | ABop (e1, (Div | Mod as op), e2) ->
      let a1, d1 = gen_expr ret e1 in
      let a2, d2 = gen_expr ret e2 in
      a2 ++ pushq !%rax ++ a1 ++ pushq !%rax ++
      call (if op = Div then "int_div" else "int_mod") ++ addq (imm 16) !%rsp,
      d1 ++ d2
  | ABop (e1, Dif, e2) ->
      let a, d = gen_expr ret ({aexpr = ABop (e1, Eq, e2); aty}) in
      a ++ xorq (imm 1) !%rax, d
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
      let al = List.fold_left
          (fun code aarg -> aarg ++ pushq !%rax ++ code) nop al
      in
      pushq !%r12 ++ al ++ af ++ movq !%rax !%r12 ++ call_star (ind r12) ++
      addq (imm (8 * List.length l)) !%rsp ++ popq r12, d
  | AWal (x, e) ->
      let a, d = gen_expr ret e in
      let a = match x with
        | VClo (n, _) ->
            a ++ movq (ind ~ofs:(8 * n + 8) r12) !%r13 ++ movq !%rax (ind r13)
        | VLoc (ofs, _) ->
            a ++ movq (ind ~ofs rbp) !%r13 ++ movq !%rax (ind r13)
        | _ -> failwith "impossible"
      in
      a ++ xorl !%eax !%eax, d (* x := e renvoie () *)
  | AClo (l, f) ->
      let n = List.length l in
      let load_var i = function
        | VClo (n, _)  ->
            movq (ind ~ofs:(8 * n + 8) r12) !%r15 ++
            movq !%r15 (ind ~ofs:(8 * i + 8) rax)
        | VLoc (ofs, _) ->
            movq (ind ~ofs rbp) !%r14 ++ movq !%r14 (ind ~ofs:(8 * i + 8) rax)
        | _ -> failwith "impossible"
      in
      let la = List.mapi load_var l |> List.fold_left (++) nop in
      movq (imm (n * 8 + 8)) !%rdi ++ call "kokalloc" ++
      movq (ilab (".fun" ^ f)) !%r14 ++ movq !%r14 (ind rax) ++ la, nop
  | AVar x ->
      let a =
        match x with
        | VGlo l -> movq (ilab ("_clo_" ^ l)) !%rax
        | VLoc (ofs, false) ->
            movq (ind ~ofs rbp) !%rax
        | VLoc (ofs, true) ->
            movq (ind ~ofs rbp) !%r13 ++ movq (ind r13) !%rax
        | VClo (n, false) ->
            movq (ind ~ofs:(8 * n + 8) r12) !%rax
        | VClo (n, true) ->
            movq (ind ~ofs:(8 * n + 8) r12) !%rax ++ movq (ind rax) !%rax
      in a, nop
  | ALst l ->
      let rec gen_lst = function
          [] -> xorl !%eax !%eax, nop
        | hd :: tl ->
            let a, d = gen_expr ret hd in
            let atl, dtl = gen_lst tl in
            atl ++ pushq !%rax ++ a ++ popq r13 ++ movq !%rax !%r14 ++
            movq (imm 16) !%rdi ++ call "kokalloc" ++ movq !%r14 (ind rax) ++
            movq !%r13 (ind ~ofs:8 rax), d ++ dtl
      in gen_lst l
  | AUop (op, e) ->
      let a, d = gen_expr ret e in
      let a = match op with
        | Neg -> a ++ negq !%rax
        | Not -> a ++ xorq (imm 1) !%rax
      in a, d

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

let gen_fun (f, e, n) =
  let a, d = gen_expr (".ret" ^ f) e in
  label (".fun" ^ f) ++
  pushq !%rbp ++
  pushq !%r12 ++
  movq !%rsp !%rbp ++
  subq (imm n) !%rsp ++
  a ++
  label (".ret" ^ f) ++
  addq (imm n) !%rsp ++
  popq r12 ++
  popq rbp ++
  ret, d ++ label ("_clo_" ^ f) ++ address [(".fun" ^ f)]

let prelude =
"kokalloc:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	call	malloc
	movq	%rbp, %rsp
	popq	%rbp
	ret

.fun_println_int:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	$int_format, %rdi
	movq	16(%rbp), %rsi
	xorq	%rax, %rax
	call	printf
	movq	%rbp, %rsp
	popq	%rbp
	ret

.fun_println_unit:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	$unit_format, %rdi
	xorq	%rax, %rax
	call	printf
	movq	%rbp, %rsp
	popq	%rbp
	ret

.fun_println_string:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	16(%rbp), %rdi
	call	puts
	movq	%rbp, %rsp
	popq	%rbp
	ret

.fun_default:
	movq	8(%rsp), %r13
	movq	16(%rsp), %rax
	testq	%r13, %r13
	jz	.ret12
	movq	(%r13), %rax
.ret12:
	ret

.fun_head:
	movq	8(%rsp), %rax
	ret

.fun_tail:
	movq	8(%rsp), %r13
	xorl	%eax, %eax
	testq	%r13, %r13
	jz	.ret11
	movq	8(%r13), %rax
.ret11:
	ret

.fun_repeat:
	pushq	%r12
	movq	16(%rsp), %rax
	movq	24(%rsp), %r12
.loop3:
	testq	%rax, %rax
	jz	.ret6
	decq	%rax
	pushq	%rax
	call	*(%r12)
	popq	%rax
	jmp	.loop3
.ret6:
	popq	%r12
	ret

.fun_for:
	pushq	%r12
	movq	32(%rsp), %r12
	movq	16(%rsp), %rax
.loop4:
	cmpq	%rax, 24(%rsp)
	jl	.ret7
	pushq	%rax
	call	*(%r12)
	popq	%rax
	incq	%rax
	jmp	.loop4
.ret7:
	popq	%r12
	xorl	%eax, %eax
	ret

.fun_while:
	pushq	%r12
.loop5:
	movq	16(%rsp), %r12
	call	*(%r12)
	testb	%al, %al
	jz	.ret8
	movq	24(%rsp), %r12
	call	*(%r12)
	jmp	.loop5
.ret8:
	popq	%r12
	xorl	%eax, %eax
	ret

int_div:
	movq	16(%rsp), %rsi
	testq	%rsi, %rsi
	cmovzq	%rsi, %rax
	jz	.ret1
	movq	8(%rsp), %rax
	cqto
	idivq	%rsi
	testq	%rdx, 8(%rsp)
	jns	.ret1
	sarq	$63, %rsi
	orq	$1, %rsi
	subq	%rsi, %rax
.ret1:
	ret

int_mod:
	movq	16(%rsp), %rsi
	movq	8(%rsp), %rax
	testq	%rsi, %rsi
	jz	.ret10
	cqto
	idivq	%rsi
	testq	%rdx, 8(%rsp)
	jns	.ret2
	movq	%rsi, %rdi
	sarq	$63, %rdi
	xorq	%rdi, %rsi
	subq	%rdi, %rsi
	addq	%rsi, %rdx
.ret2:
	movq	%rdx, %rax
.ret10:
	ret

kk_streq:
	xorl	%eax, %eax
.loop1:
	movb	(%rax, %rdi), %cl
	movb	(%rax, %rdi), %dl
	testb	%dl, %dl
	jz	.ret3
	testb	%cl, %cl
	jz	.ret3
	incq	%rax
	cmpb	%cl, %dl
	je	.loop1
	xorl	%eax, %eax
	ret
.ret3:
	xorl	%eax, %eax
	cmpb	%cl, %dl
	sete	%al
	ret

kk_lstcat:
	movq	16(%rsp), %rsi
	movq	8(%rsp), %rdi
	testq	%rdi, %rdi
	jz	.ret4
	pushq	%rsi
	movq	%rdi, %r13
	movq	$16, %rdi
	call	kokalloc
	movq	%rax, %r15
.loop2:
	movq	0(%r13), %r14
	movq	%r14, 0(%rax)
	movq	8(%r13), %r14
	testq	%r14, %r14
	jz	.ret5
	movq	%rax, %r14
	movq	$16, %rdi
	call	kokalloc
	movq	%rax, 8(%r14)
	movq	8(%r13), %r13
	jmp	.loop2
.ret4:
	movq	%rsi, %rax
	ret
.ret5:
	popq	%rsi
	movq	%rsi, 8(%rax)
	movq	%r15, %rax
	ret

kk_strcat:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	24(%rbp), %r15
	movq	16(%rbp), %r13
	movq	%r13, %rdi
	call	strlen
	movq	%rax, %r14
	movq	%r15, %rdi
	call	strlen
	addq	%r14, %rax
	incq	%rax
	movq	%rax, %rdi
	call	malloc
	movq	%rax, %rdi
	movq	%r13, %rsi
	call	strcpy
	movq	%r15, %rsi
	movq	%rax, %rdi
	call	strcat
	movq	%rbp, %rsp
	popq	%rbp
	ret
"

let prelude_data =
"int_format:
	.string \"%d\\n\"

unit_format:
	.string \"()\\n\"

_clo_println_string:
	.quad	.fun_println_string

_clo_println_int:
	.quad	.fun_println_int

_clo_println_unit:
	.quad	.fun_println_unit

_clo_repeat:
	.quad	.fun_repeat

_clo_while:
	.quad	.fun_while

_clo_for:
	.quad	.fun_for

_clo_head:
	.quad	.fun_head

_clo_tail:
	.quad	.fun_tail

_clo_default:
	.quad	.fun_default

_clo_strcat:
	.quad	kk_strcat

_clo_streq:
	.quad	kk_streq

_clo_lstcat:
	.quad	kk_lstcat
"

let gen_prog pt =
  let rec gen_prog = function
    [] -> nop, nop
  | hd :: tl ->
      let a, d = gen_fun hd in
      let atl, dtl = gen_prog tl in
      a ++ atl, d ++ dtl
  in
  let text, data = gen_prog pt in
  { text =
      globl "main" ++ inline prelude ++ text ++
      label "main" ++ pushq !%r12 ++ pushq !%r13 ++ pushq !%r14 ++
      pushq !%r15 ++ call ".funmain" ++ popq r15 ++ popq r14 ++ popq r13 ++
      popq r12 ++ xorl !%eax !%eax ++ ret
  ; data = data ++ inline prelude_data }
