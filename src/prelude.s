	.text
	.globl println_int
	.globl println_string

kokalloc:
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
	cmovzq	(%r13), %rax
	ret

.fun_head:
	movq	8(%rsp), %rax
	ret

.fun_tail:
	movq	8(%rsp), %r13
	xorl	%eax, %eax
	testq	%r13, %r13
	cmovnzq 8(%r13), %rax
	ret

.fun_repeat:
	pushq	%r12
	movq	16(%rsp), %rax
	movq	24(%rsp), %r12
.loop3:
	testq	%rax, %rax
	jz	.ret6
	decq	%rax
	call	*(%r12)
	jmp	.loop3
.ret6:
	popq	%r12
	xorl	%eax, %eax
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
	jz	.ret9
	movq	8(%rsp), %rax
	cqto
	idivq	%rsi
	testq	%rdx, 8(%rsp)
	jns	.ret1
	sarq	$63, %rsi
	orq	$1, %rsi
	subq	%rsi, %rax
.ret9:
	xorl	%eax, %eax
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
	testq	%rdi, %rdi
	jz	.ret4
	movq	%rdi, %rax
.loop2:
	movq	8(%rdi), %r13
	testq	%r13, %r13
	jz	.ret5
	movq	8(%rdi), %rdi
	jmp	.loop2
.ret4:
	movq	%rsi, %rax
	ret
.ret5:
	movq	%rsi, 8(%rdi)
	ret

kk_strcat:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	%rdi, %r13
	movq	%rsi, %r15
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

	.data
int_format:
	.string "%d\n"

unit_format:
	.string "()\n"

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
