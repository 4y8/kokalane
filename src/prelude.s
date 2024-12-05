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

int_div:
	movq	8(%rsp), %rax
	cqto
	movq	16(%rsp), %rsi
	idivq	%rsi
	testq	%rdx, 8(%rsp)
	jns	.ret1
	sarq	$63, %rsi
	orq	$1, %rsi
	subq	%rsi, %rax
.ret1:
	ret

int_mod:
	movq	8(%rsp), %rax
	cqto
	movq	16(%rsp), %rsi
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
	.quad .fun_println_string

_clo_println_int:
	.quad .fun_println_int
