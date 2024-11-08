	.text
	.globl println_int
	.globl println_string

alloc:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	24(%rbp), %rdi
	call	malloc
	movq	%rbp, %rsp
	popq	%rbp
	ret

println_int:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	$int_format, %rdi
	movq	16(%rbp), %rsi
        xorq    %rax, %rax
	call	printf
	movq	%rbp, %rsp
	popq	%rbp
	ret

println_unit:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	$unit_format, %rdi
	call	printf
	movq	%rbp, %rsp
	popq	%rbp
	ret

println_string:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	24(%rbp), %rdi
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

	.data
int_format:
	.string "%d\n"

unit_format:
	.string "()\n"
