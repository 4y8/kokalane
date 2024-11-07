.text

alloc:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	24(%rsp), %rdi
	call	malloc
	movq	%rbp, %rsp
	popq	%rbp
	ret

println_int:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	int_format, %rdi
	movq	24(%rsp), %rsi
	call	printf
	movq	%rbp, %rsp
	popq	%rbp
	ret

println_unit:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	unit_format, %rdi
	call	printf
	movq	%rbp, %rsp
	popq	%rbp
	ret

println_string:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	24(%rsp), %rdi
	call	puts
	movq	%rbp, %rsp
	popq	%rbp
	ret

int_div:
	movq	8(%rsp), %rdi
	movq	16(%rsp), %rsi
	ret

.data
int_format:
.string "%d\n"	

unit_format:
.string "()\n"
