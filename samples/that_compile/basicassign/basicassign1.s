	.arch armv8-a
	.file	"basicassign1.c"
	.text
	.global	z
	.bss
	.align	2
	.type	z, %object
	.size	z, 4
z:
	.zero	4
	.global	secret
	.align	2
	.type	secret, %object
	.size	secret, 4
secret:
	.zero	4
	.text
	.align	2
	.global	main
	.type	main, %function
main:
.LFB0:
	.cfi_startproc
	sub	sp, sp, #16
	.cfi_def_cfa_offset 16
	adrp	x0, secret
	add	x0, x0, :lo12:secret
	ldr	w0, [x0]
	str	w0, [sp, 12]
	str	wzr, [sp, 12]
	adrp	x0, z
	add	x0, x0, :lo12:z
	ldr	w1, [sp, 12]
	str	w1, [x0]
	adrp	x0, secret
	add	x0, x0, :lo12:secret
	ldr	w0, [x0]
	str	w0, [sp, 12]
	adrp	x0, z
	add	x0, x0, :lo12:z
	ldr	w1, [sp, 12]
	str	w1, [x0]
	mov	w0, 0
	add	sp, sp, 16
	.cfi_def_cfa_offset 0
	ret
	.cfi_endproc
.LFE0:
	.size	main, .-main
	.ident	"GCC: (GNU) 11.2.0"
	.section	.note.GNU-stack,"",@progbits
