# The program

	.globl p_foo

	.data
p_foo:
	.long t_foo
	.long p_make_pair
	.long p_make_extend

	.text
t_foo:
	movl $4,r1
	movl (sp)+,r9
	jmp (r9)
