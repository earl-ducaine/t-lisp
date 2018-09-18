
#  .globl foo

#  .text
#  .data
#  .bss

#  .long 3
#  .word 2,3,4
#  .byte 1

#  .align 1  - word alignment
#  .align 2  - longword alignment

#  .set sym,val

#  .space n	- reserve n bytes of storage

#  Immediate: $
#  Indirect:  *

# Entry points
	.globl _main
	.globl p_make_extend
	.globl p_make_pair

	.set nargs,1

	.text
_main:
	.word 0x0ffc

# Set up heap pointer
	pushl $1024
	calls $1,*$_malloc
	movl r0,heap_pointer
	moval 1024(r0),heap_limit
	pushl r0
    # printf("Space is at %X\n", malloc(1024));
	pushal z
	calls $nargs,*$_printf

# Call the Scheme procedure
#  P     = r0
#  A1    = r1
#  A2    = r2
#  T	 = r9
#  NARGS = r10
#  #F    = r11

	moval null+3,r11	# Set #F

	movl $1,r10		# Set NARGS
	moval p_foo+2,r0	# Set P
	movl -2(r0),r9		# Set T
	pushal ret1		# Push return point
	jmp (r9)

	.align 2		# Return template
	.long 0xabcd
	.word 0xef
ret1:
	ashl $-2,r1,-(sp)	# Pass returned value as arg to printf
	pushal z2
	calls $1,*$_printf

	ret

z:	.asciz "Heap at #x%X\n"
z2:	.asciz "Value is %d\n"

null:	.long null+3
	.long null+3

# "Unit" for top level environment

	.data
p_make_pair:
	.long t_make_pair	# Closure-internal closures
p_make_extend:
	.long t_make_extend
heap_pointer:
	.space 4
heap_limit:
	.space 4

# Pure section for initial program?

	.text
t_make_pair:
t_make_extend:
	# P has p_make_pair+2
	# heap pointer is in 6(P)
	# heap limit   is in 10(P)
	rsb

