 	# Entry to T from foreign code

#include "tmax.h"

	.text
	.globl _call_t_procedure
	.align 1
_call_t_procedure:
	.word 0x0ffe			# Save all registers
	movl 4(ap),P
	movl 8(ap),A1
	movl 12(ap),A2
	movl 16(ap),A3
	movl 20(ap),A4
	movb (ap),NARGS
	# moval _false+LIST_TAG,NIL
	movl -STORED_TAG(P),T
	pushl fp			# Save VAX frame pointer
	pushl $-1			# Mark bottom of T stack
	pushal return_point+STORED_TAG  # Return point
	jmp (T)
	.align 2
	.word 0,0,0,0
return_point:
	.byte STEMPLATE,-2		# Want 1 return value
	moval 8(sp),sp			# Pop off template & -1
	movl (sp)+,fp			# Restore frame pointer
	movl A1,r0			# Return value
	ret
