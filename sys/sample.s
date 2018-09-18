
#include "tmax.h"

	.globl _sample
	.globl sample_tem

	.text
	.align 2
	.word 0,0,0,0
sample_tem:
	.byte HTEMPLATE,2
	movl (sp),T
	jmp (T)

	.data
_sample:
	.long sample_tem+STORED_TAG
