(herald vaxkernel)

        (GLOBAL _MAIN)
        (GLOBAL %MAKE-EXTEND)
        (GLOBAL %MAKE-PAIR)
        (GLOBAL %TEXT-EQUAL?)

        (TEXT)
_MAIN
        (WORD #X0FFC)
        (PUSHL ($ 1024))
        (CALLS ($ 1) (*$ _MALLOC))
        (MOVL R0 (LABEL HEAP-POINTER))
        (ADDL3 ($ 10000) R0 (LABEL HEAP-LIMIT))
        (MOVAL (EXPR (+ NULL 3)) NIL)
        (MOVL ($ 1) NARGS)
        (MOVAL (LABEL BEGIN) P)
        (MOVL (D@R P -2) T)
        (JMP (@R T))
NULL
        (LONG (EXPR (+ NULL 3)))
        (LONG (EXPR (+ NULL 3)))

        (DATA)

        (ALIGN 2)

%MAKE-EXTEND
        (LONG
