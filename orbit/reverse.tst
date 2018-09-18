
(DEFINE (test OLD-LIST)         ; reverse!
  (COND ((NULL? OLD-LIST) '())
        (T
         (ITERATE LOOP ((OLD-CDR (CDR OLD-LIST))
                        (OLD-CAR '())
                        (TAIL    OLD-LIST))
           (COND ((NULL? (CDR TAIL))
                  (SET (CDR TAIL) OLD-CAR)
                  TAIL)
                 (T
                  (SET (CDR TAIL) OLD-CAR)
                  (LOOP (CDR OLD-CDR) TAIL OLD-CDR)))))))

;Near-optimal code (6 instruction inner loop):

CHUNK_25:
* Procedure arguments:  OLD-LIST = A1
 CMP.L NIL,A1                   * Branch unless OLD-LIST
 BEQ IRETURN
 MOVE.L 4-5(A1),A2              * (CDR OLD-LIST)
 MOVE.L NIL,A3
 BRA JUMP_29                    * enter loop in middle

LOOP:
 MOVE.L A3,4-5(A1)              * (SET-CDR TAIL OLD-CAR)
 MOVE.L A1,A3
 MOVE.L A2,A1
 MOVE.L 4-5(A2),A2              * (CDR OLD-CDR)
* JUMP LAMBDA args:  OLD-CDR = A2  OLD-CAR = A3  TAIL = A1
JUMP_29:
 CMP.L NIL,4-5(A1)
 BNE LOOP

 MOVE.L A3,4-5(A1)              * (SET-CDR TAIL OLD-CAR)
DONE:
 JMP IRETURN                    * Return from procedure


;TC does this (15 instruction inner loop):

CHUNK_25:
 CLR.L -(SP)                    * ------ (LAMBDA (OLD-LIST) (IF OLD-LIST ---
 CLR.L -(SP)
* Procedure arguments:  OLD-LIST = XP
 MOVE.L 8(SP),XP
 CMP.L (SLP),XP                 * Branch if OLD-LIST
 BNE ELSE_28
 MOVE.L (SLP),VAL
 LEA $C(SP),SP                  * Get return point address
 JMP -98(SLP)                   * Return from procedure
ELSE_28:
 MOVE.L XP,VAL                  * (LOOP (CDR OLD-LIST) (QUOTE ()) OLD-LIST)
 MOVE.L 4-5(VAL),8(SP)          * (CDR OLD-LIST)
 MOVE.L (SLP),FUN
* JUMP LAMBDA args:  OLD-CDR = 8(SP)  OLD-CAR = FUN  TAIL = XP
JUMP_29:
 MOVE.L XP,VAL                  * Branch if (CDR TAIL)
 MOVE.L 4-5(VAL),D7             * (CDR TAIL)
 CMP.L (SLP),D7
 BNE ELSE_30
 MOVE.L XP,VAL
 MOVE.L FUN,4-5(VAL)            * (SET-CDR TAIL OLD-CAR)
 MOVE.L XP,VAL
 LEA $C(SP),SP                  * Get return point address
 JMP -98(SLP)                   * Return from procedure
ELSE_30:
 MOVE.L XP,VAL
 MOVE.L FUN,4(SP)
 MOVE.L 4(SP),4-5(VAL)          * (SET-CDR TAIL OLD-CAR)
 MOVE.L 8(SP),VAL               * (LOOP (CDR OLD-CDR) TAIL OLD-CDR)
 MOVE.L 4-5(VAL),(SP)           * (CDR OLD-CDR)
 MOVE.L XP,4(SP)
 MOVE.L 8(SP),VAL
 MOVE.L (SP),8(SP)
 MOVE.L 4(SP),FUN
 MOVE.L VAL,XP
 BRA JUMP_29
