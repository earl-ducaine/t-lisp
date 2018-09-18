(herald vaxkernel)


N-ARY SETUP

;;; S1 has number of required args
;;; NARGS has number of actual args
;;; XP has %make-pair
;;; A4 holds result list

   ((JSB (*D@R XP -2))                      ; call %make-pair
    (MOVL (INDEX (R PSB) NARGS) (D@R A5 1)) ; last extra register -> car
    (MOVL (R NIL) (D@R A5 -3))              ; nil -> cdr
    (JMP NARY-TEST)

NARY-LOOP
    (MOVL (R A5) (R A4))
    (JSB (*D@R XP -2))
    (MOVL (INDEX (R PSB) NARGS) (D@R A5 1))
    (MOVL (R A4) (D@R A5 -3))
NARY-TEST
    (SUBL2 (R NARGS) ($ 1))
    (CMPL (R NARGS) (R S1))
    (BGTR NARY-LOOP)                        ; last (first) argument is in
                                            ; T(required + 1)
    (RET))



;;; XP holds list to spread
;;;
APPLY

    (CMPL (R NARGS) ($ 3))
    (BEQL APPLY-ONE-ARG)
    (CMPL (R NARGS) ($ 4))
    (BEQL APPLY-TWO-ARGS)
    (CMPL (R NARGS) ($ 5))
    (BEQL APPLY-THREE-ARGS)
    (CMPL (R NARGS) ($ 6))
