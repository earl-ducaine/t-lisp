(HERALD (TSYS RATIO T 29)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

(DEFINE (RATIO P Q)
  (LET ((P (CHECK-ARG INTEGER? P RATIO))
        (Q (CHECK-ARG INTEGER? Q RATIO))
        (NORMAL (LAMBDA (P Q)
                  (LET ((G (GCD P Q)))
                    (LET ((P (DIV P G))
                          (Q (DIV Q G)))
                      (COND ((= Q 1) P)
                            (ELSE (OBJECT NIL
                                       ((EXTENDED-NUMBER-TYPE SELF)
                                        %%RATIO-NUMBER-TYPE)
                                       ((RATIO? SELF) T)
                                       ((NUMERATOR SELF) P)
                                       ((DENOMINATOR SELF) Q)
                                       ((PRINT SELF STREAM)
                                        (FORMAT STREAM "~S~C~S"
                                                P *RATIO-CHAR* Q))))))))))
    ;; ... put P & Q in lowest terms ...
    (COND ((= Q 0) (ERROR "attempt to divide by zero~%  (/ ~S ~S)" P Q))
          ((< Q 0) (NORMAL (- 0 P) (- 0 Q)))
          (ELSE (NORMAL P Q)))))

(DEFINE-PREDICATE RATIO?)

(DEFINE (RATIONAL? X)
  (OR (INTEGER? X) (RATIO? X)))

(DEFINE-OPERATION (NUMERATOR X)
  (COND ((INTEGER? X) X)
        (ELSE (ERROR "cannot take NUMERATOR of non-integer ~S" X))))

(DEFINE-OPERATION (DENOMINATOR X)
  (COND ((INTEGER? X) 1)
        (ELSE (ERROR "cannot take DENOMINATOR of non-integer ~S" X))))

(DEFINE (RATIONAL-PARTS-ADD N1 D1 N2 D2)
  (RATIO (+ (* N1 D2) (* N2 D1))
         (* D1 D2)))

(DEFINE (RATIONAL-PARTS-SUBTRACT N1 D1 N2 D2)
  (RATIO (- (* N1 D2) (* N2 D1))
         (* D1 D2)))

(DEFINE (RATIONAL-PARTS-MULTIPLY N1 D1 N2 D2)
  (RATIO (* N1 N2) 
         (* D1 D2)))

(DEFINE-INTEGRABLE (RATIONAL-PARTS-DIVIDE N1 D1 N2 D2)
  (RATIONAL-PARTS-MULTIPLY N1 D1 D2 N2))

(DEFINE (RATIONAL-PARTS-DIV N1 D1 N2 D2)
  (DIV (* N1 D2) 
       (* N2 D1)))

;;; Hacked for consistency

(DEFINE (RATIONAL-ADD R1 R2) 
  (RATIONAL-OP RATIONAL-PARTS-ADD R1 R2))

(DEFINE (RATIONAL-SUBTRACT R1 R2) 
  (RATIONAL-OP RATIONAL-PARTS-SUBTRACT R1 R2))

(DEFINE (RATIONAL-MULTIPLY R1 R2) 
  (RATIONAL-OP RATIONAL-PARTS-MULTIPLY R1 R2))

(DEFINE (RATIONAL-DIVIDE R1 R2) 
  (RATIONAL-OP RATIONAL-PARTS-DIVIDE R1 R2))

(DEFINE (RATIONAL-DIV R1 R2) 
  (RATIONAL-OP RATIONAL-PARTS-DIV R1 R2))

(DEFINE (RATIONAL-LESS? R1 R2)
  (< (* (NUMERATOR R1) (DENOMINATOR R2)) (* (NUMERATOR R2) (DENOMINATOR R1))))

(DEFINE (RATIONAL-EQUAL? R1 R2)
  ;; (= (* (NUMERATOR R1) (DENOMINATOR R2)) (* (NUMERATOR R2) (DENOMINATOR R1)))
  ;; Assume normalization.
  (AND (= (NUMERATOR R1)   (NUMERATOR R2))
       (= (DENOMINATOR R2) (DENOMINATOR R1))))

(DEFINE (RATIONAL-OP PROC R1 R2)
  (PROC (NUMERATOR R1)
        (DENOMINATOR R1)
        (NUMERATOR R2)
        (DENOMINATOR R2)))

;;; Coercers

(DEFINE (RATIO->FLONUM R)
  (FLONUM-DIVIDE (INTEGER->FLONUM (NUMERATOR R))
                 (INTEGER->FLONUM (DENOMINATOR R))))
