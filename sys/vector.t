(HERALD (TSYS VECTOR T 15)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Vector-handling routines

(DEFINE (VECTOR-REPLACE TARGET SOURCE LEN)
  (%VECTOR-REPLACE (CHECK-ARG VECTOR? TARGET VECTOR-REPLACE)
                   (CHECK-ARG VECTOR? SOURCE VECTOR-REPLACE)
                   (CHECK-ARG NONNEGATIVE-FIXNUM? LEN VECTOR-REPLACE))) ; almost

(DEFINE (COPY-VECTOR VECTOR)            ; See SYSTEM.T for def of %COPY-VECTOR
  (%COPY-VECTOR (CHECK-ARG VECTOR? VECTOR COPY-VECTOR)))

(DEFINE (COPY-BYTEV BYTEV)
  (%COPY-BYTEV (CHECK-ARG BYTEV? BYTEV COPY-BYTEV)))

(DEFINE (LIST->VECTOR L)
  (LET ((L (CHECK-ARG LIST? L LIST->VECTOR)))
    (LET ((LEN (LENGTH L)))
      (LET ((VEC (MAKE-VECTOR LEN)))
        (DO ((I 0 (FX+ I 1))
             (L L (CDR L)))
            ((FX= I LEN) VEC)
          (VSET VEC I (CAR L)))))))

(DEFINE (VECTOR->LIST V)
  (LET ((V (CHECK-ARG VECTOR? V VECTOR->LIST)))
    (DO ((I (FX- (VECTOR-LENGTH V) 1) (FX- I 1))
         (L '() (CONS (VREF V I) L)))
        ((FX< I 0) L))))

(DEFINE (VECTOR-POS PRED THING VECTOR)
  (LET ((LEN (VECTOR-LENGTH VECTOR)))
    (ITERATE LOOP ((I 0))
      (COND ((FX>= I LEN) NIL)
            ((PRED THING (VREF VECTOR I)) I)
            (ELSE (LOOP (FX+ I 1)))))))

(DEFINE-INTEGRABLE (VECTOR-POSQ THING VECTOR) (VECTOR-POS EQ? THING VECTOR))

(DEFINE (WALK-VECTOR FN VEC)
  (LET ((VEC (CHECK-ARG VECTOR? VEC WALK-VECTOR)))
    (LET ((LIMIT (FX- (VECTOR-LENGTH VEC) 1)))
      (COND ((FX>= LIMIT 0)
             (ITERATE LOOP ((I 0))
               (COND ((FX>= I LIMIT) 
                      (FN (VREF VEC I)))
                     (ELSE
                      (FN (VREF VEC I))
                      (LOOP (FX+ I 1))))))))))

(DEFINE (VECTOR . ELTS)		;for NIA
  (LIST->VECTOR ELTS))
