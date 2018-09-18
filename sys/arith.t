(herald (tsys arith t 70)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Arithmetical and mathematical and generical

;;; Basic predicates

(define (integer? x)
  (or (fixnum? x)
      (and (extend? x) (bignum? x))))

(define (number? x)
  (or (fixnum? x)
      (flonum? x)
      (and (extend? x)
           (true? (extended-number-type x)))))

(define float? flonum?)

(define real? number?)

(define short-float? false)	;Fix later for T3

;;; (define complex? number?)

(define-constant %%fixnum-number-type 0)
(define-constant %%flonum-number-type 1)
(define-constant %%bignum-number-type 2)
(define-constant %%ratio-number-type  3)

(define-constant %%number-of-number-types 4)

(define-operation (extended-number-type obj) nil)

(define-integrable (number-type obj op)
  (select (pointer-tag obj)
    ((%%fixnum-tag) %%fixnum-number-type)
    ((%%flonum-tag) %%flonum-number-type)
    ((%%extend-tag) (cond ((extended-number-type obj) => identity)
                          (else (losing-number-type obj op))))
    (else (losing-number-type obj op))))

(define (losing-number-type obj op)
  (number-type (error "non-numeric argument~%  (~S ... ~S ...)" op obj)
                op))

(define-operation (set-dispatch generic-op-frob type1 type2 procedure))

(comment
(define (make-one-arg-number-routine identifier)
  (let ((table (make-vector %%number-of-number-types)))
    (vector-fill table
                 (lambda (n)
                   (error "generic number routine not defined for this ~
                           number type~%  (~S ~S)"
                          identifier n)))
    (object (lambda (n)
              ((vref table (number-type n identifier)) n))
            ((set-dispatch-1 self type proc)
             (vset table type (unguarded-version proc)))
            ((identification self) identifier))))
)

(define (make-two-arg-number-routine identifier)
  (let ((table (make-vector %%number-of-number-types)))
    (let ((lose
           (lambda (n1 n2)
             (error "generic number routine not defined for this ~
                     combination of types~%  (~S~_~S~_~S)"
                    identifier n1 n2))))
      (do ((i 0 (fx+ i 1)))
          ((fx>= i %%number-of-number-types)
           (object (lambda (n1 n2)
                     ((vref (vref table (number-type n1 identifier))
                            (number-type n2 identifier))
                      n1 n2))
                   ((set-dispatch self type1 type2 proc)
                    (vset (vref table type1) type2 (unguarded-version proc)))
                   ((identification self) identifier)))
        (let ((v (make-vector %%number-of-number-types)))
          (vset table i v)
          (vector-fill v lose))))))

(define (make-assoc-commute-op table identifier arithmetic-identity)
  (object (lambda x 
            (cond ((null? x)
                   arithmetic-identity)
                  ((null? (cdr x))
                   (number-type (car x) identifier)
                   (car x))
                  (else
                   (do ((args (cdr x) (cdr args))
                        (result (car x) (table result (car args))))
                       ((null? args)
                        result)))))
          ((identification self) identifier)))


;;; The dispatch tables

(define %add      (make-two-arg-number-routine 'add))
(define %subtract (make-two-arg-number-routine 'subtract))
(define %multiply (make-two-arg-number-routine 'multiply))
(define %divide   (make-two-arg-number-routine 'divide))
(define %div      (make-two-arg-number-routine 'div))
(define %less?    (make-two-arg-number-routine 'less?))
(define %equal?   (make-two-arg-number-routine 'equal?))

(define (set-dispatches type1 type2 + - * / div < =)
  (set-dispatch %add      type1 type2 +)
  (set-dispatch %subtract  type1 type2 -)
  (set-dispatch %multiply type1 type2 *)
  (set-dispatch %divide    type1 type2 /)
  (set-dispatch %div       type1 type2 div)
  (set-dispatch %less?     type1 type2 <)
  (set-dispatch %equal?    type1 type2 =))

;;; Fixnum hackery

(define (fixnum-expt x y)
  (cond ((fx< y 0) (ratio 1 (fixnum-expt x (fx- 0 y))))
        ((fx= y 0) 1)
        (else (fx* x (fixnum-expt x (fx- y 1))))))

(define (fixnum-bit-field fixnum start count)
  (fixnum-logand (fixnum-lognot (fixnum-ashl -1 count))
                 (fixnum-ashr fixnum start)))

(define (set-fixnum-bit-field fixnum start count val)
  (let ((val (fixnum-bit-field val 0 count)))   ; chop off VAL high bits
    (fixnum-logior                      ; or field into target
     (fixnum-ashl val start)            ; move field to appropriate pos
                                        ; zero field in target
     (fixnum-logand (fixnum-logior (fixnum-lognot (fixnum-ashl -1 start))
                                   (fixnum-ashl -1 (fx+ start count)))
                    fixnum))))

;;; The entrypoints themselves

(define add      (make-assoc-commute-op %add 'add 0))
(define subtract %subtract)
(define multiply (make-assoc-commute-op %multiply 'multiply 1))
(define divide   %divide)
(define div      %div)

(define (negate x) (subtract 0 x))

(define + add)
(define (- x . y) 
  (cond ((null? y) (negate x))
        ((null? (cdr y)) (subtract x (car y)))
        (else (error "wrong number of arguments to procedure~%  ~S"
		     `(- ,x . ,y)))))
(define * multiply)
(define / divide)

(define (add1      x) (%add     x 1))
(define (subtract1 x) (subtract x 1))

(define  1+ add1)
(define -1+ subtract1)
(define (=1? x) (= x 1))

(define less?              %less?)
(define (not-less? x y)    (not (%less? x y)))
(define equal?             %equal?)
(define (not-equal? x y)   (not (%equal? x y)))
(define (greater? x y)     (%less? y x))
(define (not-greater? x y) (not (%less? y x)))

(define <  less?)
(define <= not-greater?)
(define =  equal?)
(define N= not-equal?)
(define >  greater?)
(define >= not-less?)

(define (negative? x)     (< x 0))
(define (zero? x)         (= x 0))
(define (positive? x)     (> x 0))
(define (not-negative? x) (>= x 0))
(define (not-zero? x)     (N= x 0))
(define (not-positive? x) (<= x 0))

(define <0?  negative?)
(define =0?  zero?)
(define >0?  positive?)
(define >=0? not-negative?)
(define n=0? not-zero?)
(define <=0? not-positive?)

(define (div2 x y values)
  (cond ((fixnum? y)
         (cond ((fixnum? x)
                (values (fx/ x y) (fixnum-remainder x y)))
               ((bignum? x)
                (b-f-div2 x y values))
               (else
                (div2-kludgily x y values))))
        ((bignum? y)
         (cond ((fixnum? x) (values 0 x))
               ((bignum? x) (bignum-div2 x y values))
               (else        (div2-kludgily x y))))
        (else (div2-kludgily x y))))

(define (div2-kludgily x y values)
  (let ((q (div x y)))
    (values q (subtract x (%multiply q y)))))

(define (remainder-kludgily x y)
  (div2-kludgily x y proj1))

(define (remainder x y)    ; Kludge - add to dispatch table later.
  (cond ((fixnum? y)
         (cond ((fixnum? x) (fixnum-remainder x y))
               ((bignum? x) (b-f-remainder x y))
               (else        (remainder-kludgily x y))))
        ((bignum? y)
         (cond ((fixnum? x)
                (if (and (fx= x *min-fixnum*)       ;Thanks to Joe Stoy!
                         (= y (negate *min-fixnum*)))
                    0
                    x))
               ((bignum? x) (bignum-remainder x y))
               (else        (remainder-kludgily x y))))
        (else (remainder-kludgily x y))))

(define (odd? x) 
  (odd?-aux x odd?))

(define (even? x) 
  (not (odd?-aux x even?)))

(define (odd?-aux x who)
  (let ((x (check-arg integer? x who)))
    (cond ((fixnum? x) (fixnum-odd? x))
          (else        (bignum-odd? x)))))

(define-integrable (nonnegative-integer? n)
  (and (integer? n) (not-negative? n)))

(define-integrable (fixnum-length x)
  (if (fx>= x 0)
      (fixnum-howlong x)
      (fixnum-howlong (fx- -1 x))))

(define (integer-length x) 
  (let ((x (check-arg integer? x integer-length)))
    (cond ((fixnum? x)
	   (fixnum-length x))
          (else
	   (if (>= x 0)
	       (bignum-howlong x)
	       (bignum-howlong (subtract -1 x)))))))

(define (ash num amount)
  (%ash (check-arg nonnegative-integer? num ash)
        (check-arg fixnum? amount ash)))

(define (%ash num amount)           ; See PRINT-FLONUM
  (cond ((fixnum? num)
         (cond ((fx> amount 0)
                (let ((num-length (integer-length num)))
                  (let ((result-length (fx+ num-length amount)))
                    (cond ((fx> result-length *u-bits-per-fixnum*)
                           (fixnum-ashl-bignum num amount))
                          (else
                           (fixnum-ashl num amount))))))
               (else
                (fixnum-ashr num (fx- 0 amount)))))
        (else
         (cond ((fx> amount 0)
                (bignum-ashl num amount))
               ((fx= amount 0)
                num)
               (else
                (let ((amount (fx- 0 amount))
                      (num-length (integer-length num)))
                  (let ((result-length (fx- num-length amount)))
                    (cond ((fx> result-length *u-bits-per-fixnum*)
                           (bignum-ashr num amount))
                          ((fx<= result-length 0) 0)
                          (else
                           (bignum-ashr-fixnum num amount))))))))))

(define logand fixnum-logand)           ; Temporary
(define logior fixnum-logior)
(define logxor fixnum-logxor)
(define lognot fixnum-lognot)

(define bit-field     fixnum-bit-field) ; Temporary
(define set-bit-field set-fixnum-bit-field)

(define (max2 n1 n2)
  (if (greater? n1 n2) n1 n2))

(define (max number . numbers)
  (do ((n numbers (cdr n))
       (result number (max2 result (car n))))
      ((null? n) result)))

(define (min2 n1 n2)
  (if (less? n1 n2) n1 n2))

(define (min number . numbers)
  (do ((n numbers (cdr n))
       (result number (min2 result (car n))))
      ((null? n) result)))

(define (abs n) (if (less? n 0) (negate n) n))

;;; Raise any number to a fixnum power > 1.

(define (raise-to-fixnum-power base power)
  (do ((bit (fixnum-ashl 1 (fx- (fixnum-howlong power) 2))
            (fixnum-ashr bit 1))
       (result base (let ((result^2 (%multiply result result)))
                      (if (fx= (fixnum-logand power bit) 0)
                          result^2
                        (%multiply result^2 base)))))
      ((fx= bit 0) result)))

;;; (define (foo base power)
;;;   (do ((p power (fixnum-ashr p 1))
;;;        (temp base (* temp temp))
;;;        (result 1 (if (fixnum-odd? p) (* temp result) result)))
;;;       ((fx= p 0) result)))

;;; Has to deal with flonums

(define (expt x y)
  (let ((x (check-arg number? x expt))
        (y (check-arg fixnum? y expt)))
    (cond ((fx= y 1) x)
          ((fx< y 0) (/ 1 (expt x (fx- 0 y))))
          ((fx= y 0) 1)                 ; ??? if x is float, should return 1.0?
          ((not (fixnum? x)) (raise-to-fixnum-power x y))
          ((fx= x 0) 0)
          ((fx= x 1) 1)
          ((fx= x -1) (if (fixnum-odd? y) -1 1))
          (else (raise-to-fixnum-power x y)))))

;;; Euclid's algorithm.  Binary GCD would probably be better, esp. on machines
;;; like the 68000 that lack divide instructions.

(define (gcd p q)
  (let ((p (abs p))
        (q (abs q)))
    (cond ((equal? q 0) p)
          (else (gcd q (remainder p q))))))

(comment                                ; TC loses on this one.
 (define (gcd p q)
   (do ((p (abs p) q)
        (q (abs q) (remainder p q)))
     ((equal? q 0) p)))
 )

;;; Canonical representative of X's residue class mod Y
;;; Awful, awful kludgey definition.

(define (mod x y)
  (cond ((< x 0)
         (subtract (subtract y 1) (remainder (subtract -1 x) y)))
        (else (remainder x y))))

;;; Return largest multiple N of Y such that N <= X
;;; Awful, awful kludgey definition.

(define (floor x y)
  (subtract x (mod x y)))

;;; Return smallest multiple N of Y such that N >= X
;;; Awful, awful kludgey definition.

(define (ceiling x y)
  (floor (%add x (subtract y 1)) y))

;;; Coerce to integer.
;;; Awful, awful kludgey definition.

(define (->integer x)
  (cond ((integer? x) x)
        ((flonum? x) (flonum->integer x))
        ((ratio? x) (div (numerator x) (denominator x)))
        (else (->integer (error "can't coerce to integer~%  (~S ~S)"
                                '->integer x)))))

;;; Coerce to floating point number.
;;; Awful, awful kludgey definition.

(define (->float x)
  (cond ((float? x) x)
        ((fixnum? x) (fixnum->flonum x))
        ((bignum? x) (bignum->flonum x))
        ((ratio? x) (flonum-divide (->float (numerator x)) 
                                   (->float (denominator x))))
        (else
         (->float (error "can't coerce to floating point number~%  (~S ~S)"
                         '->float x)))))

(define (flonum->integer x)
  (cond ((fl> x (fixnum->flonum *max-fixnum*))
         (integer-decode-float x
           (lambda (sig exp)
             (ash sig exp))))
        ((fl< x (fixnum->flonum *min-fixnum*))
         (integer-decode-float x
           (lambda (sig exp)
             (let ((n (ash sig exp)))
               (if (bignum? n) (bignum-negate! n) (fx- 0 n))))))
        (else (flonum->fixnum x))))

(define (flonum-div x y)
  (flonum->integer (flonum-divide x y)))

(define integer->flonum fixnum->flonum)
