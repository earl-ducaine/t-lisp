(herald (tsys bignum t 223)
        (env tsys))

;;; (c) Copyright 1983, 1984 Yale University

;;; To do:
;;;   destructive routines
;;;   rewrite bignum-stringify & associates (format nil ... etc)
;;;   pool bignums
;;;   bignum templates, positve & negative ?
;;;   pack densely - use hardware multiply, etc


;;; Data concretions:

(define-structure-type bignum sign vector)

(set (bignum-sign (stype-master bignum-stype)) 1)

(define-integrable (bigset b n val)
  (vset (bignum-vector b) n val))

(define-integrable bigref
  (if-integrated (lambda (b n)
                   (vref (bignum-vector b) n))
                 (object (lambda (b n)
                           (vref (bignum-vector b) n))
                         ((setter self) bigset))))

(lset *bignum-cons-counter* 0)
(lset *bignum-cons-size-counter* 0)

;;; Depend on fact that vectors elements are initially zero.

(define (create-bignum j)
  (set *bignum-cons-counter* (fx+ *bignum-cons-counter* 1))
  (set *bignum-cons-size-counter* (fx+ *bignum-cons-size-counter* j))
  (let ((b (make-bignum)))
    (set (bignum-vector b) (make-vector j))
    b))

(define-integrable (set-bignum-size b size)
  (set (vector-length (bignum-vector b)) size))

(define-integrable bignum-size
  (if-integrated (lambda (b)
                   (vector-length (bignum-vector b)))
                 (object (lambda (b)
                           (vector-length (bignum-vector b)))
                         ((setter self) set-bignum-size))))

(declare-setter bignum-size set-bignum-size)

(define-integrable (bignum-positive? num)
  (fx> (bignum-sign num) 0))

;;; sign must be either 1 or -1.

(define-integrable (set-bignum-sign num sign)
  (set (bignum-sign num) sign)
  num)

(define-integrable (bignum-negate! num)
  (set (bignum-sign num) (fixnum-negate (bignum-sign num)))
  num)

(define-methods handle-bignum
  ((print self stream)
   (print-bignum self stream))
  ((extended-number-type self)
   %%bignum-number-type))


;;; Constants:

(define-constant *bits-per-hyperdigit* 13)      ; Should be 14 or 15 or 28

(define-constant *bits-per-fixnum* 29)
(define-constant *u-bits-per-fixnum* (fx- *bits-per-fixnum* 1))

;;; The following will lose if *max-hyperdigit* >= *max-fixnum*.

(define-constant *max-hyperdigit*
  (fx- (fixnum-ashl 1 *bits-per-hyperdigit*) 1))

(define-constant *half-max-hyperdigit*
  (fx- (fixnum-ashl 1 (fx- *bits-per-hyperdigit* 1)) 1))

(comment                                ; Can't compile for vax
(define-constant *max-fixnum*
  (do ((prev 0 n)
       (n 1 (fx+ (fx* n 2) 1)))
      ((fx< n 0) prev)))
)

(define-constant *min-fixnum* (fixnum-ashl 1 *u-bits-per-fixnum*))

(define-constant *max-fixnum* (fx- *min-fixnum* 1))


;;; Random general utilities:

(define (make-and-replace-bignum size old i1 i2 count)
  (let ((new (create-bignum size)))
    (do ((i 0 (fx+ i 1)))
        ((fx>= i count)
         new)
      (bigset new (fx+ i1 i) (bigref old (fx+ i2 i))))))

(define (copy-bignum old)
  (let* ((len (bignum-size old))
         (new (make-and-replace-bignum len old 0 0 len)))
    (set-bignum-sign new (bignum-sign old))))

;;; Given a bignum of length at least l, truncates that
;;; vector, eliminating the leading zeros.  This is destructive

(define (bignum-trim! num . foo)
  (do ((i (fx- (bignum-size num) 1) (fx- i 1)))
      ((or (fx<= i 0)
           (not (fx= (bigref num i) 0)))
       (set (bignum-size num) (fx+ i 1))
       num)))


;;; Basic arithmetic primitives:

;;; Add two hyperdigits and a carry bit, returning sum and new carry.

(define-integrable (%digit-add u v carry values)
  (let* ((sum (fx+ (fx+ u v) carry))
         (d (fixnum-logand sum *max-hyperdigit*)))
    (values d (if (fx= d sum) 0 1))))

;;; Subtract two hyperdigits and carry, returning difference and new carry.

(define-integrable (%digit-subtract u v carry values)
  (let* ((diff (fx- (fx- u v) carry))
         (d (fixnum-logand diff *max-hyperdigit*)))
    (values d (if (fx= d diff) 0 1))))

;;; Multiply two hyperdigits, returning low and high digits of product.

(define-integrable (%digit-multiply u v values)
  (let ((kludge (fx* u v)))
    (values (fixnum-logand kludge *max-hyperdigit*)
            (fixnum-ashr kludge *bits-per-hyperdigit*))))

;;; Temporary hack

(define-integrable (%digit-divide uhigh ulow v values)
  (let* ((u (fx+ (fixnum-ashl uhigh *bits-per-hyperdigit*) ulow))
         (q (fx/ u v)))
    (values q
            (fx- u (fx* v q)))))


;;; Addition:

;;; Places the modular sum of u, v, and carry, in element i of
;;; vector destv.  Returns the new carry.

(define-integrable (add-step u v destv i carry)
  (%digit-add u v carry
     (lambda (sum carry)
       (bigset destv i sum)
       carry)))

;;; add-magnitudes takes two bignums, adds their magnitudes, and returns
;;; the magnitude of their sum.

(define (add-magnitudes u v)
  (cond ((fx< (bignum-size u) (bignum-size v))
         (really-add-magnitudes v u))
        (else
         (really-add-magnitudes u v))))

;;; Assumes that first number is longer than second number.

(define (really-add-magnitudes u v)
  (let* ((u-size (bignum-size u))
         (v-size (bignum-size v))        ; u-size >= v-size
         ;; The following test tries to predict whether there will be carryout.
         (lc (if (or (fx> (bigref u (fx- u-size 1)) *half-max-hyperdigit*)
                     (and (fx= u-size v-size)
                          (fx> (bigref v (fx- v-size 1)) *half-max-hyperdigit*)))
                 (fx+ 1 u-size)
                 u-size))
         (sum (create-bignum lc)))
    (do ((carry 0 (add-step (bigref u i) (bigref v i) sum i carry))
         (i 0 (fx+ 1 i)))
        ((fx= i v-size)
         ;; Propagate carry
         (do ((carry carry
                     (add-step (bigref u i) 0 sum i carry))
              (i i (fx+ 1 i)))
             ((or (fx= i u-size) (fx= carry 0))
              ;; Copy remaining digits from u
              (do ((i i (fx+ 1 i)))
                  ((fx= i u-size)
                   (cond ((fx= 0 carry)
                          (set (bignum-size sum) u-size))
                         (else
                          (bigset sum i 1)))
                   sum)
                (bigset sum i (bigref u i)))))))))

;;; Subtraction:

(define-integrable (subtract-step u v destv i carry)
  (%digit-subtract u v carry
     (lambda (sum carry)
       (bigset destv i sum)
       carry)))

;;; Returns the difference of the magnitudes of two bignums.
;;; Assumes that first number is at least as long as than second.

(define (subtract-magnitudes u v)
  (let* ((la (bignum-size u))
         (lb (bignum-size v))
         (diff (create-bignum la)))
    (do ((carry 0 (subtract-step (bigref u i) (bigref v i) diff i carry))
         (i 0 (fx+ 1 i)))
        ((fx= i lb)
         (do ((carry carry (subtract-step (bigref u i) 0 diff i carry))
              (i i (fx+ 1 i)))
             ((or (fx= la i) (fx= carry 0))
              (do ((i i (fx+ 1 i)))
                  ((fx= la i)
                   (bignum-trim! diff))
                (bigset diff i (bigref u i)))))))))


;;; Multiplication:

;;; Multiplies a vector by a scalar, and adds the product into an
;;; accumulator beginning at the specified start index.
;;; Assumes that accum is at least of length v-size + start + 1.

(define (multiply-step scalar v v-size accum start)
  (do ((carry 0 (%digit-multiply scalar (bigref v j)
                  (lambda (d0 d1)
                    (add-step (bigref accum (fx+ 1 i))
                              (fx+ d1 carry)  ; Note: d1 < *max-hyperdigit*
                              accum
                              (fx+ 1 i)
                              (add-step (bigref accum i) d0 accum i 0)))))
       (i start (fx+ 1 i))
       (j 0 (fx+ 1 j)))
      ((fx>= j v-size)
       (cond ((fxn= carry 0)
              (add-step (bigref accum (fx+ 1 j)) 0 accum (fx+ 1 j) carry)))
       accum)))

;;; Similar in action to ADD-MAGNITUDES only with product.

(define (multiply-magnitudes u v)
  (let* ((u-size (bignum-size u))
         (v-size (bignum-size v))
         (l-product (fx+ (fx+ u-size v-size) 1))       ;Why + 1?
         (product (create-bignum l-product)))
    (do ((i 0 (fx+ 1 i)))
        ((fx= i u-size) (bignum-trim! product))
      (multiply-step (bigref u i) v v-size product i))))


;;; Division:

;;; The New Division Routine by J.R.
;;; (In a division u/v, u is the dividend and v is the divisor.)

(define (div2-magnitudes u v values)
  (let* ((m+n (bignum-size u))
         (n (bignum-size v))
         (d (fx- *bits-per-hyperdigit*
                 (fixnum-howlong (bigref v (fx- n 1)))))
         (new-u (make-and-replace-bignum (fx+ m+n 1) u 0 0 m+n)))
    ;; Normalize
    (cond ((fx> d 0)
           (really-div2-magnitudes (bignum-ashl! new-u d)
                                     (bignum-ashl v d)
                                     m+n n d values))
          (else
           (really-div2-magnitudes new-u v m+n n d values)))))

;;; Knuth equivalences:
;;;   u[j]     ->   (bigref u m+n-j)
;;;   u[j+1]   ->   (bigref u (- m+n-j 1))
;;;   u[j+n]   ->   (bigref u (- m-j))

(define-constant *div-debug?* nil)

(define (really-div2-magnitudes u v m+n n d values)
  (let* ((v1 (bigref v (fx- n 1)))
         (v2 (bigref v (fx- n 2)))
         (m (fx- m+n n))
         (q (create-bignum (fx+ m 1))))
    (cond (*div-debug?*
           (format t "~% m = ~s, n = ~s, d = ~s~%"
                         m       n       d)
           (format t " u = ~S~% v = ~s~%"
                   (reverse! (vector->list (bignum-vector u)))
                   (reverse! (vector->list (bignum-vector v))))))
    (do ((m-j   m   (fx- m-j   1))
         (m+n-j m+n (fx- m+n-j 1)))
        ((fx< m-j 0)
         (values (bignum-trim! q)
                 (bignum-trim! (bignum-ashr! u d))))

      ;; Calculate one digit of the quotient.
      (let ((q^ (compute-q^ (bigref u m+n-j)
                            (if (fx> m+n-j 0) (bigref u (fx- m+n-j 1)) 0)
                            (if (fx> m+n-j 1) (bigref u (fx- m+n-j 2)) 0)
                            v1
                            v2)))

        (cond (*div-debug?*
               (format t "~% q[~s] := ~s~%" m-j q^)))

        (bigset q m-j q^)

        ;; Multiply and subtract: (uj...uj+n)  -:=  q^ * (v1...vn)
        (iterate loop ((prev 0)
                       (borrow 0)
                       (i m-j)
                       (k 0))
          (cond ((fx< k n)
                 (%digit-multiply q^ (bigref v k)
                   (lambda (d0 d1)
                    (if *div-debug?*
                     (format t "   ~s * ~s = {~s ~s}~%" q^ (bigref v k) d1 d0))
                     (%digit-add d0 prev 0
                       (lambda (sum carry)
                         (let ((d1 (fx+ d1 carry)))
                           (%digit-subtract (bigref u i) sum borrow
                             (lambda (diff borrow)
                               (if *div-debug?*
                                (format t "  u[~s] := ~s (~s-)~%" i diff sum))
                               (bigset u i diff)
                               (loop d1
                                     borrow
                                     (fx+ i 1)
                                     (fx+ k 1))))))))))

                (else
                 ;; i should equal m+n-j at this point.  Frob leftmost digit.
                 (%digit-subtract (bigref u i) prev borrow
                   (lambda (diff borrow)
                     (if *div-debug?*
                         (format t "  u[~s] := ~s (~s-)~%" i diff prev))
                     (bigset u i diff)

        (cond (*div-debug?*
               (format t " u = ~S~%"
                       (reverse! (vector->list (bignum-vector u))))))

                     (cond ((fxn= borrow 0)
                            (add-back u v q q^ n m-j))
                           ((fxn= diff 0)
                            (error "inconsistency in bignum division!~%  ~S"
                                   `(div ,u ,v)))
                           ))))))))))

;;; In the loop, q^ starts out being 0, 1, or 2 larger than the actual
;;; first digit of the quotient of the bignums u and v.
;;; The loop may decrement q^, eliminating all cases where q^ is
;;; two larger, and most cases where it is one larger.

;;; The initial guess for q^ is obtained by dividing u's first two digits
;;; by v's first digit.
;;; r^ is initially the remainder of the division, but as q^ is
;;; decremented, r^ maintains as its value the actual difference between
;;; the dividend  {u[j] u[j+1]}  and the product  q^*v[1].
;;; We stop pruning q^ as soon as its product with the second digit
;;; of v, exceeds r^ adjoined with the third digit of u.

(define (compute-q^ uj uj+1 uj+2 v1 v2)
  (labels (((loop q^ r^)
            ;; Test to see whether, in Knuth's notation,
            ;;  q^*v[2] > r^*b + u[j+2]
            ;;     where r^ = u[j]*b + u[j+1] - q^*v[1]
            ;; We use the same tricks he does in his MIX code.
            (%digit-multiply q^ v2
              (lambda (a0 a1)
                ;; {a1 a0}  =  v[2]*q^
                (cond ((and (fx<= a1 r^)
                            (or (fxn= a1 r^)
                                (fx<= a0 uj+2)))
                       q^)
                      (else
                       (test (fx- q^ 1) r^))))))
           ((test q^ r^)
            ;; Adjust r^.  q^ must get no smaller if r^ overflows here.
            (%digit-add r^ v1 0
              (lambda (sum carry)
                (if (fxn= carry 0)
                    q^
                    (loop q^ sum))))))
    ;; uj <= v1
    (cond ((fx= uj v1)
           ;; Note that in this case, uj+1 <= v2 also.  E.g. in
           ;; (/ 8123,4567,... 8123,xxxx,...) -> q= FFFF, r= C68A (=8123+4567)
           ;; we know that xxxx >= 4567.
           (test *max-hyperdigit* uj+1))
          (else
           (%digit-divide uj uj+1 v1
             (lambda (q^ r^) (loop q^ r^)))))))

;;; We come here if compute-q^ screwed up and gave us a bogus guess for
;;; the quotient digit.  The probability of this happening is about
;;;  2 / b  where b = (1+ *max-hyperdigit*).

(define (add-back u v q q^ n m-j)
  (*let-us-know* u v q^)
  (if *div-debug?* (format t " Add back: q[~s] := ~s~%" m-j (fx- q^ 1)))
  (bigset q m-j (fx- q^ 1))
  (iterate loop ((carry 0)
                 (i m-j)
                 (k 0))
    (cond ((fx< k n)
           (%digit-add (bigref u i) (bigref v k) carry
              (lambda (sum carry)
                (if *div-debug?* (format t " u[~s] := ~s~%" i sum))
                (bigset u i sum)
                (loop carry (fx+ i 1) (fx+ k 1)))))
          (else
           (bigset u i 0)))))

(lset *let-us-know*
  (lambda (u v q^)
    (if (experimental?)
        (format (terminal-output)
                '("~&;Please send the following numbers to the T implementors.~%"
                  ";This is not an error.~%; q^ = ~s~%; u  = ~s~%; v  = ~s~%")
                q^ u v))
    (set *let-us-know* false)))

;;; Division of a bignum by a fixnum.

(define (b-f-div2 u v values)
  (cond ((or (fx> v        *max-hyperdigit*)
             (fx< v (fx- 0 *max-hyperdigit*)))
         (bignum-div2 u (fixnum->bignum v) values))
        ((fx= v 0)
         (error "division by zero~%  (DIV ~s ~s)" u v))
        (else
         (%b-f-div2 u v t values))))

;;; An unnormalized result is required by BIGNUM-STRINGIFY
;;; and INTEGER->POINTER.

(define-integrable (b-f-div2-unnormalized u v values)
  (%b-f-div2 u v nil values))

(define (%b-f-div2 u v normalize? values)
  (let* ((v-abs (fixnum-abs v))
         (d (fx- *bits-per-hyperdigit*
                 (fixnum-howlong v-abs)))
         (sign (if (fx= (bignum-sign u)
                        (if (fx< v 0) -1 1))
                   1
                   -1)))
    (cond ((fx= d 0)
           (b-f-div2-aux u v-abs normalize? d sign values))
          (else
           (b-f-div2-aux (bignum-ashl u d)
                         (fixnum-ashl v-abs d)
                         normalize?
                         d
                         sign
                         values)))))

(define (b-f-div2-aux u v normalize? d sign values)
  (let* ((u-size (bignum-size u))
         (q (create-bignum u-size)))
    (iterate loop ((i (fx- u-size 1))
                   (r 0))
      (cond ((fx< i 0)
             ;; Last time through, r is the remainder.
             (set (bignum-sign q) sign)
             (bignum-trim! q)
             (values (if normalize? (normalize-integer q) q)
                     (let ((r (fixnum-ashr r d)))
                       (if (bignum-positive? u) r (fx- 0 r)))))
            (else
             (let ((word0 (bigref u i)))
               (%digit-divide r word0 v
                 (lambda (qi r)
                   (bigset q i qi)
                   (loop (fx- i 1) r)))))))))


;;; Comparison:

;;; Returns a fixnum whose sign is the same as (- |u| |v|).

(define (bignum-compare-magnitudes u v)
  (let ((u-size (bignum-size u))
        (v-size (bignum-size v)))
    (cond ((fx= u-size v-size)
           (iterate loop ((i (fx- u-size 1)))
             (let ((diff (fx- (bigref u i)
                              (bigref v i))))
               (cond ((and (fxn= i 0) (fx= diff 0))
                      (loop (fx- i 1)))
                     (else diff)))))
          (else
           (fx- u-size v-size)))))

(define-integrable (bignum-magnitude-less? u v)
  (fx< (bignum-compare-magnitudes u v) 0))

;;; Returns a fixnum whose sign is the same as (- u v).

(define (bignum-compare u v)
  (let ((u-sign (bignum-sign u)))
    (cond ((fxn= u-sign (bignum-sign v)) u-sign)
          ((fx> u-sign 0) (bignum-compare-magnitudes u v))
          (else           (bignum-compare-magnitudes v u)))))

(define-integrable (bignum-less? u v)
  (fx< (bignum-compare u v) 0))

(define-integrable (bignum-equal? u v)
  (fx= (bignum-compare u v) 0))


;;; Sign negotiation and normalization:

;;; The BIGNUM-FOO routines negotiate a sign for the result, then
;;; dispatch to the appropriate FOO-MAGNITUDES routine.  The result
;;; is then normalized.

(define (bignum-add u v)
  (let ((u-sign (bignum-sign u))
        (v-sign (bignum-sign v)))
    (normalize-integer
     (cond ((fx= u-sign v-sign)
            (set-bignum-sign (add-magnitudes u v) u-sign))
           ((bignum-magnitude-less? u v)
            (set-bignum-sign (subtract-magnitudes v u) v-sign))
           (else
            (set-bignum-sign (subtract-magnitudes u v) u-sign))))))

(define (bignum-subtract u v)
  (let ((u-sign (bignum-sign u))
        (v-sign (bignum-sign v)))
    (normalize-integer
     (cond ((fxn= u-sign v-sign)
            (set-bignum-sign (add-magnitudes u v) u-sign))
           ((bignum-magnitude-less? v u)
            (set-bignum-sign (subtract-magnitudes u v) u-sign))
           (else
            (set-bignum-sign (subtract-magnitudes v u)
                             (fixnum-negate u-sign)))))))

(define-integrable (bignum-multiply-sign u v)
  (if (fx= (bignum-sign u) (bignum-sign v)) 1 -1))

(define (bignum-multiply u v)
  (normalize-integer
   (set-bignum-sign (multiply-magnitudes u v) (bignum-multiply-sign u v))))

(define (b-f-multiply u v)
  (cond ((or (fx> v        *max-hyperdigit*)
             (fx< v (fx- 0 *max-hyperdigit*)))
         (bignum-multiply u (fixnum->bignum v)))
        (else
         (let* ((u-size (bignum-size u))
                (product (create-bignum (fx+ 1 u-size))))
           (multiply-step (fixnum-abs v) u u-size product 0)
           (set (bignum-sign product)
                (if (fx= (bignum-sign u)
                         (if (fx< v 0) -1 1))
                    1
                    -1))
           (normalize-integer (bignum-trim! product))))))

;;; Used only by BIGNUM-DIVIDE, BIGNUM-REMAINDER, and B-F-DIV2

(define (bignum-div2 u v values)
  (let ((m (bignum-compare-magnitudes u v)))
    (cond ((fx= m 0)
           (values (bignum-multiply-sign u v) 0))
          ((fx< m 0)
           (values 0 u))
          (else
           (div2-magnitudes u v
             (lambda (q r)
               (values (normalize-integer
                        (set-bignum-sign q (bignum-multiply-sign u v)))
                       (normalize-integer
                        (set-bignum-sign r (bignum-sign u))))))))))

(define (bignum-divide u v)    (bignum-div2 u v proj0))
(define (bignum-remainder u v) (bignum-div2 u v proj1))

(define (b-f-add u v)      (bignum-add u (fixnum->bignum v)))
(define (b-f-subtract u v) (bignum-subtract u (fixnum->bignum v)))

(define (b-f-divide u v)    (b-f-div2 u v proj0))
(define (b-f-remainder u v) (b-f-div2 u v proj1))   ;|remainder| < |divisor|


;;; Total randomness: negate, odd?, howlong.

(define (bignum-negate num)
  (normalize-integer (bignum-negate! (copy-bignum num))))

(define (bignum-odd? num)
  (fixnum-odd? (bigref num 0)))

(define (bignum-howlong num)
  (let ((last (fx- (bignum-size num) 1)))
    (fx+ (fixnum-howlong (bigref num last))
         (fx* last *bits-per-hyperdigit*))))


;;; Arithmetic shift entry points:

;;; Left shift bignum.

(define (bignum-ashl num amount)
  (%digit-divide 0 amount *bits-per-hyperdigit*
    (lambda (q r)
      (let* ((old-size (bignum-size num))
             (new-size (fx+ (fx+ old-size q) 1)))
        (bignum-trim!
         (bignum-ashl! (make-and-replace-bignum new-size num q 0 old-size)
                       r))))))

;;; Destructive shift.  Size of dst is ignored.  src and dst may be the
;;; same bignum.  0 <= amount < *bits-per-hyperdigit*

(define (bignum-ashl! num amount)
  (let* ((j (fx- (bignum-size num) 1))
         (z (fx- *bits-per-hyperdigit* amount)))
    (iterate loop ((prev (bigref num j))
                   (j j))
      (let ((high (fixnum-logand (fixnum-ashl prev amount)
                                 *max-hyperdigit*)))
        (cond ((fx<= j 0)
               (bigset num j high)
               num)
              (else
               (let ((foo (bigref num (fx- j 1))))
                 (bigset num j (fx+ high (fixnum-ashr foo z)))
                 (loop foo (fx- j 1)))))))))

;;; Left shift fixnum yielding bignum.

(define (fixnum-ashl-bignum num amount)
  (bignum-ashl (fixnum->bignum num) amount))   ; Fix later

;;; Right shift bignum yielding bignum.

(define (bignum-ashr src amount)
  (%digit-divide 0 amount *bits-per-hyperdigit*
    (lambda (q r)
      (let* ((old-size (bignum-size src))
             (new-size (fx- old-size q)))
        (bignum-trim!
         (bignum-ashr! (make-and-replace-bignum new-size src 0 q new-size)
                       r))))))

;;; Right shift bignum yielding fixnum.  This could be done better.

(define (bignum-ashr-fixnum src amount)
  (normalize-integer (bignum-ashr src amount)))

;;; Destructive right shift.

(define (bignum-ashr! num amount)
  (let* ((end (fx- (bignum-size num) 1))
         (z (fx- *bits-per-hyperdigit* amount)))
    (iterate loop ((prev (bigref num 0))
                   (j 0))
      (let ((high (fixnum-ashr prev amount)))
        (cond ((fx>= j end)
               (bigset num j high)
               num)
              (else
               (let ((foo (bigref num (fx+ j 1))))
                 (bigset num j
                         (fx+ high (fixnum-logand (fixnum-ashl foo z)
                                                  *max-hyperdigit*)))
                 (loop foo (fx+ j 1)))))))))

;;; Careful fixnum routines:

;;; Assume N bit arithmetic, machine result is N bits (what the hardware
;;; gives).  The "true result" may be up to N+1 bits sign of result on
;;; overflow.
;;;
;;;   if (A-B) overflows, A and B have different signs,
;;;            true result is sign of A 
;;;   if (A+B) overflows, A and B have the same sign,
;;;            true result is the same sign
;;;
;;;   When the sign of the true result is positive, the magnitude of the
;;;     true result is the machine result.
;;;
;;;   When the sign of the true result is negative, the magnitude of the
;;;     true result is the magnitude of the N+1 bit number
;;;     formed by the carry bit and the machine result.
;;;
;;; Subtle issue:                                                            
;;; Taking the arithmetic complement of an N+1 bit number requires N+1 bits
;;; in all cases except for the most negative number which requires N+2 bits
;;; to complement.  Fortunately we only want the magnitude, which is
;;; the arithmetic complement of the hardware result, except when the
;;; N+1 bit number is the most negative N+1 bit number (carry bit = 1,
;;; fixnum = 0), in which case the magnitude is that same N+1 bit number.
;;;
;;; Aren't you glad I figured this out?

;;; This routine takes wreckage from an overflowed ADD or SUB and
;;; makes a bignum out of it.  SIGN is computed as described above.

(define (fixnum-aftermath->bignum sign carry? fixnum)
  (cond ((fx= sign -1)
         (cond ((and carry? (fx= fixnum 0))     ; min N+1 bit fixnum
                (sign&magnitude->bignum -1 t 0))
               (else 
                (sign&magnitude->bignum -1 nil (fixnum-negate fixnum)))))
        (else
         (sign&magnitude->bignum 1 nil fixnum))))

(define (fixnum-add-carefully x y)
  (cond ((fixnum-add-overflow? x y)
         (fixnum-aftermath->bignum (if (fx< x 0) -1 1)  ; sign of either
                                   (fixnum-add-carry? x y)
                                   (fx+ x y)))
        (else (fx+ x y))))

(define (fixnum-subtract-carefully x y)
  (cond ((fixnum-subtract-overflow? x y)
         (fixnum-aftermath->bignum (if (fx< x 0) -1 1)  ; sign of first
                                   (fixnum-add-carry? x y)
                                   (fx+ x y)))
        (else (fx- x y))))

(define (fixnum-multiply-carefully x y)
  (let ((res (fx+ (fixnum-howlong (fixnum-abs x))
                  (fixnum-howlong (fixnum-abs y)))))
    (cond ((fx>= res *bits-per-fixnum*)
           (bignum-multiply (fixnum->bignum x) (fixnum->bignum y)))
          (else (fx* x y)))))

(define fixnum-divide-carefully ratio)

(define (fixnum-negate-carefully fixnum)
  (cond ((fx= fixnum *max-fixnum*)
         (fixnum->bignum fixnum))
        (else (fixnum-negate fixnum))))

;;; MAGN is a fixnum interpreted as an unsigned  integer that is 
;;; *bits-per-fixnum* long.  EXTRA-BIT? is a handy N+1st bit for those
;;; times that you have (+ *bits-per-fixnum* 1) bits of magnitude.

(define (sign&magnitude->bignum sign extra-bit? magn)
  (let* ((\#bits (cond (extra-bit? (fx+ *bits-per-fixnum* 1))
                       ((fx< magn 0) *bits-per-fixnum*)
                       (else (fixnum-howlong magn))))
         (\#bits-to-loop-over (if extra-bit? *bits-per-fixnum* \#bits))
         (\#digits (fx/ (fx+ \#bits (fx- *bits-per-hyperdigit* 1)) 
                        *bits-per-hyperdigit*))
         (most-significant-hyperdigit (fx- \#digits 1))
         (num (create-bignum \#digits)))
    (do ((fxpos 0 (fx+ fxpos *bits-per-hyperdigit*))
         (bnpos 0 (fx+ bnpos 1)))
        ((fx>= fxpos \#bits-to-loop-over)
         ;; if the extra bit was set, then set it in the bignum
         (cond (extra-bit?
                (bigset num
                        most-significant-hyperdigit
                        (set-fixnum-bit-field
                         (bigref num most-significant-hyperdigit)
                         (fixnum-remainder *bits-per-fixnum* 
                                           *bits-per-hyperdigit*)
                         1              ; just 1 bit field
                         1              ; turn the bit on
                         ))))
         (set-bignum-sign num sign))
      (bigset num
              bnpos
              (fixnum-bit-field magn
                                fxpos
                                (fixnum-min *bits-per-hyperdigit* 
                                            (fx- *bits-per-fixnum* fxpos)))))))


;;; Normalization:

;;; Convert an integer to normal form.  That is, if it is a bignum within
;;; the fixnum range, convert it to a fixnum.

(define (normalize-integer n)
  (cond ((fixnum? n) n)
        ((if (bignum-positive? n)
             (bignum-less? *max-fixnum-as-bignum* n)
             (bignum-less? n *min-fixnum-as-bignum*))
         n)
        (else (bignum->fixnum n))))

(define-constant *max-fixnum-as-bignum*
  (sign&magnitude->bignum  1 nil *max-fixnum*))

(define-constant *min-fixnum-as-bignum*
  (sign&magnitude->bignum -1 nil *min-fixnum*))


;;; Coercion routines:

(define (fixnum->bignum fx)
  (cond ((fx= fx *min-fixnum*) *min-fixnum-as-bignum*)
        (else (sign&magnitude->bignum (if (fx< fx 0) -1 1) 
				      nil
				      (fixnum-abs fx)))))

(define (bignum->fixnum bn)
  (cond ((bignum-equal? *min-fixnum-as-bignum* bn) *min-fixnum*)
        (else
	 (let ((len (bignum-size bn)))
             (do ((i 0 (fx+ 1 i))
                  (skip 0 (fx+ skip *bits-per-hyperdigit*))
                  (num 0 (set-fixnum-bit-field 
                          num
                          skip
                          (fixnum-min *bits-per-hyperdigit*
                                      (fx- *bits-per-fixnum* skip))
                          (bigref bn i))))
                 ((fx>= i len) (if (bignum-positive? bn) num (fx- 0 num)))
               )))))

(define (pointer->integer p)
  (let ((addr (pointer-address p)))
    (cond ((and (fx>= addr 0)
                (fx<= addr (fixnum-ashr *max-fixnum* 3)))
           (pointer->fixnum p))
          (else
           (b-f-add (bignum-ashl (sign&magnitude->bignum 1 nil addr) 3)
                    (pointer-tag p))))))

(define (integer->pointer n)                ; Fix later.
  (let ((n (check-arg nonnegative-integer? n integer->pointer)))
    (cond ((fixnum? n) (fixnum->pointer n))
          (else
           (b-f-div2-unnormalized n 8
             (lambda (q r)
               (make-pointer (bignum->fixnum q) r)))))))

(define (bignum->flonum b)
  (error "integer to float conversion not yet implemented~%  (~S ~S)"
         '->float b))


;;; Input and output:

(define (print-bignum num stream)
  (let ((new-num (normalize-integer num)))
    (cond ((neq? num new-num)
           (format stream "#{Unnormalized-bignum~_~S}"
                   new-num))
          (else
           (let ((buffer (bignum->buffer num)))
             (cond ((not (bignum-positive? num)) (writec stream #\-)))
             (do ((i (fx- (string-length buffer) 1) (fx- i 1)))
                 ((not (char= (nthchar buffer i) #\0))
                  (do ((i i (fx- i 1)))
                      ((fx< i 0) (release-buffer buffer))
                    (writec stream (nthchar buffer i))))))))))

;;; Convert a bignum to a sequence of characters.
;;; Characters are generated in reverse order by successive divisions.

(define (bignum->buffer num)
  (let* ((radix (rt-radix *print-table*))
         (k (\#chars-in-bit-field radix *bits-per-hyperdigit*))
         (radix^k (fixnum-expt radix k))
         (buffer (get-buffer)))
    (iterate loop ((num num))
      (b-f-div2-unnormalized num radix^k
        (lambda (q r)
          (output-bignum-digit r k buffer radix)
          (cond ((fx> (bignum-size q) 1)
                 (loop q))
                (else
                 (output-bignum-digit
                    (output-bignum-digit (bigref q 0) k buffer radix)
                    k buffer radix)
                 buffer)))))))

;;; Generate k digits of output.  Returns the k+1'th digit.

(define (output-bignum-digit digit k buffer radix)
  (iterate loop ((n digit)
                 (i k))
    (cond ((fx> i 0)
           (%digit-divide 0 n radix
             (lambda (q r)
               (buffer-writec buffer (digit->char r radix))
               (loop q (fx- i 1)))))
          (else n))))

;;; Number of characters in RADIX that can surely fit in FIELD-SIZE bits.
;;; Make this more accurate.  How does BIGNUM-STRINGIFY work and did
;;; I screw it up by changing this routine?

(define (\#chars-in-bit-field radix field-size)
  (fx/ field-size (fixnum-howlong radix)))

;;; Convert string to fixnum or bignum, as appropriate.

(define (string->integer string radix)
  (cond ((char= (char string) *negative-sign-char*)
         (string->integer-aux string 1 (fx- (string-length string) 1) t radix))
        ((char= (char string) *positive-sign-char*)
         (string->integer-aux string 1 (fx- (string-length string) 1) nil radix))
        (else
         (string->integer-aux string 0 (string-length string) nil radix))))

;;;  We grab a bunch of digits at a whack, convert them to fixnum, and
;;;  do multiplications just with them.  
;;; grabsize:  number of digits we can grab (whack size)
;;; shift:     radix of grabsize considered as a hyperdigit
;;; leftovers: number of digits that don't fit into an even number of grabs -
;;;            convert these first

;;; Fast enough?  Clean enough?
;;; Hack sign inside loop rather than after so that we read *min-fixnum*
;;; as a fixnum and not a bignum.
;;; Note that any compiler worth its salt will integrate the definitions
;;; of my+ and my*.

(define (string->integer-aux string start length neg? radix)
  (let ((length (string-length string))
        (grabsize (\#chars-in-bit-field radix *u-bits-per-fixnum*))
        (my* (lambda (x y) (cond ((fixnum? x) (fixnum-multiply-carefully x y))
                                 (else (b-f-multiply x y)))))
        (my+ (lambda (x y) (cond ((fixnum? x) (fixnum-add-carefully x y))
                                 (else (b-f-add x y))))))
    (let ((shift (fixnum-expt radix grabsize))
          (leftovers (fixnum-remainder (fx- length start) grabsize)))
      (let ((sum (string->fixnum string start leftovers radix)))
        (do ((sum (if neg? (fixnum-negate sum) sum)
                  (my+ (my* sum shift) 
                       (let ((x (string->fixnum string strpos grabsize radix)))
                         (if neg? (fixnum-negate x) x))))
             (strpos (fx+ start leftovers)
                     (fx+ strpos grabsize)))
            ((fx>= strpos length) sum))))))

;;; This belongs elsewhere

(define (string->fixnum string start count radix)
  (let ((limit (fx+ start count)))
    (do ((i start (fx+ i 1))
         (sum 0 (fx+ (fx* sum radix) (%char->digit (nthchar string i) radix))))
        ((fx>= i limit) sum))))


;;; Debugging utility:

;(define-syntax (bignum-pig x)
;  `(',*bignum-pig (lambda () ,x)))

(define (*bignum-pig x)
  (let ((b1 *bignum-cons-counter*)
        (b2 *bignum-cons-size-counter*))
    (let ((val (x))
          (a1 *bignum-cons-counter*)
          (a2 *bignum-cons-size-counter*))
      `(count = ,(fx- a1 b1) total = ,(fx- a2 b2) value = ,val))))
