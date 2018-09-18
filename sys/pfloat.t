(herald (tsys pfloat t 16)
        (env tsys))

;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; Spice Lisp is currently incomplete and under active development.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************


;;;
;;; Spice Lisp printer.
;;; Written by Neal Feinberg, Spice Lisp Group.
;;; Currently maintained by Skef Wholey.
;;; 
;;; *******************************************************************

;;; *** Adapted for use in T by J. Rees, Nov 1983 ***

;;;; Floating Point printing
;;;
;;;  Written by Bill Maddox
;;;
;;;
;;;
;;; FLONUM-TO-STRING (and its subsidiary function FLOAT-STRING) does most of 
;;; the work for all printing of floating point numbers in the printer and in
;;; FORMAT.  It converts a floating point number to a string in a free or 
;;; fixed format with no exponent.  The interpretation of the arguments is as 
;;; follows:
;;;
;;;     X        - The floating point number to convert, which must not be
;;;                negative.
;;;     WIDTH    - The preferred field width, used to determine the number
;;;                of fraction digits to produce if the FDIGITS parameter
;;;                is unspecified or NIL.  If the non-fraction digits and the
;;;                decimal point alone exceed this width, no fraction digits
;;;                will be produced unless a non-NIL value of FDIGITS has been
;;;                specified.  Field overflow is not considerd an error at this
;;;                level.
;;;     FDIGITS  - The number of fractional digits to produce. Insignificant
;;;                trailing zeroes may be introduced as needed.  May be
;;;                unspecified or NIL, in which case as many digits as possible
;;;                are generated, subject to the constraint that there are no
;;;                trailing zeroes.
;;;     SCALE    - If this parameter is specified or non-NIL, then the number
;;;                printed is (* x (expt 10 scale)).  This scaling is exact,
;;;                and cannot lose precision.
;;;     FMIN     - This parameter, if specified or non-NIL, is the minimum
;;;                number of fraction digits which will be produced, regardless
;;;                of the value of WIDTH or FDIGITS.  This feature is used by
;;;                the ~E format directive to prevent complete loss of
;;;                significance in the printed value due to a bogus choice of
;;;                scale factor.
;;;
;;; Most of the optional arguments are for the benefit of FORMAT and are not
;;; used by the printer.
;;;
;;; Returns:
;;; (VALUES DIGIT-STRING DIGIT-LENGTH LEADING-POINT TRAILING-POINT DECPNT)
;;; where the results have the following interpretation:
;;;
;;;     DIGIT-STRING    - The decimal representation of X, with decimal point.
;;;     DIGIT-LENGTH    - The length of the string DIGIT-STRING.
;;;     LEADING-POINT   - True if the first character of DIGIT-STRING is the
;;;                       decimal point.
;;;     TRAILING-POINT  - True if the last character of DIGIT-STRING is the
;;;                       decimal point.
;;;     POINT-POS       - The position of the digit preceding the decimal
;;;                       point.  Zero indicates point before first digit.
;;;
;;; NOTE:  FLONUM-TO-STRING goes to a lot of trouble to guarantee accuracy.
;;; Specifically, the decimal number printed is the closest possible 
;;; approximation to the true value of the binary number to be printed from 
;;; among all decimal representations  with the same number of digits.  In
;;; free-format output, i.e. with the number of digits unconstrained, it is 
;;; guaranteed that all the information is preserved, so that a properly-
;;; rounding reader can reconstruct the original binary number, bit-for-bit, 
;;; from its printed decimal representation. Furthermore, only as many digits
;;; as necessary to satisfy this condition will be printed.
;;;
;;;
;;; FLOAT-STRING actually generates the digits for positive numbers.  The
;;; algorithm is essentially that of algorithm Dragon4 in "How to Print 
;;; Floating-Point Numbers Accurately" by Steele and White.  The current 
;;; (draft) version of this paper may be found in [CMUC]<steele>tradix.press.
;;; DO NOT EVEN THINK OF ATTEMPTING TO UNDERSTAND THIS CODE WITHOUT READING 
;;; THE PAPER!


;;; Temporary compatibility support hacks.

(define-local-syntax (multiple-value-bind vars form . body)
  `(,@form (lambda ,vars . ,body)))

(define-local-syntax (dotimes pat . body)
  (destructure (((var limit) pat))
    `(let ((%limit% ,limit))
       (do ((,var 0 (fx+ ,var 1)))
           ((fx>= ,var %limit%) 0)
         . ,body))))

(comment        ; Uncomment if you want to run this outside of implem. env
(import *t-implementation-env*
        get-buffer
        buffer-writec
        flonum-guts
        hacked-copy-flonum)
)

;;; E.g. (%ceiling 27 10) => 3

(define (%ceiling x y)          ; YUCKO !!
  (div2 x y (lambda (q r) (if (= r 0) q (+ q 1)))))

;;; Returns a BUFFER, which must be freed at some point.

(define (flonum-to-string x width fdigits scale fmin values)
  (cond ((zero? x)
         ;; zero is a special case which float-string cannot handle
         (let ((buffer (get-buffer)))
           (buffer-writec buffer #\.)
           (values buffer 1 t t 0)))
        (else
         (multiple-value-bind (sig exp)
                              (integer-decode-float x)
           (float-string sig exp (integer-length sig) width fdigits scale fmin
                         values)))))

(define (float-string fraction exponent precision width fdigits scale fmin
                      values)
  (let ((r fraction) (s 1) (m- 1) (m+ 1) (k 0)
        (digits 0) (decpnt 0) (cutoff nil) (roundup nil) (u) (low) (high)
        (ash-r-1) (ash-s-1)
        (buffer (get-buffer)))
    ;; Represent fraction as r/s, error bounds as m+/s and m-/s.
    ;; Rational arithmetic avoids loss of precision in subsequent calculations.
    (cond ((fx> exponent 0)
           (set r (%ash fraction exponent))      ; Bignum
           (set m- (%ash 1 exponent))            ; Bignum
           (set m+ m-))
          ((fx< exponent 0)
           (set s (%ash 1 (fx- 0 exponent)))))
    ;; adjust the error bounds m+ and m- for unequal gaps
    (cond ((= fraction (%ash 1 precision))
           (set m+ (%ash m+ 1))
           (set r (%ash r 1))
           (set s (%ash s 1))))
    ;; scale value by requested amount, and update error bounds
    (cond (scale
           (if (fx< scale 0)
               (let ((scale-factor (expt 10 (fx- 0 scale))))
                 (set s (* s scale-factor)))
               (let ((scale-factor (expt 10 scale)))
                 (set r (* r scale-factor))
                 (set m+ (* m+ scale-factor))
                 (set m- (* m- scale-factor))))))
    ;; scale r and s and compute initial k, the base 10 logarithm of r
    (let ((c (%ceiling s 10)))
      (do ()
          ((>= r c))
        (set k (fx- k 1))
        (set r (* r 10))
        (set m- (* m- 10))
        (set m+ (* m+ 10))))
    (let ((z (+ (%ash r 1) m+)))
      (iterate loop ()
        (set ash-s-1 (%ash s 1))
        (cond ((>= z ash-s-1)
               (set s (* s 10))
               (set k (fx+ k 1))
               (loop)))))
    ;;determine number of fraction digits to generate
    (cond (fdigits
           ;;use specified number of fraction digits
           (set cutoff (fx- 0 fdigits))
           ;;don't allow less than fmin fraction digits
           (if (and fmin (fx> cutoff (fx- 0 fmin)))
               (set cutoff (fx- 0 fmin))))
          (width
           ;;use as many fraction digits as width will permit
           ;;but force at least fmin digits even if width will be exceeded
           (if (fx< k 0)
               (set cutoff (fx- 1 width))
               (set cutoff (fx+ 1 (fx- k width))))
           (if (and fmin (fx> cutoff (fx- 0 fmin)))
               (set cutoff (fx- 0 fmin)))))
    ;;If we decided to cut off digit generation before precision has
    ;;been exhausted, rounding the last digit may cause a carry propagation.
    ;;We can prevent this, preserving left-to-right digit generation, with
    ;;a few magical adjustments to m- and m+.  Of course, correct rounding
    ;;is also preserved.
    (cond ((or fdigits width)
           (let ((a (fx- cutoff k))
                 (y s))
             (if (fx>= a 0)
                 (dotimes (i a) (set y (* y 10)))
                 (dotimes (i (fx- 0 a)) (set y (%ceiling y 10))))
             (set m- (max y m-))
             (set m+ (max y m+))
             (cond ((= m+ y) (set roundup t))))))


    ;;zero-fill before fraction if no integer part
    (cond ((fx< k 0)
           (set decpnt digits)
           (buffer-writec buffer #\.)
           (dotimes (i (fx- 0 k))
             (set digits (fx+ digits 1))
             (buffer-writec buffer #\0))))
    ;;generate the significant digits
    (iterate loop ()
      (set k (fx- k 1))
      (cond ((fx= k -1)
             (buffer-writec buffer #\.)
             (set decpnt digits)))
      ;;(multiple-value-set (u r) (truncate (* r 10) s))
      (let ((z (div2 (* r 10) s cons)))
        (set u (car z))
        (set r (cdr z)))
      (set m- (* m- 10))
      (set m+ (* m+ 10))
      (set ash-r-1 (%ash r 1))
      (set low (< ash-r-1 m-))
      (if roundup
          (set high (>= ash-r-1 (- ash-s-1 m+)))
          (set high (>  ash-r-1 (- ash-s-1 m+))))
      ;;stop when either precision is exhausted or we have printed as many
      ;;fraction digits as permitted
      (cond ((not (or low high (and cutoff (fx<= k cutoff))))
             (buffer-writec buffer (digit->char u 10))
             (set digits (fx+ digits 1))
             (loop))))
    ;;if cutoff occured before first digit, then no digits generated at all
    (cond ((or (not cutoff) (fx>= k cutoff))
           ;;last digit may need rounding
           (buffer-writec buffer
                          (digit->char
                                (cond ((and low (not high)) u)
                                      ((and high (not low)) (fx+ 1 u))
                                      (else
                                       (if (<= ash-r-1 s) u (fx+ 1 u))))
                                10))
           (set digits (fx+ digits 1))))
    ;;zero-fill after integer part if no fraction
    (cond ((fx>= k 0)
           (dotimes (i k)
             (set digits (fx+ digits 1))
             (buffer-writec buffer #\0))
           (buffer-writec buffer #\.)
           (set decpnt digits)))
    ;;add trailing zeroes to pad fraction if fdigits specified
    (cond (fdigits
           (dotimes (i (fx- fdigits (fx- digits decpnt)))
             (set digits (fx+ digits 1))
             (buffer-writec buffer #\0))))
    ;;all done
    (values buffer (fx+ 1 digits) (fx= decpnt 0) (fx= decpnt digits) decpnt)))


;;; Given a non-negative floating point number, SCALE-EXPONENT returns a
;;; new floating point number Z in the range (0.1, 1.0] and an exponent
;;; E such that Z * 10^E is (approximately) equal to the original number.
;;; There may be some loss of precision due the floating point representation.


;(defconstant short-log10-of-2 #~F0.30103s0)
(define-constant %sp-l-float fixnum->flonum)
(define-constant %long-float-ten (fixnum->flonum 10))
(define-constant %long-float-one-tenth (/ 1 %long-float-ten))
(define (log10 x) (fl/ (log x) (log %long-float-ten)))
(define-constant long-log10-of-2 (log10 (fixnum->flonum 2)))
(define-constant zero (fixnum->flonum 0))

(define (scale-exponent x values)
      (scale-expt-aux x (%sp-l-float 0) (%sp-l-float 1) %long-float-ten
                      %long-float-one-tenth long-log10-of-2 values))


(define (scale-expt-aux x zero one ten one-tenth log10-of-2 values)
  (multiple-value-bind (sig exponent)
                       (decode-float x)
    (if (fl= x zero)
        (values zero 1)
        (let* ((e (flonum->fixnum (fl* (fixnum->flonum exponent) log10-of-2)))
               (x (if (fx< e 0)                ;For the end ranges.
                      (* (* x ten) (expt ten (fx- -1 e)))
                      (/ (/ x ten) (expt ten (fx-  e 1))))))
          (do ((d ten (* d ten))
               (y x (/ x d))
               (e e (fx+ 1 e)))
              ((< y one)
               (do ((m ten (* m ten))
                    (z y (* z m))
                    (e e (fx- e 1)))
                   ((>= z one-tenth) (values z e)))))))))


;;; Entry point for the float printer as called by PRINT, PRIN1, PRINC,
;;; etc.  The argument is printed free-format, in either exponential or 
;;; non-exponential notation, depending on its magnitude.
;;;
;;; NOTE:  When a number is to be printed in exponential format, it is scaled
;;; in floating point.  Since precision may be lost in this process, the
;;; guaranteed accuracy properties of FLONUM-TO-STRING are lost.  The
;;; difficulty is that FLONUM-TO-STRING performs extensive computations with
;;; integers of similar magnitude to that of the number being printed.  For
;;; large exponents, the bignums really get out of hand.  When we switch to
;;; IEEE format for long floats, this will significantly restrict the magnitude
;;; of the largest allowable float.  This combined with microcoded bignum
;;; arithmetic might make it attractive to handle exponential notation with
;;; the same accuracy as non-exponential notation, using the method described
;;; in the Steele and White paper.


(define %long-float1l-3 (/ (fixnum->flonum 1) 1000)) ;1.0e-3
(define %long-float1l7  (fixnum->flonum 10000000))   ;1.0e7

(define (print-flonum x stream)         ;Entry point from PRINT
  (output-float-aux x %long-float1l-3 %long-float1l7 stream))

;;; There is (was?) a TC bug lurking in here.  Beware.

(define (output-float-aux x e-min e-max stream)
  (cond ((fl= x 0.0) (write-string stream "0.0")
                   ;(if (not (typep x *read-default-float-format*))
                   ;    (write-string (if (typep x 'short-float) "s0" "L0")))
                   )
        (else
         (let ((x (cond ((fl< x 0.0)
                         (write-char stream #\-)
                         (fl- 0.0 x))
                        (else x))))
           (if (and (fl>= x e-min) (fl< x e-max))
               ;; free format
               (multiple-value-bind (str len lpoint tpoint ())
                   (flonum-to-string x nil nil nil nil)
		 (ignore len)
                 (if lpoint (write-char stream #\0))
                 (write-string stream str)
                 (release-buffer str)
                 (if tpoint (write-char stream #\0))
                 ;(if (not (typep x *read-default-float-format*))
                 ;    (write-string (if (typep x 'short-float) "s0" "L0")))
                 )
               ;; exponential format
               (multiple-value-bind (f e)
                                    (scale-exponent x)
                 (multiple-value-bind (str len lpoint tpoint ())
                                      (flonum-to-string f nil nil 1 nil)
		   (ignore len)
                   (if lpoint (write-char stream #\0))
                   (write-string stream str)
                   (release-buffer str)
                   (if tpoint (write-char stream #\0))
                   (write-char stream
                               ;(if (typep x *read-default-float-format*)
                                   #\E
                                   ;(if (typep x 'short-float) #\S #\L))
                   )
                   ;; must subtract 1 from exponent here, due to
                   ;; the scale factor of 1 in call to FLONUM-TO-STRING
                   (if (not (fx< (fx- e 1) 0)) (write-char stream #\+))
                   (print (fx- e 1) stream))))))))
