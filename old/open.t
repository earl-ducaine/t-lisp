(herald open
        (support (make-empty-support-env 'nil) vaxbase vaxprimops))

;;; Copyright (c) 1983, 1984 Yale University

;;; The following variables are free in this file: (FIXNUM-ODD? FIXNUM-NEGATE
;;; STRING-NTHTAIL! CHOPY STRING-TAIL! STRING-EMPTY? MEM *EOF*
;;; **NO-MORE-COND-CLAUSES** NTH)

;;;; Standard integrable procedures

;;; Open-coded routines, defined as integrable procedures.

;;; System constants

(define-constant t    '#t)
(define-constant else '#t)
(define-constant nil  '#f)

(define-constant *null-char* #\null)
(define-constant *number-of-char-codes* 256.)
(define-constant *string-length-limit* 32767)   ;exclusive limit.

;;; Boolean stuff

(define-integrable (not x) (if x nil t))
(define-integrable (false? x) (if x nil t))

(define-integrable (boolean? x) (or (eq? x t) (eq? x nil)))

;;; Combinator stuff

(define-integrable (always k) (lambda x (ignore x) k))

(define-integrable (proj0 x . rest) (ignore rest) x)
(define-integrable (proj1 x y . rest) (ignore rest x) y)
(define-integrable (proj2 x y z . rest) (ignore rest x y) z)
(define-integrable (proj3 x y z w . rest) (ignore rest x y z) w)

(define-integrable (projn n) (lambda arglist (nth arglist n)))

(define-integrable (identity x) x)

;;; Macro support for compiler

(define-integrable (cond-=>-aux p f a) (if p ((f) p) (a)))

(define-integrable (or-aux p r) (if p p (r)))

;;; Assertion crud

(define-integrable (proclaim type obj)
  (if (type obj) obj (undefined-effect)))

(define-integrable (check-arg type obj fn)
  (ignore fn)
  (if (type obj) obj (undefined-effect)))

(define-integrable enforce proclaim)

;;; Really basic type predicates, not primitive to the compiler.

(define-integrable (null? x) (eq? x '()))

(define-integrable (atom? x) (not (pair? x)))

(define-integrable (pair? x) (and (list? x) (not (null? x))))

(define-integrable (null-list? x)
  (cond ((null? x) t)
        ((atom? x) (undefined-effect))
        (else nil)))

(define-integrable (nonnegative-fixnum? x)
  (and (fixnum? x) (fixnum-not-negative? x)))

(define-integrable (eof? x)
  (eq? x *eof*))

(define-integrable (newline? c) (eq? c #\newline))

(define-integrable (mem? pred obj list)
  (if (mem pred obj list) t nil))

(define-integrable (memq  obj list) (mem  eq? obj list))
(define-integrable (memq? obj list) (mem? eq? obj list))

(define-integrable (neq? x y) (not (eq? x y)))

;;; Character and string stuff.

(define-integrable (non-empty-string? x)
  (and (string? x) (not (string-empty? x))))

;;; Here we depend on characters being implemented as simply their ASCII code
;;; as fixnum with character type code adjoined.
(define-integrable (string-tail s)
  (string-tail! (chopy s)))

(define-integrable (string-nthtail s n)
  (string-nthtail! (chopy s) n))

;;; Arithmetic stuff

(define-integrable (fixnum-greater? x y)
  (fixnum-less? y x))

(define-integrable (fixnum-not-equal? x y)
  (not (fixnum-equal? x y)))

(define-integrable (fixnum-not-less? x y)
  (not (fixnum-less? x y)))

(define-integrable (fixnum-not-greater? x y)
  (not (fixnum-less? y x)))

(define-integrable (fixnum-abs n)
  (if (fixnum-less? n 0) (fixnum-negate n) n))

(define-integrable (fixnum-min x y)
  (if (fixnum-less? x y) x y))

(define-integrable (fixnum-max x y)
  (if (fixnum-greater? x y) x y))

;Unix assembler can't handle this.
;(define-integrable (flonum-abs n)
;  (if (flonum-less? n 0.0) (flonum-negate n) n))

(define-integrable (fixnum-positive? x)  (fixnum-greater?  x 0))
(define-integrable (fixnum-negative? x)  (fixnum-less?     x 0))
(define-integrable (fixnum-zero? x)      (fixnum-equal?    x 0))
(define-integrable (fixnum-not-positive? x) (fixnum-not-greater? x 0))
(define-integrable (fixnum-not-negative? x) (fixnum-not-less?    x 0))
(define-integrable (fixnum-not-zero? x)     (fixnum-not-equal?   x 0))

(define-integrable (fixnum-even? x) (not (fixnum-odd? x)))

;;; Character stuff

(define-integrable (char= x y)
  (fixnum-equal? (char->ascii x) (char->ascii y)))

(define-integrable (char< x y)
  (fixnum-less? (char->ascii x) (char->ascii y)))

(define-integrable (char> x y)
  (char< y x))

(define-integrable (charn= x y)
  (not (char= x y)))

(define-integrable (char>= x y)
  (not (char< x y)))

(define-integrable (char<= x y)
  (not (char< y x)))
