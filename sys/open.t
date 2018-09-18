(herald (tsys open t 108)
        (env () (tsys aliases))
        (syntax-table
         (block (require (tsys guard))
                &&syntax-table&&)))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Standard integrable procedures

;;; Open-coded routines, defined as integrable procedures.

;;; System constants

(define-constant nil '())
(define-constant t 't)
(define-constant else 'else)

(define-constant *null-char* #\null)
(define-constant *number-of-char-codes* 256.)
(define-constant *string-length-limit* 32767)   ;Exclusive limit.

;;; Boolean stuff

(define-integrable (not x) (if x nil t))
(define-integrable (false? x) (if x nil t))
(define-integrable (true? x) (if x t nil))

(define-integrable (boolean? x) (or (eq? x t) (eq? x nil)))

;;; Combinator stuff

(define-integrable (always k) (lambda x (ignore x) k))

(define-integrable (proj0 x . rest) (ignore rest) x)
(define-integrable (proj1 x y . rest) (ignore rest x) y)
(define-integrable (proj2 x y z . rest) (ignore rest x y) z)
(define-integrable (proj3 x y z w . rest) (ignore rest x y z) w)

(define-integrable (projn n) (lambda arglist (nth arglist n)))

(define-integrable (identity x) x)              ; who needs this?

;;; Macro support for compiler

(define-integrable (cond-=>-aux p f a) (if p ((f) p) (a)))

(define-integrable (or-aux p r) (if p p (r)))

;;; Assertion crud

(define-integrable (proclaim type obj)
  (ignorable type)                      ; ?
  (if-integrated obj (enforce type obj)))

(comment
(define-integrable (check-arg type obj fn)
  (if-integrated (if (type obj) obj
                   (*check-arg type obj fn))
                 (*check-arg type obj fn)))     ; forward reference

(define-integrable (enforce type obj)
  (if-integrated (if (type obj) obj
                   (*enforce type obj))
                 (*enforce type obj)))
)

;;; Really basic type predicates, not primitive to the compiler.

(define-integrable (null? x) (if x nil t))      ; (eq? x '())

(define-integrable (atom? x) (not (pair? x)))

(define-integrable (list? x) (if x (pair? x) t))  ; (or (null? x) (pair? x))

(define-integrable (null-list? x)
  (cond ((null? x) t)
        ((atom? x) (losing-non-null-list x))
        (else nil)))

(define-integrable (nonnegative-fixnum? x)
  (and (fixnum? x) (fixnum-not-negative? x)))

(define-integrable (eof? x)
  (eq? x *eof*))

(define-integrable (newline? c) (eq? c #\newline))

;;; There's a pattern to the following.

(define-integrable (vcell? x)
  (and (extend? x) (eq? (extend-template x) *vcell-template*)))

(define-integrable (symbol? x)
  (and (extend? x) (eq? (extend-template x) (symbol-template))))

(define-integrable (vector? x)
  (and (extend? x) (eq? (extend-template x) (vector-template))))

(define-integrable (bitv? x)
  (and (extend? x) (eq? (extend-template x) *bitv-template*)))

(define-integrable (bytev? x)
  (and (extend? x) (eq? (extend-template x) *bytev-template*)))

(define-integrable (escape-procedure? x)
  (and (extend? x) (eq? (extend-template x) *escape-procedure-template*)))

(define-integrable (unit? x)
  (and (extend? x) (eq? (extend-template x) *unit-template*)))

(define-integrable (xenoid? x)
  (and (extend? x) (eq? (extend-template x) *xenoid-template*)))

(define-integrable (bogus-entity? x)
  (and (extend? x) (eq? (extend-template x) *bogus-entity-template*)))

;;; Vector primitives are same as EXTEND primitives.

(define-integrable set-vector-elt
  (if-integrated set-extend-elt
                 ((guard () (proc) (vector? nonnegative-fixnum? true)
                         (lambda (vec idx val)
                           (ignore val)
                           (fx< idx (vector-length vec))))
                  (named-lambda set-vector-elt (vec idx val)
                                ((primop set-extend-elt) vec idx val)))))

(define-integrable vector-elt
  (if-integrated
   extend-elt
   (make-accessor ((guard () (proc) (vector? nonnegative-fixnum?)
                          (lambda (vec idx) (fx< idx (vector-length vec))))
                   (named-lambda vector-elt (vec idx)
                                 ((primop extend-elt) vec idx)))
                  set-vector-elt)))

(define-integrable (mem? pred obj list)
  (if (mem pred obj list) t nil))

(define-integrable (memq  obj list) (mem  eq? obj list))
(define-integrable (memq? obj list) (mem? eq? obj list))


;;; Character and string stuff.

(define-integrable (non-empty-string? x)
  (and (string? x) (not (string-empty? x))))

;;; Here we depend on characters being implemented as simply their ASCII code
;;; as fixnum with character type code adjoined.

(define-integrable (char? x)
  (and (misc? x)
       (pointer-less? x *number-of-char-codes*)))

(define-integrable (char->ascii c)
  (pointer-address (if-integrated c (check-arg char? c char->ascii))))

(define-integrable (ascii->char c)
  (make-pointer (if-integrated c (check-arg fixnum? c ascii->char))
                %%char-tag))

(define-integrable (string-tail s)
  (string-tail! (chopy (if-integrated s (check-arg string? s string-tail)))))

(define-integrable (string-nthtail s n)
  (string-nthtail! (chopy (if-integrated s (check-arg string? s
                                                      string-nthtail)))
                   n))


;;; Nonvalue stuff

(define-integrable (env-lookup env identifier local? create?)
  (env identifier local? create?))

(define-integrable (value->nonvalue x)
  (pointer-add (if-integrated x (check-arg extend? x value->nonvalue))
               *nonvalue-hack*))

(define-integrable (nonvalue->value x)
  (pointer-subtract x *nonvalue-hack*))

(define-integrable (nonvalue? x)
  (and (misc? x) (pointer-greater? x *nonvalue-hack*)))

(define-integrable vcell-nonvalue
  (if-integrated (lambda (vcell)        ; Should always yield an extend.
                   (nonvalue->value (vcell-contents vcell)))
                 (object (lambda (vcell)
                           (nonvalue->value (vcell-contents vcell)))
                         ((setter self) set-vcell-nonvalue))))

(define-integrable (set-vcell-nonvalue vcell x)
  (set (vcell-contents vcell) (value->nonvalue x)))

(declare-setter vcell-nonvalue set-vcell-nonvalue)

(define-integrable (vcell-has-value? vcell)
  (not (nonvalue? (vcell-contents vcell))))

;;; Arithmetic stuff

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

(define-integrable (fixnum-add1 x) (fixnum-add x 1))

(define-integrable (fixnum-even? x) (not (fixnum-odd? x)))

;;; Random low-level stuff

(define-integrable (change-tag obj from-tag to-tag)
  (pointer-add obj (fixnum->pointer (fixnum-subtract to-tag from-tag))))

;;; Hack

(define-integrable (buffer-size buffer)
; (assert (=0? (string-base buffer)))
  (text-length (string-pointer buffer)))

(define-integrable %vector-replace copy-mem)    ; GC needs
