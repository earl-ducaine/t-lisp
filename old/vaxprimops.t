(herald vaxprimops
        (support (make-empty-support-env 'nil) vaxbase))

;;; The PRIMOP.TYPE values are suggestions, none of them are currently used.

;;; CONSTANTS
;;;============================================================================

(define-constant tag/fixnum    0)
(define-constant tag/immediate 1)
(define-constant tag/extend    2)
(define-constant tag/pair      3)

(define-constant %car-offset% 1)
(define-constant %cdr-offset% -3)

(define-constant header/char            1)
(define-constant header/bitv            9)
(define-constant header/text           17)
(define-constant header/general-vector 25)
(define-constant header/bignum         37)
(define-constant header/slice          41)
(define-constant header/string         41)
(define-constant header/symbol         49)

;;; CALL-XENOID
;;;============================================================================

(define-wired call-xenoid
  (primop call-xenoid ()
    ((primop.generate self node)
     (generate-call-xenoid node))
    ((primop.arg-specs self) nil)))


;;; (MAKE-ACCESSOR OFFSET)
;;; %SET-ACCESSOR
;;;===========================================================================
;;; Make accessor make an access primop with the given offset.  %set-accessor
;;; generates sets for access primops.

(define-wired (make-accessor offset)
  (primop make-accessor (offset)
    ((primop.generate self node)
     (generate-accessor node (primop.setter self)))
    ((primop.accessor self)
     (lambda (node loc)
       (fixed-accessor node ((call-arg 2) loc) (primop.setter self))))
    ((primop.arg-specs self) '(pointer))
    ((primop.setter self) (fx+ -2 (car (primop.arglist self))))))

(define-wired %set-accessor
  (primop %set-accessor ()
    ((primop.generate self node)
     (generate-%set-accessor node))
    ((primop.arg-specs self)
     '(pointer *))))

(define-wired xenoid-pointer
  (make-accessor 4))

(define-wired extend-template
  (make-accessor 0))

(define-wired string-text
  (make-accessor 4))

(define-wired string-offset
  (make-accessor 8))

(define-wired car
  (make-accessor 3))       ; (fx- 4 tag/pair))) No constant folding yet.

(define-wired cdr
  (make-accessor -1))      ; (fx- 0 tag/pair)))

;;; AREF and %SET-AREF
;;;===========================================================================
;;; Aref and its setter.

(define-wired aref
  (primop aref ()
    ((primop.generate self node)
     (generate-aref node))
    ((primop.accessor self)
     (lambda (node loc)
       (aref-accessor node loc)))
    ((primop.setter self)
     %set-aref)
    ((primop.arg-specs self)
     '(pointer scratch))))

(define-wired %set-aref
  (primop %set-aref ()
    ((primop.generate self node)
     (generate-%set-aref node))
    ((primop.arg-specs self)
     '(pointer scratch))))


;;; COMPARATORS
;;;===========================================================================

(define-wired (make-comparator branch-inst type) ; type is currently ignored
  (primop make-comparator (branch-inst type)
    ((primop.generate self node)
     (primitive-comparator node (car (primop.arglist self))))
    ((primop.simplify self node)
     (simplify-to-conditional node))
    ((primop.arg-specs self)
     '(* *))
    ((primop.conditional? self) t)
    ((primop.type self node)
     '(proc (cont) (cont) comp? top? top?))))
       
(define-wired fixnum-equal? (make-comparator 'jneq 'number?))
(define-wired fixnum-less?  (make-comparator 'jgeq 'number?))
(define-wired eq? (make-comparator 'jgeq 'top?))

       
;;; STORAGE ALLOCATORS
;;;===========================================================================

(define-wired %make-pair
  (primop %make-pair ()
    ((primop.generate self node)
     (generate-early-binding-call node))
    ((primop.integrate? self node) nil)
    ((primop.arg-specs self) '())
    ((primop.used-registers self) '(5 6))
    ((primop.return-reg self) 5)))

(define-wired %make-extend
  (primop %make-extend ()
    ((primop.generate self node)
     (generate-early-binding-call node))
    ((primop.integrate? self node) nil)
    ((primop.arg-specs self) '(5 6))
    ((primop.used-registers self) '(7))
    ((primop.return-reg self) 5)))


;;; TYPE PREDICATES
;;;===========================================================================

(define-local-syntax (define-type-predicate name variant . rest)
  `(define-wired ,name
     ,(case variant
        ((tag)
         `(make-tag-type-predicate . ,rest))
        ((header)
         `(make-header-type-predicate . ,rest))
        (else
         `(primop ,name ()
            ((primop.test-code self . ,(car rest))
             . ,(cdr rest))
            ((primop.simplify self node)
             (simplify-predicate node))
            ((primop.type-predicate? self) t))))))

(define-wired (make-tag-type-predicate tag)
  (primop make-tag-type-predicate (tag)
    ((primop.test-code self node arg)
     (icreate node "cmpzv" (machine-num 0) (machine-num 2)
                          arg (machine-num (car (primop.arglist self)))))
    ((primop.simplify self node)
     (simplify-predicate node))
    ((primop.type-predicate? self) t)))

(define-wired (make-header-type-predicate header)
  (primop make-header-type-predicate (header)
    ((primop.test-code self node arg)
     (icreate node "cmpb" arg (machine-num (car (primop.arglist self)))))
    ((primop.simplify self node)
     (simplify-predicate node))
    ((primop.type-predicate? self) t)))

(define-type-predicate list?      tag tag/pair)
(define-type-predicate extend?    tag tag/extend)
(define-type-predicate fixnum?    tag tag/fixnum)
(define-type-predicate immediate? tag tag/immediate)

(define-type-predicate char?                  header header/char)
(define-type-predicate general-vector-header? header header/general-vector)
(define-type-predicate bitv-header?           header header/bitv)
(define-type-predicate text-header?           header header/text)
(define-type-predicate string?                header header/string)
(define-type-predicate symbol-header?         header header/symbol)

(define-type-predicate bignum-header? t (node arg)
  (icreate node "cmpzv" (machine-num 0) (machine-num 7)
                       arg (machine-num header/bignum)))


;;; HEADER CONSTRUCTORS
;;;============================================================================

(define-wired (make-header-maker tag)
  (primop make-header-maker (tag)
    ((primop.generate self node)
     (receive (source target) (one-arg-primitive node)
       (icreate node "ashl" (machine-num 6) (r source) (r target))
       (icreate node
                "movb"
                (machine-num (car (primop.arg-list self)))
                (r target))
       (mark-continuation node target)))
    ((primop.arg-specs self)
     '(scratch))))

(define-wired %make-text-header
  (make-header-maker header/text))

(define-wired %make-string-header
  (make-header-maker header/string))

(define-wired %make-symbol-header
  (make-header-maker header/symbol))


;;; LOGNOT
;;;==========================================================================

(define-wired lognot
  (primop lognot ()
    ((primop.generate self node)
     (receive (source target) (one-arg-primitive node)
       (icreate node "mcom" (r source) (r target))
       (mark-continuation node target)))
    ((primop.arg-specs self)
     '(scratch))))

;;; TYPE CONVERTERS
;;;==========================================================================

(define-wired (make-type-converter shift tag arg-specs)
  (primop make-type-converter (shift tag arg-specs)
    ((primop.generate self node)
     (let* ((args (primop.args self))
            (shift (car args))
            (tag (cadr args)))
       (receive (source target)
                (one-arg-primitive node)
         (if shift
             (icreate node 'ashl (machine-num shift) (r source) (r target)))
         (if tag
             (icreate node 'movb (machine-num tag) (r source)))
         (generate-move node (r source) (r target))
         (mark-continuation node target))))
    ((primop.arg-specs self) (nth 2 (primop.arg-list)))))

(define-wired text->symbol!
  (make-type-converter '#f header/symbol '(*)))

(define-wired char->ascii
  (make-type-converter -6 '#f '(*)))

(define-wired ascii->char
  (make-type-converter 6 header/char '(*)))

;;; ARITHMETIC
;;;===========================================================================

(define-wired (simple-arithmetic-op inst1 inst2)
  (primop simple-arithmetic-op (inst1 inst2)
    ((primop.generate self node)
     (receive (l ar d) (generate-comm-binop node)
       (if (eq? ar d)
           (icreate node (car  (primop.arglist self)) (r l) (r ar))
           (icreate node (cadr (primop.arglist self)) (r l) (r ar) (r d)))
       (mark-continuation node d)))
    ((primop.arg-specs self) '(* *))
    ((primop.type self node) '(proc (cont number?) number? number?))))

(define-wired fixnum-add
  (simple-arithmetic-op 'addl2 'addl3))

(define-wired logxor
  (simple-arithmetic-op 'xorl2 'xorl3))

(define-wired logior
  (simple-arithmetic-op 'bisl2 'bisl3))


(define-wired fixnum-multiply
  (primop fixnum-multiply ()
    ((primop.generate self node)
     (receive (l ar d) (generate-comm-binop node)
       (cond ((eq? (reg-type d) 'scratch)
              (icreate node 'ashl (machine-num -2) (r ar) (r d))
              (icreate node 'mull2 (r l) (r d)))
             (else
              (let ((s (get-register 'scratch node '*)))
                (icreate node 'ashl (machine-num -2) (r ar) (r s))
                (icreate node 'mull3 (r l) (r s) (r d)))))
       (mark-continuation node d)))
    ((primop.arg-specs self) '(* *))
    ((primop.type self node) '(proc (cont number?) number? number?))))

(define-wired ash
  (primop ash ()
    ((primop.generate self node)
     (receive (l ar d) (generate-non-comm-binop node)
       (cond ((eq? (reg-type d) 'scratch)
              (icreate node 'ashl (machine-num -2) (r l) (r d))
              (icreate node 'ashl (r d) (r ar) (r d)))
             (else
              (let ((s (get-register 'scratch node '*)))
                (icreate node 'ashl (machine-num -2) (r l) (r s))
                (icreate node 'ashl (r s) (r ar) (r d)))))
       (mark-continuation node d)))
    ((primop.arg-specs self) '(scratch pointer))
    ((primop.type self node) '(proc (cont number?) number? number?))))

(define-wired fixnum-subtract
  (primop fixnum-subtract ()
    ((primop.generate self node)
     (receive (l ar d) (generate-non-comm-binop node)
       (if (eq? ar d)
           (icreate node 'subl2 (r l) (r ar))
           (icreate node 'subl3 (r l) (r ar) (r d)))
       (mark-continuation node d)))
    ((primop.arg-specs self) '(* *))
    ((primop.type self node) '(proc (cont number?) number? number?))))

(define-wired fixnum-divide
  (primop fixnum-divide ()
    ((primop.generate self node)
     (receive (l ar d) (generate-non-comm-binop node)
       (cond ((eq? (reg-type d) 'scratch)
              (icreate node 'divl3 (r l) (r ar) (r d))
              (icreate node 'ashl (machine-num 2) (r d) (r d)))
             (else
              (let ((s (get-register 'scratch node '*)))
                (icreate node 'divl3 (r l) (r ar) (r s))
                (icreate node 'ashl (machine-num 2) (r s) (r d)))))
       (mark-continuation node d)))
    ((primop.arg-specs self) '(* *))
    ((primop.type self node) '(proc (cont number?) number? number?))))
