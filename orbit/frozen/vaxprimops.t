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

(define-constant vax/addl2 #xc0)
(define-constant vax/addl3 #xc1)


;;; CALL-XENOID
;;;============================================================================

;(define-wired call-xenoid
  ;(primop call-xenoid ()
    ;((primop.generate self node)
     ;(generate-call-xenoid node))
    ;((primop.arg-specs self) nil)))


;;; (MAKE-LOCATION OFFSET)
;;; %SET-ACCESSOR
;;;===========================================================================
;;; Make accessor make an access primop with the given offset.  %set-accessor
;;; generates sets for access primops.

;(define-wired (make-location offset)
  ;(primop make-location (offset)
    ;((primop.location? self) t)
    ;((primop.location-offset self node)
     ;(car (primp.arglist self)))
    ;((primop.simplify self node)
     ;(simplify-location node))))

;(define-wired (make-accessor offset)
  ;(primop make-accessor (offset)
    ;((primop.generate self node)
     ;(generate-accessor node (primop.setter self)))
    ;((primop.accessor self)
     ;(lambda (node loc)
       ;(fixed-accessor node ((call-arg 2) loc) (primop.setter self))))
    ;((primop.arg-specs self) '(pointer))
    ;((primop.setter self) (fx+ -2 (car (primop.arglist self))))))
    ;
;(define-wired %set-accessor
  ;(primop %set-accessor ()
    ;((primop.generate self node)
     ;(generate-%set-accessor node))
    ;((primop.arg-specs self)
     ;'(pointer *))))


;(define-wired xenoid-pointer-loc
  ;(make-location 4))
  ;
;(define-wired extend-template-loc
  ;(make-location 0))

;(define-wired string-text-loc
  ;(make-location 4))
  ;
;(define-wired string-offset-loc
  ;(make-location 8))
  ;
;(define-wired car-loc
  ;(make-location 3))       ; (fx- 4 tag/pair))) No constant folding yet.
  ;
;(define-wired cdr-loc
  ;(make-location -1))      ; (fx- 0 tag/pair)))
  ;
;;; AREF

;(define-wired aref-loc
  ;(primop aref-loc ()
    ;((primop.location? self) t)
    ;((primop.location-offset self node)
     ;(generate-aref-offset node))
    ;((primop.simplify self node)
     ;(simplify-location node))))
     ;
;(comment
;(define-wired aref
  ;(primop aref ()
    ;((primop.generate self node)
     ;(generate-aref node))
    ;((primop.accessor self)
     ;(lambda (node loc)
       ;(aref-accessor node loc)))
    ;((primop.setter self)
     ;%set-aref)
    ;((primop.arg-specs self)
     ;'(pointer scratch))))

;(define-wired %set-aref
  ;(primop %set-aref ()
    ;((primop.generate self node)
     ;(generate-%set-aref node))
    ;((primop.arg-specs self)
     ;'(pointer scratch))))
;)

;;; The three location primops.  These generate code for locations the same
;;; way COMPARE does for EQ? etc.
;;;   (CAR-LOC (LAMBDA (X) (CONTENTS <cont> X) L) =>
;;;   (CONTENTS-LOCATION <cont> CAR-LOC L)
;;;
;;;   (CAR-LOC (LAMBDA (X) (SET-CONTENTS <cont> X A) L) =>
;;;   (SET-LOCATION <cont> CAR-LOC A L)  ;Value goes before arguments.
;;;
;(define-wired contents-location
  ;(primop contents-location ()))
  ;
;(define-wired set-location
  ;(primop set-location ()))
  ;
;(define-wired locative-location
  ;(primop locative-location ()))
  ;

;;; COMPARATORS
;;;===========================================================================

(define-wired (make-comparator branch-inst type) ; type is currently ignored
  (primop make-comparator (branch-inst type)
    ((primop.generate self node)
     (primitive-comparator node (car (primop.arglist self))))
    ((primop.presimplify self node)
     (presimplify-to-conditional node))
    ((primop.arg-specs self)
     '(* *))
    ((primop.conditional? self) t)
    ((primop.type self node)
     '(proc (cont) (cont) comp? top? top?))))
       
(define-wired fixnum-equal? (make-comparator 'jneq 'number?))
(define-wired fixnum-less?  (make-comparator 'jgeq 'number?))
(define-wired eq? (make-comparator 'jneq 'top?))

       

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
            ((primop.presimplify self node)
             (presimplify-predicate node))
            ((primop.type-predicate? self) t))))))

(define-wired (make-tag-type-predicate tag)
  (primop make-tag-type-predicate (tag)
    ((primop.test-code self node arg)
     (emit vax/cmpzv (machine-num 0) (machine-num 2)
                          arg (machine-num (car (primop.arglist self)))))
    ((primop.presimplify self node)
     (presimplify-predicate node))
    ((primop.type-predicate? self) t)))

(define-wired (make-header-type-predicate header)
  (primop make-header-type-predicate (header)
    ((primop.test-code self node arg)
     (emit vax/cmpb arg (machine-num (car (primop.arglist self)))))
    ((primop.presimplify self node)
     (presimplify-predicate node))
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
  (emit vax/cmpzv (machine-num 0) (machine-num 7)
                       arg (machine-num header/bignum)))


;;; HEADER CONSTRUCTORS
;;;============================================================================

(define-wired (make-header-maker tag)
  (primop make-header-maker (tag)
    ((primop.generate self node)
     (receive (source target) (one-arg-primitive node)
       (emit vax/ashl (machine-num 6) source target)
       (emit vax/movb (machine-num (car (primop.arg-list self)))
                   target)
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
       (emit vax/mcoml source target)
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
             (emit vax/ashl (machine-num shift) source target))
         (if tag
             (emit vax/movb (machine-num tag) source))
         (generate-move node source target)
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
           (emit (car (primop.arglist self)) l ar)
           (emit (cadr (primop.arglist self)) l ar d))
       (mark-continuation node d)))
    ((primop.arg-specs self) '(* *))
    ((primop.type self node) '(proc (cont number?) number? number?))))

(define-wired fixnum-add
  (simple-arithmetic-op vax/addl2 vax/addl3))

;(define-wired logxor
  ;(simple-arithmetic-op 'xorl2 'xorl3))

;(define-wired logior
  ;(simple-arithmetic-op 'bisl2 'bisl3))


(define-wired fixnum-multiply
  (primop fixnum-multiply ()
    ((primop.generate self node)
     (receive (l ar d) (generate-comm-binop node)
       (cond ((eq? (reg-type d) 'scratch)
              (emit vax/ashl (machine-num -2) ar d)
              (emit vax/mull2 l d))
             (else
              (let ((s (get-register 'scratch node '*)))
                (emit vax/ashl (machine-num -2) ar s)
                (emit vax/mull3 l s d))))
       (mark-continuation node d)))
    ((primop.arg-specs self) '(* *))
    ((primop.type self node) '(proc (cont number?) number? number?))))

(define-wired ash
  (primop ash ()
    ((primop.generate self node)
     (receive (l ar d) (generate-non-comm-binop node)
       (cond ((eq? (reg-type d) 'scratch)
              (emit vax/ashl (machine-num -2) l d)
              (emit vax/ashl d ar d))
             (else
              (let ((s (get-register 'scratch node '*)))
                (emit vax/ashl (machine-num -2) l s)
                (emit vax/ashl s ar d))))
       (mark-continuation node d)))
    ((primop.arg-specs self) '(scratch pointer))
    ((primop.type self node) '(proc (cont number?) number? number?))))

(define-wired fixnum-subtract
  (primop fixnum-subtract ()
    ((primop.generate self node)
     (receive (l ar d) (generate-non-comm-binop node)
       (if (eq? ar d)
           (emit vax/subl2 l ar)
           (emit vax/subl3 l ar d))
       (mark-continuation node d)))
    ((primop.arg-specs self) '(* *))
    ((primop.type self node) '(proc (cont number?) number? number?))))

(define-wired fixnum-divide
  (primop fixnum-divide ()
    ((primop.generate self node)
     (receive (l ar d) (generate-non-comm-binop node)
       (cond ((eq? (reg-type d) 'scratch)
              (emit vax/divl3 l ar d)
              (emit vax/ashl (machine-num 2) d d))
             (else
              (let ((s (get-register 'scratch node '*)))
                (emit vax/divl3 l ar s)
                (emit vax/ashl (machine-num 2) s d))))
       (mark-continuation node d)))
    ((primop.arg-specs self) '(* *))
    ((primop.type self node) '(proc (cont number?) number? number?))))
