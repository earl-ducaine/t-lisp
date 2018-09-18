(herald early
        (syntax-table *orbit-syntax-table*))

;;; Early binding (i.e. primop) database for ORBIT.

;;; ---------------------------------------------------------------------
;;; Support environments.

(define (make-support-env super id)
  (let ((table (make-table id)))
    (object (lambda (name)
              (or (table-entry table name)
                  (super name)))
            ((set-support-env-entry self name val)
             (set (table-entry table name) val))
            ((print self stream)
             (format stream "#{Support-env~_~S}" id)))))

(define-operation (set-support-env-entry env name val))

(define (make-empty-support-env id)
  (make-support-env false id))

(define support-env-entry
  (object (lambda (env name) (env name))
          ((setter self) set-support-env-entry)))

(define *primitive-support-env*
  (make-empty-support-env '*primitive-support-env*))

(define *standard-support-env* *primitive-support-env*)     ; for now at least

;;; ---------------------------------------------------------------------
;;; Syntactic sugar.

(define-local-syntax (early name . clauses)
  `(*define-support ,name
     (object nil
             ,@(map (lambda (clause)
                      `((,(symbolconc 'primop- (car clause)) %self%)
                        . ,(cdr clause)))
                    clauses)
             ((identification self) ,name)
             ((primop? self) t)
             ((print-type-string self) "Early-binding"))))

(define-predicate primop?)

(define-local-syntax (define-early name . clauses)
  `(define ,(symbolconc name '-primop)
     (early ',name ,@clauses)))

(define (*define-support name primop)
  (if name
      (set (support-env-entry *primitive-support-env* name)
           primop))
  primop)

;;; ---------------------------------------------------------------------
;;; Operations on primops.

(define-operation (primop-constant primop) *empty*)

(define-operation (primop-integrable primop) nil)

(define-operation (primop-type primop) 'top?)

(define-operation (primop-simplifier primop) false)

(define-operation (primop-generate primop) nil)

;;; ---------------------------------------------------------------------
;;; Ephemeral primops.

(define-early continuation)   ; Ephemeral; removed by CPS conversion.
(define-early set)            ; Ephemeral; removed by assignment analysis.
(define-early locative)       ; Ditto.
(define-early let-reference)  ; Ditto.

;;; ---------------------------------------------------------------------
;;; Combinator primops, etc.

(define-early Y
  (type '(proc cont? proc?)))
(define-early n-ary
  (type '(proc (cont proc?) proc?)))

(define-early block
  (type '(proc (cont) proc?)))
(define-early if
  (type '(proc (cont) (cont) type? top?)))

;;; ---------------------------------------------------------------------
;;; Data manipulation primops.

(define-early make-locative
  (type '(proc (cont (loc top?)) top?)))
(define-early value
  (type '(proc (cont top?) loc?)))
(define-early set-value
  (type '(proc (cont) loc? top?)))

(early 'u-field
  (type '(proc (cont u-integer?) integer? u-fixnum? u-fixnum?)))
(early 's-field
  (type '(proc (cont   integer?) integer? u-fixnum? u-fixnum?)))

;;; Fields within bit vectors
(early 'u-field-loc
  (type '(proc (cont (loc u-integer?)) bitv? u-fixnum? u-fixnum?)))
(early 's-field-loc
  (type '(proc (cont (loc s-integer?)) bitv? u-fixnum? u-fixnum?)))

;;; Fields within structured types
(early 'car-loc
  (type '(proc (cont (loc top?)) list?)))
(early 'cdr-loc
  (type '(proc (cont (loc top?)) list?)))
(early 'descriptor-loc
  (type '(proc (cont (loc top?)) escape?)))
(early 'v-loc       ; vector reference
  (type '(proc (cont (loc top?)) vector?)))
(early 'a-loc       ; array reference
  (type '(proc (cont (loc top?)) array?)))

;;; Comparisons
(early 'if-eq?
  (type '(proc (cont) (cont) top? top?)))
(early 'if-=
  (type '(proc (cont) (cont) number? number?)))
(early 'if-<
  (type '(proc (cont) (cont) number? number?)))
(early 'if-char=
  (type '(proc (cont) (cont) char? char?)))
(early 'if-char<
  (type '(proc (cont) (cont) char? char?)))

;;;  Coercion  -  ->float of various sorts?

;;; Storage allocators
(early 'make-pair)
(early 'make-bitv)
(early 'make-closure)
(early 'dynamic-state)
(early 'set-dynamic-state)

(early 'pointer->integer
  (type '(proc (cont u-integer?) top?)))
(early 'integer->pointer
  (type '(proc (cont top?) u-integer?)))

;;; ---------------------------------------------------------------------
;;; Exported primops.

(define-early values
  (simplifier simplify-values))

(define-early receive-values)

;;; Types
(define-early top?      ; universal type
  (type 'type?))
(define-early bottom?   ; empty type
  (type 'type?))
(define-early true?)
(define-early false?)
(define-early list?)
(early 'pair?)
(early 'vector?)
(early 'array?)

;;; Other
(early '+
  (type '(proc (cont number?) number? number?)))
(early '-)
(early '*)
(early '/)
(early 'truncate        ; as in common lisp - also round, floor, ceiling
  (type '(proc (cont integer? number?) number? number?)))

(early 'logior
  (type '(proc (cont integer?) integer? integer?)))
(early 'logxor)
(early 'logand)
(early 'lognot)
(early 'ash)

;;; ---------------------------------------------------------------------
;;; Constants.

(define-early nil
  (type 'false?))
(define-early t
  (type 'true?))
(define-early $
  (type 'bottom?))

(define *undefined-value* $-primop)
(define undefined-primop  $-primop)

;;; ---------------------------------------------------------------------
;;; Integrable procedures.

(early 'proclaim
  (integrable
   '(lambda (type val)
      ((primop if) (clambda () (continuation val)) $ type val))))

(early 'not
  (integrable
   '(lambda (test) (if test nil t))))       ; see open.t

(early 'car
  (integrable
   '(lambda (l) (value (car-loc l)))))

(early 'cdr
  (integrable
   '(lambda (l) (value (cdr-loc l)))))

(early 'eq?
  (integrable
   '(lambda (x y) (exits 2 if-eq? (clambda () t) (clambda () nil) x y))))

(early '=
  (integrable
   '(lambda (x y) (exits 2 if-= (clambda () t) (clambda () nil) x y))))

(early '<
  (integrable
   '(lambda (x y) (exits 2 if-< (clambda () t) (clambda () nil) x y))))

(early 'odd?
  (integrable
   '(lambda (x) (not (fx= (u-field x 0 1) 0)))))

(early 'call-with-continuation
  (integrable
   '(clambda (cont p)
      (exits 1 dynamic-state
          (clambda (state)
            (exits 1 p cont (clambda (ignored x)   ; do n-ary case later
                              (exits 1 set-dynamic-state
                                       (lambda ()
                                         (exits 0 cont x))
                                       state))))))))

;(early 'undefined-value
;  (integrable
;   '(lambda () $)))

;;; ---------------------------------------------------------------------
;;; Aliases.

(*define-support 'else t-primop)

(*define-support '**no-more-cond-clauses**    undefined-primop)
(*define-support '**let-missing-initializer** undefined-primop)
(*define-support 'undefined-effect            undefined-primop)

