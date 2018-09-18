(herald (tsys operation t 113)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; %OPERATION and friends

(define-integrable (operation? obj)
  (and (extend? obj) (eq? (extend-template obj) *operation-template*)))

;;; Jump to the object's handler to obtain a method for an operation.

(define-integrable (%dispatch obj state)
  ((vref *handlers* (pointer-tag obj)) obj state))

;;; (PERFORM-DISPATCH obj state)
;;;        This is called by OPERATE to get the method for performing
;;;        the operation.

;(define (perform-dispatch obj state)
;  (%dispatch obj state))

(lset *dispatch-cache-enabled?* t)
(lset *dispatch-cache-hits*     0)
(lset *dispatch-cache-misses*   0)

(define (perform-dispatch obj state)
  (if (or (not *dispatch-cache-enabled?*)
	  (not (operation? op)))		;For trace package
      (%dispatch obj state)
      (let ((op (%state-op state)))
        (cond ((and (eq? obj (%operation-cache-obj op))
                    (neq? obj '%%flushed%%))        ;Unfortunate.
               (set (%state-obj state)  (%operation-cache-new-obj op))
               (set (%state-next state) (%operation-cache-new-next op))
               (modify *dispatch-cache-hits* (lambda (x) (fx+ x 1)))
               (%operation-cache-method op))
              (else
               (let ((method (%dispatch obj state)))
                 (set (%operation-cache-obj      op) obj)
                 (set (%operation-cache-new-obj  op) (%state-obj state))
                 (set (%operation-cache-new-next op) (%state-next state))
                 (set (%operation-cache-method   op) method)
                 (modify *dispatch-cache-misses* (lambda (x) (fx+ x 1)))
                 method))))))

;;; Create an operation.

(define (%operation default spect id h)
  (let ((op (make-extend-n *operation-template* %%operation-size)))
    (set (%operation-default     op)
         (or default no-default-method))
    (set (%operation-argspectrum op) spect)
    (set (%operation-id          op) id)
    (set (%operation-handler     op) h)
    (set (%operation-cache-obj   op) '%%flushed%%)
    (add-to-population *operation-population* op)
    op))

(comment
 (define (%operation default spect id h)
  (labels ((op (object (lambda (obj . args)
                         (apply operate op obj *the-buck-stops-here* obj args))
                       ((argspectrum self) spect)
                       ((identification self) id)
                       ((default-method self) default)
                       ((operation? self) t)
                       (=> h))))
    op))

 (define (operate obj op next self . args)
   (receive (method obj op next)
            (perform-dispatch obj op next)
     (apply method obj op next args)))

)

(define *operation-population* (make-population '*operation-population*))

(push *pre-gc-agenda*
      (cons 'operation-pre-gc-hook
            (lambda ()
              (walk-population-unsafely
                 (lambda (op)
                   (set (%operation-cache-obj      op) '%%flushed%%)
                   (set (%operation-cache-new-obj  op) '%%flushed%%)
                   (set (%operation-cache-new-next op) '%%flushed%%)
                   (set (%operation-cache-method   op) '%%flushed%%)
                  )
                 *operation-population*))))

;;; Say (RET procedure) to proceed from this error.
;;; The value returned will be the value of the call to the operation.

(define (no-default-method obj op next . args)
  (ignore obj next)
  (error "operation not handled~%  ~S"
         `(,(or (identification op) op) . ,args)))

(define (%massage-default default)
  (cond ((extend? default)
         (%method (#f self . args)
           (apply default self args)))
        (else no-default-method)))

(define (join . objects)
  (cond ((null? objects) (object nil))
        ((null? (cdr objects)) (car objects))
        ((null? (cddr objects)) (join2 (car objects) (cadr objects)))
        (else (join2 (car objects)
                     (apply join (cdr objects))))))

(define (join2 lhs rhs)
  (cond ((joined? lhs)
         (join2 (joined-lhs lhs) (join2 (joined-rhs lhs) rhs)))
        (else
         (%object lhs
                  (lambda (obj state)
                    (set (%state-obj state) lhs)
                    (set (%state-next state) rhs)
                    (%dispatch lhs state))))))

(define (joined? obj) nil)        ; Bug.
;(define (joined-lhs obj) ...)        ; Bug.
;(define (joined-rhs obj) ...)        ; Bug.

(define (%predicate id)
  (%operation (%method (#f #f) nil)
              '(1)
              id
              (%handler #f
                        ((print-type-string self) "Predicate")
                        (=> handle-operation))))

;;; Called as a last resort by any handler which fails to find a method.

(define (%dispatch-next obj state)
  (let ((obj (%state-next state)))
    (set (%state-obj state) obj)
    (set (%state-next state) *the-buck-stops-here*)
    (%dispatch obj state)))

;;; The thing that the "next" guy is when he isn't something else.

(define *the-buck-stops-here*
  (%object nil
           (lambda (obj state)
             (ignore obj)
             (let ((op (%state-op state)))
               (cond ((operation? op)
                      (%operation-default op))
                     (else                ;For trace package!
                      (default-method op)))))))

;;; Must precede any DEFINE-OPERATION.

(define handle-operation
  (%handler op
            ((argspectrum self)    (%operation-argspectrum op))
            ((identification self) (%operation-id op))
	    ((set-identification self id)
	     (if (not (%operation-id op))
		 (set (%operation-id op) id)))
            ((default-method self) (%operation-default op))
            ((get-loaded-file self)
             (get-loaded-file (%operation-default op)))
            ((print-type-string self) "Operation")))

(define-operation (default-method op))

;;; Mutable handlers.
;;; Note that there's a bootstrap problem; ASSQ or HASSQ might not be
;;;  available this soon.
;;; - There's a bug in this: *DEFINE-METHOD should really be associating the
;;;    method not with the operation object itself but with its "name", so
;;;    to speak.  There are a couple of ways to make this work: pass it
;;;    (LAMBDA () OP), or (LOCATIVE OP), or (SYNONYM OP).  Worry about
;;;    efficiency (the overhead of a procedure call or locative lookup)? ...

(define (make-mutable-handler id)
  (let ((methods '())
        (default-handler %dispatch-next))
    (object (lambda (obj state)
              ;;(cond ((assq op methods) => cdr)
              ;;      (else (default-handler obj state)))
              (let ((op (%state-op state)))
                (iterate loop ((m methods))
                  (cond ((null? m) (default-handler obj state))
                        ((eq? op (car (car m))) (cdr (car m)))
                        (else (loop (cdr m)))))))
            ((*define-method self op method)
             (let ((op (check-arg operation? op *define-method)))
               (iterate loop ((m methods))   ; ... assq
                 (cond ((null? m)
                        (set methods (cons (cons op method) methods)))
                       ((eq? op (car (car m)))
                        (set (cdr (car m)) method))
                       (else (loop (cdr m)))))
               ;(flush-dispatch-cache)
               method))
            ((set-default-handler self h)
             ;(flush-dispatch-cache)
             (set default-handler h))
            ((identification self) id)
            ((print-type-string self) "Mutable-handler"))))

(define-operation (*define-method handler op method))
(define-operation (set-default-handler handler other-handler))

;;; Handlers

(define *handlers*
  (vector-fill (make-vector 8) %dispatch-next))

;;; Extends are handled by gross hack implemented in CHUNK module.

(vset *handlers* %%extend-tag handle-extend)

;;; Handlers for primitive pointer types.

(define handle-fixnum   (make-mutable-handler 'handle-fixnum))
(define handle-flonum   (make-mutable-handler 'handle-flonum))
(define handle-pair     (make-mutable-handler 'handle-pair))
(define handle-string   (make-mutable-handler 'handle-string))
(define handle-template (make-mutable-handler 'handle-template))

(vset *handlers* %%fixnum-tag   handle-fixnum)
(vset *handlers* %%flonum-tag   handle-flonum)
(vset *handlers* %%pair-tag     handle-pair)
(vset *handlers* %%string-tag   handle-string)
(vset *handlers* %%template-tag handle-template)

;;; Handlers for types implemented with "misc" tag.

(define handle-null     (make-mutable-handler 'handle-null))
(define handle-char     (make-mutable-handler 'handle-char))
(define handle-nonvalue (make-mutable-handler 'handle-nonvalue))

(vset *handlers* %%misc-tag
      (lambda (obj state)
        (cond ((null? obj)     (handle-null obj state))
              ((char? obj)     (handle-char obj state))
              ((nonvalue? obj) (handle-nonvalue obj state))
              (else            (%dispatch-next obj state)))))

;;; Primitive EXTEND types.  Compare this list with that in KERNEL.
;;; (Primitive frame types are defined in FRAME module.)

(define handle-symbol           (make-mutable-handler 'handle-symbol))
(define handle-vcell            (make-mutable-handler 'handle-vcell))
(define handle-vector           (make-mutable-handler 'handle-vector))
(define handle-bitv             (make-mutable-handler 'handle-bitv))
(define handle-bytev            (make-mutable-handler 'handle-bytev))
(define handle-xenoid           (make-mutable-handler 'handle-xenoid))
(define handle-escape-procedure (make-mutable-handler 'handle-escape-procedure))

;;; SETTER and "settable operations."
;;; Use CONS instead of LIST because compiler isn't smart enough to do
;;; anything besides close-compile LIST, and LIST isn't available at the
;;; time this module is initialized.  (CONS is.)

(define (%settable-operation default spect id)
  (%operation default
              spect
              id
              (let ((the-setter
                     (%operation nil
                                 (increment-argspectrum spect)
                                 (cons 'setter (cons id '()))
                                 handle-operation)))
                (%handler #f
                          ((setter op) the-setter)
                          (=> handle-operation)))))

;;; Trivial utility

(define (increment-argspectrum spect)
  (cons (fx+ (car spect) 1)
        (cond ((fixnum? (cdr spect)) (fx+ (cdr spect) 1))
              (else (cdr spect)))))

;;; Note: evaluating the following does an operation dispatch, and so must
;;; follow the (VSET *HANDLERS* %%EXTEND-TAG ...).

(define-operation (setter proc))

;;; Standard very-general-purpose operations

(define-operation (set-identification obj id) nil)   ;do nothing by default

(define identification
  (operation (lambda (obj)
               (cond ((bogus-entity? obj)
                      (identification (bogus-entity-procedure obj)))
                     ((extend? obj)
                      (let ((d (extend-definee obj)))
                        (if (vcell? d) (vcell-id d) d)))
                     (else nil)))
    ((setter self) set-identification)
    ((identification self) 'identification)))

(define-operation (procedure? obj)
  (cond ((bogus-entity? obj) (procedure? (bogus-entity-procedure obj)))
        ((and (extend? obj) (extend-procedure? obj)) t)
        (else nil)))

(define-operation (argspectrum obj)
  (cond ((bogus-entity? obj)
         (argspectrum (bogus-entity-procedure obj)))
        ((and (extend? obj) (extend-procedure? obj))
         (extend-argspectrum obj))
        (else
         (error "(~S ~S): object not callable" 'argspectrum obj))))

(define-operation (print obj stream)
  (print-random obj stream))

(define-operation (display obj stream)
  (print obj stream))

(define-operation (print-type-string obj)
  (print-type-string-random obj))

;;; IMMUTABLE? - for making objects be read-only.

(define-settable-operation (mutable? obj))

(define set-mutable? (setter mutable?))

(define-operation (set-immutable obj)
  (set-mutable? obj nil))

(define (immutable? obj) (not (mutable? obj)))

;;; Hacko definition of HANDLER?

(define (handler? obj) (procedure? obj))

(define (pre-dispatch op obj) op)        ;Fix later
