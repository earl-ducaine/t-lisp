(herald simplifiers
        (syntax-table *orbit-syntax-table*))

;;; Procedures to simplify calls to various primops

;;; (SIMPLIFY-TEST node)
;;;=========================================================================
;;; (PRIMOP/CONDITIONAL <exit1> <exit2> PRIMOP/TEST PRIMOP/TRUE? #F)
;;; => <exit2>
;;; (PRIMOP/CONDITIONAL <exit1> <exit2> PRIMOP/TEST PRIMOP/TRUE? not-#F)
;;; => <exit1>

(define (simplify-test node)
  (destructure (((exit-1 exit-2 #f test val) (call-args node)))
    (cond ((not (and (primop-ref? test primop/true?)
                     (literal-node? val)))
           nil)
          ((eq? '#f (primop-value val))
           (replace-test node exit-2)
           t)
          (else
           (replace-test node exit-1)
           t))))

(define (replace-test call-node new-node)
  (let ((new-call (create-call-node 1 0)))
    (detach new-node)
    (relate call-proc new-call new-node)
    (replace call-node new-call)))

;;; (SIMPLIFY-DEFINE-CONSTANT node)
;;;=========================================================================
;;;    Check to see if the value has been simplified to something suitable
;;; for support.

(define (simplify-define-constant node)
  (let ((original (get-support-from-ref support/wired ((call-arg 2) node))))
    (cond ((not original)
           nil)
          ((suitable-constant-support? original)
           (change-support-value node original)
           nil)
          ((lambda-node? original)
           (set (primop-value (call-proc node)) primop/*define-wired)
           (change-support-variant (reference-variable ((call-arg 2) node))
                                   support/wired)
           (change-support-value node original)
           t)
          (else
           nil))))

;;; (SIMPLIFY-Y node)
;;;==========================================================================
;;; (Y (lambda (B ...) (B K0))
;;;    (lambda (K1 ...)
;;;      (K1 (lambda (K2) . <body>) ...)))
;;; Substitute K0 for K2, since it is not necessary to actually call the body.

(define (simplify-Y node)
  (destructure (((#f l1 l2) (call-proc+args node)))
    (let ((var (lambda-cont-var ((call-arg 1) (lambda-body l2)))))
      (cond ((used? var)
             (substitute var ((call-arg 1) (lambda-body l1)) nil)
             t)
            (else
             nil)))))

;;; (SIMPLIFY-LOCATION node)
;;;============================================================================
;;;    A location primop supposedly returns a locative.  This does simple
;;; representation analysis to elide the call to the location primop if the
;;; locative is not really needed.  This can go away once real representation
;;; analysis exists.
;;;
;;; (CONTENTS (<location> . <args>)) => (CONTENTS-LOCATION <location> . <args>)
;;; (SET-CONTENTS (<location> . <args>) <value>) =>
;;;     (SET-LOCATION <location> <value> . <args>)
;;; Otherwise (<location> . <args>) => (LOCATIVE-LOCATION <location> . <args>)

(define (simplify-location node)
  (destructure (((location cont . args) (copy-list (call-proc+args node))))
    (walk detach (call-proc+args node))
    (receive (primop new-cont new-args)
             (parse-location cont args)
      (let ((new-call (create-call-node (fx+ 3 (length new-args)) 1)))
        (relate call-proc new-call (primop-lookup primop
                                                  *standard-support-env*))
        (relate-call-args new-call `(,new-cont ,location . ,new-args))
        (replace node new-call)
        t))))

(define (parse-location cont args)
  (cond ((not (and (lambda-node? cont)
                   (null? (cdr (variable-refs (car (lambda-variables cont)))))
                   (variable-ref? ((call-arg 2) (lambda-body cont))
                                   (car (lambda-variables cont)))
                   (let ((p (known-primop (call-proc (lambda-body cont)))))
                     (or (eq? p primop/contents)
                         (eq? p primop/set-contents)))))
         (values 'locative-location cont args))
        ((eq? (known-primop (call-proc (lambda-body cont))) primop/contents)
         (values 'contents-location
                 (detach ((call-arg 1) (lambda-body cont)))
                 args))
        (else
         (values 'set-location
                 (detach ((call-arg 1) (lambda-body cont)))
                 `(,(detach ((call-arg 3) (lambda-body cont))) . ,args)))))
