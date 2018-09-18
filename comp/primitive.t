(herald primitive)

;;; These are all the necessary primops to bootstrap the compiler, and a
;;; macro to construct them.

;;; These primops are have a PRIMOP.ENVIRONMENT of NIL.  This means that the
;;; support for anything compiled using these will list them as being defined
;;; in that file.  The file should contain definitions of them.  The boot-
;;; strapping process is as follows:
;;;  1) Create a file with the definitions of these primops.
;;;  2) Load primitive.t into the compiler and then compile your file to
;;;     get a support file.
;;;  3) Compile your file using it's support file to initilize the
;;;     compiler.

(define-local-syntax (define-initial-primop id . clauses)
  (let ((name (concatenate-symbol 'primop/ id)))
   `(let ((primop (object nil
                    ((identification self) ',id)
                    ((primop? self) t)
                    ((primop.source primop) ',clauses)
                    ((primop.environment primop) nil)
                    ((primop.id primop) ',id)
                    . ,clauses)))
      (set (*primitive-primop-table* ',id) primop)
      (set (*primitive-support-table* ',id)
           (make-support-entry (create-variable 'id)
                               nil
                               nil
                               support/constant
                               (create-primop-node primop)))
      (lset ,name primop))))

;;; *PRIMOP
;;;============================================================================
;;; Needed to define primops.

(define-initial-primop *primop)

;;; Place marking primops
;;;============================================================================
;;; These are used by alpha to mark points in the tree.

(define-initial-primop continuation)
(define-initial-primop undefined)
(define-initial-primop undefined-effect)

;;; RECEIVE-VALUES
;;;============================================================================
;;; Used to alphatize block.

(define-initial-primop receive-values
  ((primop.presimplify self node)
   (presimplify-receive-values node)))

;;; *DEFINE-CONSTANT
;;;============================================================================
;;; Used to define primops.

(define-initial-primop *define-constant
  ((primop.defines-support? self) t)
  ((primop.uses-L-value? self) t)
  ((primop.support-variant self) support/constant))


;;; *DEFINE-WIRED
;;;============================================================================
;;; Used to define primops.

(define-initial-primop *define-wired
  ((primop.defines-support? self) t)
  ((primop.uses-L-value? self) t)
  ((primop.support-variant self) support/wired))
