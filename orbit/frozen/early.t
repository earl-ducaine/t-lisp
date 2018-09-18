(herald early
        (syntax-table *orbit-syntax-table*))

;;; Early binding database for ORBIT.  DEFINE and its cousins generate early
;;; binding information, usually referred to as support, for the variable
;;; defined.  Currently the only information generated, besides the fact that
;;; the variable have been defined, is an integrable value and then only if
;;; the defining special form is of the proper type.

;;; (MAKE-SUPPORT-TABLE id primop-table)
;;;============================================================================
;;;     A support table contains the support information for a given locale.
;;;  PRIMOP-TABLE is a table containing all of the primops defined in the
;;;  locale.
;;;
;;;  (<table> <symbol>) => the support for <symbol> in the locale' this
;;;      is settable.
;;;  (LOCAL-SUPPORT <table>) => a list of all supported variables with their
;;;      support.
;;;  (PRIMOP-SUPPORT <table>) => the primop table of the locale.
;;;  (SUPPORT-ENV-ID <table> <symbol>) => the ID of the table if <symbol> has
;;;      support in it and NIL otherwise.  This is used to get the specific
;;;      table id of the table that supports a given name within a support
;;;      environment.

(define (make-support-table id primop-table)
  (let ((table (make-table id)))
    (object (lambda (name)
              (table-entry table name))
      ((setter self)
       (lambda (name value)
         (set (table-entry table name) value)))
      ((local-support self)
       (select-from-table (lambda (key entry)
                            (ignore key)
                            (support.local? entry))
                          table))
      ((primop-support self name)
       (primop-table name))
      ((support-env-id self name)
       (if (table-entry table name)
           id
           nil))
      ((print self stream)
       (format stream "#{Support-table ~D ~S}" (object-hash self) id)))))

(define-operation (local-support env proc))
(define-operation (primop-support env proc))
(define-operation (support-env-id env name) nil)

(define (make-empty-support-table id)
  (make-support-table id (make-primop-table id)))

;;; (MAKE-PRIMOP-TABLE id)
;;;============================================================================
;;;    The primops defined in a locale have a distinct name space.  This allows
;;; primops to be anonymous in the regular name space and still be referred to
;;; by name by the support tables and files.  Primop tables are essentially
;;; identical to support tables.

(define (make-primop-table id)
  (let ((table (make-table id)))
    (object (lambda (name)
              (table-entry table name))
            ((setter self)
             (lambda (name value)
               (set (table-entry table name) value)))
            ((print self stream)
             (format stream "#{Primop-table ~D ~S}" (object-hash self) id)))))

;;; (MAKE-SUPPORT-ENV id support-tables)
;;;============================================================================
;;;  A support environment is made from a list of support tables.  It responds
;;; to information requests by trying each support table in turn.  The method
;;; for AUGMENT-CONTEXT is used by the HERALD special form.

(define (make-support-env id support-tables)
  (object (lambda (name)
            (any (lambda (p)
                   (p name))
                 support-tables))
   ((support-env-id self name)
    (any (lambda (p)
           (support-env-id p name))
         support-tables))
   ((primop-support self name)
    (any (lambda (p)
           (primop-support p name))
         support-tables))
   ((augment-context self . rest)
    (get-support-environment id support-tables rest))
   ((print self stream)
    (format stream "#{Support-env ~D ~S}" (object-hash self) id))))

(define (make-empty-support-env id)
  (make-support-env id '()))

;;; *PRIMITIVE-PRIMOP-TABLE*
;;; *PRIMITIVE-SUPPORT-TABLE*
;;; *PRIMITIVE-SUPPORT-ENV*
;;; *STANDARD-SUPPORT-ENV*
;;;===========================================================================
;;;   A starter set of tables and environments.

(define *primitive-primop-table*
  (make-primop-table '*primitive-primop-table*))

(define *primitive-support-table*
  (make-support-table '*primitive-support-table* *primitive-primop-table*))

(define *primitive-support-env*
  (make-support-env '*primitive-support-env* `(,*primitive-support-table*)))

(lset *standard-support-env* *primitive-support-env*)

;;; SUPPORT
;;;============================================================================
;;;   Structure to hold support information for a symbol in a particular
;;; environment.

(define-structure-type support
  variable  ; The variable being supported.
  local?    ; Is this support defined in the locale currently being compiled?
  variant   ; What kind of support this is, one of SUPPORT/DEFINE etc.
  value     ; The integrable value of the support.  This is a procedure of
            ;  no arguments that returns a node.
  data      ; An a-list containing any other support information.  Should this
  )         ;   be a table?

;;; The structure above started out life as an object so all of the names
;;; had . instead of - and I found I liked them better that way.
(define-integrable support.variable support-variable)
(define-integrable support.local?   support-local?)
(define-integrable support.all-data support-data)
(define-integrable support.variant  support-variant)

(define-settable-operation (support.data self))
(define support.value
  (object (lambda (support)
            ((support-value support)))
    ((setter self) (setter support-value))))

(define-methods handle-support
  ((print self stream)
   (format stream "#{Support~_~S}" (object-hash self)))
  ((support.data self key)
   (cdr (assq key data)))
  (((setter support.data) self key new)
     (cond ((assq key data)
            => (lambda (entry)
                 (set (cdr entry) new)))
           (else
            (push data `(,key . ,new))))))

(define (make-support-entry var local? data variant value)
  (let ((s (make-support)))
    (set (support-variable s) var)
    (set (support-local?   s) local?)
    (set (support-variant  s) variant)
    (set (support-value    s) value)
    (set (support-data     s) data)
    s))

;;; SUPPORT VARIANTS
;;;============================================================================
;;;   These are the different kinds of support.  A permanent support variant
;;; is one where the user has forfeited the right to redefine the variable
;;; later, thus the value can be integrated, etc.

(define-predicate support-variant?)
(define-predicate permanent-support-variant?)

(define-local-syntax (define-support-variant name constant?)
  (let ((name (concatenate-symbol 'support/ name)))
    `(define ,name
       (object nil
         ((support-variant? self) t)
         ((identification self) ',name)
         ,@(if constant?
               '(((permanent-support-variant? self) t))
               '())))))

(define-support-variant lset       ())
(define-support-variant define     ())
(define-support-variant constant    t)
(define-support-variant integrable  t)
(define-support-variant wired       t)
(define-support-variant multiple    t)  ;This is never used...

;;; (SUPPORTED? var)
;;;============================================================================
;;;   Is VAR defined in some environment?

(define (supported? var)
  (if (variable-support-env var) t nil))

;;; (VARIABLE-SUPPORT var)
;;;============================================================================
;;;   The support entry for VAR in the evironment its defined in.  Should only
;;; be called if it is known that there is such an environment.

(define (variable-support var)
  (cond ((variable-support-env var)
         => (lambda (env)
              (env (variable-name var))))
        (else
         (bug "there is no support for ~S" var))))

;;; (ADD-SUPPORT-VALUE var variant value)
;;;============================================================================
;;;   Make VALUE the support value of VAR.  If VALUE is not a primop node or a
;;; literal node then it may be changed by simplification.  Thus VALUE itself
;;; is not stored, but is recomputed every time it needed.  Note that this has
;;; nothing to do with whether or not the VARIANT is a 'permanent' one.

(define (add-support-value var variant value)
  (let ((name (variable-name var))
        (env  (variable-support-env var))
        (permanent? (and value
                         (or (primop-node? value)
                             (literal-node? value)))))
    (cond ((env name)
           => (lambda (support)
                (set (support.value support)
                     (if permanent?
                         (lambda () value)
                         (lambda () (get-support-definition var variant))))))
          (else
           (bug "adding support value of ~S but it has no support" name)))
    name))

;;; (CHANGE-SUPPORT-VARIANT var variant)
;;;============================================================================
;;;   Change the support variant of VAR to be VARIANT.

(define (change-support-variant var variant)
  (let ((name (variable-name var))
        (env  (variable-support-env var)))
    (cond ((env name)
           => (lambda (support)
                (set (support.variant support) variant)))
          (else
           (bug "changing support variant of ~S but it has no support" name)))
    name))

;;; (EARLY-BIND free-variables support)
;;;============================================================================
;;;   Find which of the FREE-VARIABLES are defined in the current locale and
;;; which have support in the support environment SUPPORT.  The defined
;;; variables are put in a new support table.

(define (early-bind free-variables support)
  (let ((new-support (make-empty-support-table nil)))
    (iterate loop ((to-do free-variables) (free '()) (defined '()))
      (cond ((null? to-do)
             (values (reverse! free) (reverse! defined) new-support))
            (else
             (receive (flag var)
                      (early-bind-var (car to-do) new-support support)
               (xcase flag
                 ((defined)
                  (loop (cdr to-do) free (cons (car to-do) defined)))
                 ((early-bound)
                  (loop (cdr to-do) (cons var free) defined))
                 ((free)
                  (loop (cdr to-do) (cons (car to-do) free) defined)))))))))

;;; (EARLY-BIND-VAR var new-support support-env)
;;;============================================================================
;;;    Calls FIND-DEFINITIONS to find all of the defining references to VAR.
;;; If the variable is defined it is added to the table NEW-SUPPORT.  If it
;;; is not defined but has an entry in SUPPORT-ENV the variable is replaced
;;; by its namesake from SUPPORT-ENV.

(define (early-bind-var var new-support support-env)
  (let* ((defs (find-definitions var))
         (name (variable-name var))
         (support (support-env name)))
    (if (and defs support)
        (warning "shadowing ~S" name))
    (cond ((< 1 (length defs))
           (fix-multiple-definitions var)
           (values 'defined nil))
          (defs
           (*define-support var new-support (caar defs) (cdar defs))
           (values 'defined nil))
          (support
           (replace-with-bound-var var (support.variable support))
           (values 'early-bound (support.variable support)))
          (else
           (values 'free nil)))))

;;; (FIND-DEFINITIONS var)
;;;============================================================================
;;;   Go through the list of VARs references looking for those that define
;;; support for it.  Return a list of pairs (<type of support> . <reference>).

(define (find-definitions var)
  (iterate loop ((to-do (variable-refs var)) (defs '()))
    (let ((ref (car to-do)))
      (cond ((null? to-do)
             defs)
            ((support-from-ref ref)
             => (lambda (support.variant)
                  (loop (cdr to-do) `((,support.variant . ,ref) . ,defs))))
            (else
             (loop (cdr to-do) defs))))))

;;; (*DEFINE-SUPPORT var env variant)
;;;============================================================================
;;;   Make a support entry for VAR in ENV.

(define (*define-support var env variant ref)
  (let ((name (variable-name var)))
    (cond ((env name)
           (bug "support for ~S mutiply defined" name))
          (else
           (let ((new-support (make-support-entry var t '() variant false)))
             (noise "Defining '~S' support for ~S~%"
                    (identification variant) name)
             (set (env name) new-support)
             (set (variable-support-env var) env)
             (if (permanent-support-variant? variant)
                 (add-support-value var
                                    variant
                                    (get-support-from-ref variant ref))))))))

;;; (REPLACE-WITH-BOUND-VAR var bound-var)
;;;============================================================================
;;;   VAR has been early bound to BOUND-VAR so replace all its references
;;; with references to BOUND-VAR.

(define (replace-with-bound-var var bound-var)
  (noise "Early binding ~S~%"
         (variable-name var))
  (walk (lambda (ref)
          (set (reference-variable ref)
          bound-var))
        (variable-refs var))
  (modify (variable-refs bound-var)
          (lambda (r)
            (append! (variable-refs var) r)))
  (set (variable-refs var) '()))

;;; (SUPPORT-FROM-REF ref)
;;;============================================================================
;;;    REF is a variable-reference node.  This returns a support variant if
;;; REF is the second argument to a primop that defines support.

(define (support-from-ref ref)
  (let ((proc (call-proc (node-parent ref))))
    (if (and (eq? (call-arg 2) (node-role ref))
             (primop-node? proc)
             (primop.defines-support? (primop-value proc)))
        (primop.support-variant (primop-value proc))
        nil)))

;;; (SUPPORTS-DEFINITION? ref)
;;;============================================================================
;;;  A predicate that determines if REF is the support value of some variable.

(define (supports-definition? ref)
  (let ((proc (call-proc (node-parent ref))))
    (and (eq? (call-arg 3) (node-role ref))
         (primop-node? proc)
         (primop.defines-support? (primop-value proc)))))

;;; (SUPPORT-IS-PRIMOP? value)
;;;============================================================================
;;;  T if VALUE will be bound to a primop at runtime.  The first clause checks
;;; to see if VALUE is the lambda variable of a continuation to *PRIMOP, the
;;; second checks to see if VALUE is lambda node that evaluates to a
;;; parameterized primop.  The second clause does inadequate testing.  This
;;; could be done much more accurately by some form of type analysis.

(define (support-is-primop? value)
  (cond ((and (reference-node? value)
              (variable-binder (reference-variable value)))
         (let* ((binder (variable-binder (reference-variable value)))
                (call (node-parent binder)))
           (if (and (primop-ref? (call-proc call) primop/*primop)
                    (eq? (call-arg 1) (node-role binder))
                    (null? (cddr (call-args call))))
               (literal-value ((call-arg 2) call))
               nil)))
        ((and (lambda-node? value)
              (primop-ref? (call-proc (lambda-body value)) primop/*primop))
         (literal-value ((call-arg 2) (lambda-body value))))
        (else
         nil)))

;;; (GET-SUPPORT-DEFINITION var variant)
;;;============================================================================
;;;  This returns the support value for VAR by finding the reference that
;;; defines the variable and then calling GET-SUPPORT-FROM-REF.  The defining
;;; reference is cached at the front of VAR's reference list in the hope that
;;; it will be there next time.

(define (get-support-definition var variant)
  (cond ((not (permanent-support-variant? variant))
         nil)
        ((eq? variant (support-from-ref (car (variable-refs var))))
         (get-support-from-ref variant (car (variable-refs var))))
        (else
         (iterate loop ((refs (cdr (variable-refs var))))
           (let ((ref (car refs)))
             (cond ((null? refs)
                    nil)
                   ((neq? variant (support-from-ref ref))
                    (loop (cdr refs)))
                   (else
                    (set (variable-refs var)
                         (cons ref (delq! ref (variable-refs var))))
                    (get-support-from-ref variant ref))))))))

;;; (GET-SUPPORT-FROM-REF variant ref)
;;;============================================================================
;;;   Get the value of the third argument of the call to which REF is the
;;; second.   (DEFINE <cont> <var> <value>)

(define (get-support-from-ref variant ref)
  (let ((value ((call-arg 3) (node-parent ref))))
    (cond ((support-is-primop? value)
           => (lambda (primop)
                (create-primop-node primop)))
          ((and (eq? variant support/constant)
                (not (suitable-constant-support? value)))
           nil)
          (else
           value))))

;;; (SUITABLE-CONSTANT-SUPPORT? node)
;;;============================================================================
;;;   Is node a constant?

(define (suitable-constant-support? node)
  (and (leaf-node? node)
       (not (and (reference-node? node)
                 (variable-binder (reference-variable node))))))


;;; (FIX-MULTIPLE-DEFINITIONS var)
;;;============================================================================
;;;  Replace all more permanent definitions of VAR with DEFINE.

(define (fix-multiple-definitions var)
  (warning "~S is multiply defined" (variable-name var))
  (walk (lambda (ref)
          (let ((variant (support-from-ref ref)))
            (if (permanent-support-variant? variant)
                (set (primop-value (call-proc (node-parent ref)))
                     primop/*define))))
        (variable-refs var)))
