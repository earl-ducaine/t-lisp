(herald (t3 comex t 5))

;;; Compiled expressions

(define-structure-type comex
  code                  ;Code vector (probably a byte vector)
  closure-size          ;Size of closure to be created
  objects               ;General vector of interesting objects
  opcodes               ;Byte vector of things to do with the objects
  annotation)

;;; The OBJECTS and OPCODES vectors are in 1-1 correspondence.  Each opcode
;;; describes what should be done with the corresponding object.  The
;;; result of processing an object/opcode pair gets stored at the
;;; corresponding position when the closure ("unit") is being created.

;;; The OBJECTS/OPCODES vectors must be of the same length, but they
;;; might actually be longer than the closure to be created.  The extra
;;; slots at the end then correspond to extra operations or consistency
;;; checks to be performed.

(define-local-syntax (define-opcodes . opcodes)
  (do ((i 1 (fx+ i 1))
       (l opcodes (cdr l))
       (z '() (cons `(define-constant ,(concatenate-symbol 'op/ (car l))
                       ,i)
                    z)))
      ((null? l)
       `(block ,@(reverse! z)
               (define *number-of-opcodes* ,i)))))

(define-opcodes
  ;; Structural nonsense
  literal               ;Literal s-expression
  foreign               ;Name of a foreign procedure (a string, e.g. "_getc")
  closure               ;Object is a code vector offset - put a cl-int-cl here
  template1             ;Similarly, but put a cl-int-template here
  template2             ; (cl-int-templates take up 3 cells)
  template3             ; (info is copied from the aux. template)
  ;; Variable references
  definition            ;Pair (name . cv-offset) for a top-level define
  stored-definition     ;Pair (name . cl-offset) for a top-level define
  defined               ;Name of a var otherwise defined
  lset                  ;Name of a free variable which is lset
  free                  ;Name of some other free variable
  ;; Other stuff
  special-literal       ;Descriptor for something peculiar, e.g. a primop
  nonlocal              ;Pair (env-name . var-name) for (*value x 'y)
  ;; The following don't correspond to closure slots.
  early                 ;Name of something which was bound early
  )

(define-structure-type annotation
  filename              ;Source file name or other id (?)
  roadmap               ;A-list of (cv-offset . names) for closures (?)
  dead-ends             ;A-list of (cv-offset . error-info) for type tests
  ;...
  )


(define (create-comex unit templates thing code)
  (let ((size (fx+ (fx+ (length unit) 4) (fx* (length templates) 3)))
        (comex (make-comex)))
    (receive (objects opcodes)
             (construct-obj-op-vectors thing unit templates size)
      (set (comex-code comex) code)
      (set (comex-closure-size comex) size)
      (set (comex-objects comex) objects)
      (set (comex-opcodes comex) opcodes)
      comex)))


(define (create-obj-op-vectors thing unit templates size)
  (let ((objects (make-vector size))
        (opcodes (make-vector size)))
    (vset opcodes 0 op/closure)
    (vset objects 0 thing)
    (do ((a-list unit (cdr a-list))
         (i 1 (fx+ i 1)))
        ((null? a-list)
         (do ((templates templates (cdr templates))
              (i i (fx+ i 3)))
             ((null? templates)
              (values objects opcodes))
           (vset objects i (code-vector-offset (car templates)))
           (vset opcodes i op/template1)
           (vset opcodes (fx+ i 1) op/template2)
           (vset opcodes (fx+ i 2) op/template3)))
      (receive (opcode object) (comex-decipher unit (caar a-list))
        (vset objects i object)
        (vset opcodes i opcode)))))


(define (comex-decipher unit object)
  (cond ((primop-node? object)
         (values op/special-literal (identification object)))
        ((literal-node? object)
         (cond ((foreign? object)
                (values op/foreign (foreign-name (literal-value object))))
               (else
                (values op/literal (literal-value object)))))
        ((lambda-node? object)
         (values op/closure (code-vector-offset object)))
        (else
         (let ((var (leaf-value object)))
           (cond ((defined-at-top-level var)  ; returns closure
                  => (lambda (closure)
                       (values op/stored-definition
                               (cons (variable-name var)
                                     (code-vector-offset closure)))))
                 ((defined? var)
                  (values op/defined (variable-name var)))
                 ((lset? var)
                  (values op/lset (variable-name var)))
                 (else
                  (values op/free (variable-name var))))))))
