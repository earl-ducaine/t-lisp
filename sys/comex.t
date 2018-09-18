(herald (t3 comex t 12))

;;; Compiled expressions

(define-structure-type comex
  code                  ;Code vector (probably a byte vector)
  objects               ;General vector of interesting objects
  opcodes               ;Byte vector of things to do with the objects
  annotation)

;;; The OBJECTS and OPCODES vectors are in 1-1 correspondence.  Each opcode
;;; describes what should be done with the corresponding object.  The
;;; result of processing an object/opcode pair gets stored at the
;;; corresponding position when the closure ("unit") is being created.

;;; (define (run-compiled-code comex env)
;;;   ((map-vector (lambda (op obj)
;;;                  ((vref *op-procedures* op) obj env))
;;;                (comex-objects comex)
;;;                (comex-opcodes comex))))

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
                        ; Like closure - put a cl-int-cl here
  stored-definition     ;Pair (name . cl-offset) for a top-level define
  defined               ;Name of a var otherwise defined
  lset                  ;Name of a free variable which is lset
  free                  ;Name of some other free variable
  ;; Other stuff
  special-literal       ;Descriptor for something peculiar, e.g. a primop
  nonlocal              ;Pair (env-name . var-name) for (*value x 'y)
  )

(define-structure-type annotation
  filename              ;Source file name or other id (?)
  roadmap               ;A-list of (cv-offset . names) for closures (?)
  dead-ends             ;A-list of (cv-offset . error-info) for type tests
  ;...
  )

(define (write-comex-to-file spec comex)
  (with-open-streams ((s (open-dump spec
                                    dump-keys
                                    dump-accessors)))
    (write s comex)))

(define (read-comex-from-file spec)
  (with-open-streams ((s (open-retrieve spec
                                        dump-makers
                                        dump-accessors)))
    (read s)))

(define (dump-keys obj)
  (if (comex? obj) 'comex nil))

(define (dump-accessors sym)
  (xcase sym
         ((comex) (stype-selectors comex-stype))))

(define (dump-makers sym)
  (xcase sym
         ((comex) make-comex)))
