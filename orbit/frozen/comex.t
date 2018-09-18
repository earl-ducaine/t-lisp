(herald (t3 comex t 5))

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

;;; The OBJECTS/OPCODES vectors must be of the same length, but they
;;; might actually be longer than the closure to be created.  The extra
;;; slots at the end then correspond to extra operations or consistency
;;; checks to be performed.



(define (create-comex unit templates thing code)
  (let ((size (fx+ (fx+ (length unit) 1) (fx* (length templates) 3)))
        (comex (make-comex)))
    (receive (objects opcodes)
             (create-obj-op-vectors thing unit templates size)
      (set (comex-code comex) code)
      (set (comex-objects comex) objects)
      (set (comex-opcodes comex) opcodes)
      comex)))


(define (create-obj-op-vectors thing unit templates size)
  (let ((objects (make-vector size))
        (opcodes (make-vector size)))
    (vset opcodes 0 'op/closure)
    (vset objects 0 (code-vector-offset thing))
    (do ((a-list unit (cdr a-list))
         (i 1 (fx+ i 1)))
        ((null? a-list)
         (do ((templates templates (cdr templates))
              (i i (fx+ i 3)))
             ((null? templates)
              (values objects opcodes))
           (vset objects i
                 (code-vector-offset (cit->lambda (car templates))))
           (vset opcodes i 'op/template1)
           (vset opcodes (fx+ i 1) 'op/template2)
           (vset opcodes (fx+ i 2) 'op/template3)))
      (receive (opcode obj) (comex-decipher unit (caar a-list))
        (vset objects i obj)
        (vset opcodes i opcode)))))


(define (comex-decipher unit obj)
  (cond ((primop? obj)
         (values 'op/special-literal (primop.external-name obj)))
        ((lambda-node? obj)
         (values 'op/closure (code-vector-offset obj)))
        ((not (variable? obj))
         (cond ((foreign? obj)
                (values 'op/foreign (foreign-name obj)))
               (else
                (values 'op/literal obj))))
        (else
         (cond ((not (supported? obj))
                (values 'op/free (variable-name obj)))
               ((lset? obj)
                (values 'op/lset (variable-name obj)))
               ((closure-defined-at-top-level obj)  ; returns closure
                => (lambda (closure)
                     (values 'op/stored-definition
                             (cons (variable-name obj)
                                   (cdr (assq closure unit))))))
               (else
                (values 'op/defined (variable-name obj)))))))


(define (lset? var)
  (eq? (support.variant (variable-support var)) support/lset))


(define (closure-defined-at-top-level var)
  (any (lambda (ref)
         (let ((proc (call-proc (node-parent ref))))
           (and (eq? (call-arg 2) (node-role ref))
                (primop-node? proc)
                (primop.defines-support? (primop-value proc))
                (let ((node ((call-arg 3) (node-parent ref))))
                  (if (lambda-node? node)
                      node
                      nil)))))
       (variable-refs var)))

(define (code-vector-offset l)
  (lambda-name l))


(define (cit->lambda closure)
  (variable-binder (car (closure-members closure))))

(define foreign? false)
