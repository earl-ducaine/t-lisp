(herald vaxemit)

(define-constant CELL 4)
(define-constant S0 0)
(define-constant S1 1)
(define-constant S2 2)
(define-constant S3 3)
(define-constant NARGS 3)
(define-constant P 4)
(define-constant A1 5)
(define-constant A2 6)
(define-constant A3 7)
(define-constant A4 8)
(define-constant AN 9)
(define-constant AN-1 8)
(define-constant TP -1)
(define-constant nil-reg -2)
(define-constant SP -3)
(define-constant TASK -4)

(lset *registers* (vector-fill (make-vector 20) nil))
(lset *pointer-registers* 6)
(lset *scratch-registers* 4)
(lset *argument-registers* 4)
(lset *real-registers* 10)
(lset *pointer-temps* 6)
(lset *no-of-registers* 20)
(lset *assembly-output* nil)


(define (generate-move ref1 ref2)
  (if (neq? ref1 ref2)
      (if (and (pair? ref1) (null? (cdr ref1)))
          (emit vax/moval (car ref1) ref2)
          (emit vax/movl ref1 ref2))))

(define (generate-push access)
  (set *stack-pos* (fx+ *stack-pos* CELL))
  (if (and (pair? access) (null? (cdr access)))
      (emit vax/pushal (car access))
      (emit vax/pushl access )))

(define (generate-pop access)
  (emit vax/movl (@r+ SP 0) access))
                                     
(define (generate-move-address from to)
  (emit vax/moval from to))
  
(define (generate-jump-to-subroutine)
  (emit vax/jsb (*d@r AN-1 -2)))
                   
(define (generate-jump label)
  (emit vax/jmp label))

(define (emit-jump inst else then)
  (emit (concatenate-symbol 'vax/ inst) (label else)))


(define (generate-return n-args)
  (if (fxn= n-args 1)
      (emit vax/movl (machine-num (fx- -1 n-args)) (r NARGS)))
  (emit vax/movl (reg-offset SP 0) (r TP))
  (emit vax/jmp (reg-offset TP 0)))

(define (generate-general-call n-args)
  (emit vax/movl  (machine-num (fx+ n-args 1)) (r NARGS))
  (emit vax/movl (reg-offset P -2) (r TP))
  (emit vax/jmp (reg-offset TP 0)))
    
(define (generate-push-address access)
  (set *stack-pos* (fx+ *stack-pos* CELL))
  (emit vax/pushal access))

      
(define (emit op . args)
  (format *assembly-output* "~12t~a~20t"
          (if (fixnum? op)
              (vref *vax-instructions* op)
              (nthchdr (string-downcase! (symbol->string op)) 4)))
  (cond (args
         (format *assembly-output* "~a" (assembly-syntax (car args)))
         (walk (lambda (arg)
                 (format *assembly-output* ",~a" (assembly-syntax arg)))
               (cdr args))))
  (newline *assembly-output*))


(define (emit-tag l)
  (format *assembly-output* "~a:~%"
    (if (lambda-node? l)
        (tag l)
        (string-downcase (symbol->string l)))))

(define (emit-template l)
  (format *assembly-output* "t_~a:~%" (tag l)))

(define r identity)

(define (reg-offset reg offset)
  (cons reg offset))

(define (*d@r foo goo) "*-2(a4)")

(define (lit x)
  (cons 'lit x))

(define (machine-num x)
  (cons 'machine-num x))

(define (@-r a v)
  "-(sp)")

(define (@r+ aq v)
  "(sp)+")

(define (index pair reg)
  (cons (cons (car pair) reg) (cdr pair)))

(define (label l)
  (tag l))

(define (template l)
  (string-append "t_" (tag l)))


(define temp-access r)

(define (assembly-syntax operand)
  (cond ((fixnum? operand)
         (register->symbol operand)) 
        ((string? operand) operand)
        ((eq? (car operand) 'lit)
         (if (char? (cdr operand))
             (format nil "$~a"
                (logior (ash (char->ascii (cdr operand)) 8) %char))
             (format nil "$~a<<2" (cdr operand))))
        ((eq? (car operand) 'machine-num) 
         (format nil "$~a" (cdr operand)))
        ((fixnum? (car operand))
         (if (fx= (cdr operand) 0)
             (format nil "(~a)" (register->symbol (car operand)))
             (format nil "~d(~a)" (cdr operand)
                                  (register->symbol (car operand)))))
        (else
         (if (fx= (cdr operand) 0)
             (format nil "(~a)[~a]" (register->symbol (caar operand))
               (register->symbol (cdar operand)))
             (format nil "~d(~a)[~a]" (cdr operand) (register->symbol (caar operand))
               (register->symbol (cdar operand)))))))
                                                        

(define (register->symbol r)
  (cond ((fx< r 0)
         (vref *reserved-registers* (fx- 0 r)))
        ((fx< r *real-registers*)
         (vref *register-symbol-table* r))
        (else
         (concatenate-symbol 't (fx- r *real-registers*)))))

;(define *register-symbol-table*
        ;'#("r0" "r1" "r2" "r3" "r4" "r5" "r6" "r7"
           ;"r8" "r9" "r10" "r11" "12" "huh?" "sp" nil))

(define *register-symbol-table*
        '#("s0" "s1" "s2" "s3" "p" "a1" "a2" "a3"
           "a4" "an"))

(define *reserved-registers*
        '#(nil "t" "nil" "sp" "task"))

(define (mn-assembly-syntax reg)
  (string->symbol (register->symbol reg)))

(define (printable-closure-env closure)
  (map (lambda (pair)
         (variable-unique-name (car pair)))
       (closure-env closure)))

(define (emit-comment . args)
  (apply format *assembly-output* args)
  (newline *assembly-output*))

(define (tag l)
  (string-downcase! (symbol->string (lambda-name l))))

(define (access-name name)
  (string-downcase! (symbol->string name)))
    
(define (generate-nary-setup node required)
  (do ((i (fx+ A1 required) (fx+ i 1)))
      ((fx>= i *real-registers*))
    (generate-move (r i) (r (fx+ *real-registers* (fx- i A1)))))
  (generate-move (machine-num required) (r S1))
  (generate-move (access-value node primop/%make-pair) (r XP))
  (generate-jump-to-subroutine)
  (mark (lambda-rest-var node) AN))



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
    (vset opcodes 0 op/closure)
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
           (vset opcodes i op/template1)
           (vset opcodes (fx+ i 1) op/template2)
           (vset opcodes (fx+ i 2) op/template3)))
      (receive (opcode obj) (comex-decipher unit (caar a-list))
        (vset objects i obj)
        (vset opcodes i opcode)))))


(define (comex-decipher unit obj)
  (cond ((primop? obj)
         (cond ((primop.foreign obj)
                => (lambda (name)
                     (values op/foreign name)))
               (else
                (values op/special-literal (primop.external-name obj)))))
        ((lambda-node? obj)
         (values op/closure (code-vector-offset obj)))
        ((not (variable? obj))
         (values op/literal obj))
        (else
         (cond ((not (supported? obj))
                (values op/free (variable-name obj)))
               ((lset? obj)
                (values op/lset (variable-name obj)))
               ((closure-defined-at-top-level obj)  ; returns closure
                => (lambda (closure)
                     (values op/stored-definition
                             (cons (variable-name obj)
                                   (cdr (assq closure unit))))))
               (else
                (values op/defined (variable-name obj)))))))


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


(define (cit->lambda closure)
  (variable-binder (car (closure-members closure))))
