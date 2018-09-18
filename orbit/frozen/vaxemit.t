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


(define (generate-move node ref1 ref2)
  (if (neq? ref1 ref2)
      (if (and (pair? ref1) (null? (cdr ref1)))
          (emit vax/moval (car ref1) ref2)
          (emit vax/movl ref1 ref2))))

(define (generate-push node access)
  (set *stack-pos* (fx+ *stack-pos* CELL))
  (if (and (pair? access) (null? (cdr access)))
      (emit vax/pushal (car access))
      (emit vax/pushl access )))

(define (generate-pop node access)
  (emit vax/movl (@r+ SP 0) access))
                                     
(define (generate-move-address node from to)                           
  (emit vax/moval from to))
  
(define (generate-jump-to-subroutine node)
  (emit vax/jsb (*d@r AN-1 -2)))
                   
(define (generate-jump node label)
  (emit vax/jmp label))

(define (emit-jump inst else then)
  (emit (concatenate-symbol 'vax/ inst) (label else)))


(define (generate-return node n-args)
  (if (fxn= n-args 1)
      (emit vax/movl (machine-num (fx- -1 n-args)) (r NARGS)))
  (emit vax/movl (reg-offset SP 0) (r TP))
  (emit vax/jmp (reg-offset TP 0)))

(define (generate-general-call node n-args)
  (emit vax/movl  (machine-num (fx+ n-args 1)) (r NARGS))
  (emit vax/movl (reg-offset P -2) (r TP))
  (emit vax/jmp (reg-offset TP 0)))
    
(define (generate-push-address node access)
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
    (generate-move node (r i) (r (fx+ *real-registers* (fx- i A1)))))
  (generate-move node (machine-num required) (r S1))
  (generate-move node (access-value node primop/%make-pair) (r XP))
  (generate-jump-to-subroutine node)
  (mark (lambda-rest-var node) AN))
