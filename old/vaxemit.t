(herald vaxemit)

(lset *registers* (vector-fill (make-vector 20) nil))
(lset *argument-registers* 4)
(lset *scratch-registers* 4)
(lset *real-registers* 10)
(lset *no-of-registers* 20)
(lset *assembly-output* nil)


(define (generate-move node ref1 ref2)
  (if (neq? ref1 ref2)
      (if (and (pair? ref1) (null? (cdr ref1)))
          (icreate node "moval" (car ref1) ref2)
          (icreate node "movl" ref1 ref2))))

(define (generate-push node access)
  (set *stack-pos* (fx+ *stack-pos* 4))
  (if (and (pair? access) (null? (cdr access)))
      (icreate node "pushal" (car access))
      (icreate node "pushl" access )))

(define (generate-pop node access)
  (icreate node "movl" (@r+ SP 0) access))
                                     
(define (generate-move-address node from to)                           
  (if (string? from)
      (icreate node "moval" (string-append "t_" from "+2") to)
      (icreate node "moval" from to)))
  
(define (generate-jump-to-subroutine node)
  (icreate node "jsb" (*d@r XP -2)))
                   
(define (generate-jump node label)
  (icreate node "jmp" label))

(define (emit-jump node inst else then)
  (icreate node inst (label else)))


(define (generate-return node n-args)
  (if (fxn= n-args 1)
      (icreate node "movl" (machine-num (fx- -1 n-args)) (r NARGS)))
  (icreate node "movl" (d@r SP 0) (r TP))
  (icreate node "jmp" (d@r TP 0)))

(define (generate-general-call node n-args)
  (icreate node "movl"  (machine-num (fx+ n-args 1)) (r NARGS))
  (icreate node "movl" (d@r P -2) (r TP))
  (icreate node "jmp" (d@r TP 0)))
    
(define (generate-push-address node access)
  (set *stack-pos* (fx+ *stack-pos* 4))
  (icreate node "pushal" (string-append "t_" access "+2")))

      
(define (create-tag-list i)
  (do ((i i (fx- i 1))
       (l '() (cons (symbolconc 'n *variable-id*) l)))
      ((fx<= i 0) l)
    (increment *variable-id*)))
  

(define (icreate node op . args)
  (format *assembly-output* "~12t~a~20t"
          (if (string? op) op (string-downcase! (symbol->string op))))
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
  (format *assembly-output* "t_~a:~12t.word 0~%" (tag l)))

(define r identity)

(define (d@r reg offset)
  (cons reg offset))

(define (*d@r foo goo) "*-2(r12)")

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
        '#("s1" "s2" "s3" "s4" "p" "a1" "a2" "a3"
           "a4" "a5"))

(define *reserved-registers*
        '#(nil "t" "nil" "xp" "sp"))

(define (mn-assembly-syntax reg)
  (string->symbol (register->symbol reg)))

(define (printable-closure-env closure)
  (map (lambda (pair)
         (variable-unique-name (car pair)))
       (closure-env closure)))

(define (emit-comment . args)
  (apply format *assembly-output* args)
  (newline *assembly-output*))


(define (pp-code-header stream defined-vars lset-vars)
  (let ((emit-global (lambda (var)
                       (format stream "~12t.globl p_~a~%"
                               (string-downcase
                                 (mysymbol->string (variable-name var))))))
        (strings '())
        (symbols '()))
    (walk emit-global defined-vars)
    (walk emit-global lset-vars)
    (format stream "p_~a:" (tag (car *unit-closures*)))
    (format stream "~12t.long t_~a+2~%" (tag (car *unit-closures*)))
    (do ((closures *defined-variables* (cdr closures)))
        ((null? closures))
      (let ((name (string-downcase (mysymbol->string
                          (variable-name (car closures))))))
        (format stream "p_~a:" name)
        (format stream "~12t.long t_~a+2~%" name)))
    (do ((vars *unit-values* (cdr vars)))
        ((null? vars))
      (cond ((memq? (car vars) *lset-variables*)
             (format stream "p_~a:~12t.long 0~%"
                 (string-downcase! (mysymbol->string (variable-name (car vars))))))
            ((string? (car vars)
             (let ((string (car vars)))
               (format stream "~12t.long ___~a+2~%" (char string))
               (push strings string)))
            ((list? (variable-name (car vars)))
             (let ((symbol (cadr (variable-name (car vars)))))
               (format stream "~12t.long ~a+2~%"
                   (string-downcase! (mysymbol->string symbol)))
               (push symbols symbol)))
          (else
           (format stream "~12t.long p_~a+2~%"
             (let ((var (car vars)))
               (string-downcase! (mysymbol->string (if (primop? var)
                                                       (identification var)
                                                       var)))))))))
    (do ((cits *unit-templates* (cdr cits)))
        ((null? cits))
      (let ((name (tag (car cits))))
        (format stream '("~12t.long 0~%t_~a:~12t.word 0xff~%"
                         "~12t.word 0x9f17~%~12t.long at_~a+2")
                   name name)))
    (walk (lambda (string)
            (format stream '("~12t.align 2~%___~a:~12t.long (~d<<8)+~d~%"
                             "~12t.long __~a+2~%~12t.long 0~%"
                             "__~a:~12t.long (~d<<8)+~d~%~12t.asciz ~s")
               (char string) (string-length string) %slice (char string)
               (char string) (string-length string) %text string))
          strings)
    (format stream "~12t.text~%")
    (walk (lambda (symbol)
      (format stream "~12t.align 2~%~a:~12t.long ~d~%~12t.asciz ~s"
            (string-downcase! (mysymbol->string symbol)) %symbol
            (symbol-pname symbol)))
      symbols)))

(define (tag l)
  (string-downcase! (mysymbol->string (lambda-name l))))


(define (mysymbol->string sym)
  (map-string! (lambda (c)
                 (if (or (alphabetic? c)
                         (char= c #\$)
                         (digit? c 10))
                     c
                     #\_))
               (symbol->string (if (primop? sym)
                                   (identification sym)
                                   sym))))
