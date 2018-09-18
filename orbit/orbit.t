(herald orbit
        (syntax-table *orbit-syntax-table*))

;;; Orbit compiler, part 2.

;;;  Optimization of CPS code tree
;;;  Closure analysis
;;;  Code generation

;;; Post-CPS code has these properties:
;;;   For every LAMBDA node L:
;;;     - L's body is a call.
;;;     - L's parent is a call, or else L is the top of the tree.
;;;   For every call node N:
;;;     - N's procedure and arguments are all non-calls.
;;;     - N's parent is a LAMBDA.

;;; ---------------------------------------------------------------------
;;; Early binding

;;; Returns a possibly shortened list of free variables.

;;; E.g. (early-bind free-vars support)

(define (early-bind vars support)
  (walk (lambda (var)
          (cond ((support-env-entry support (variable-name var))
                 => (lambda (primop)
                      (let ((probe (primop-constant primop)))
                        (let ((val (if (empty? probe) primop probe)))
                          (format t "  - Early binding ~s~%"
                                  (variable-name var))
                          (walk (lambda (ref)
                                  (early-bind-ref ref val))
                                (variable-refs var))))))))
        vars)
  (filter variable-refs vars))

(define (early-bind-ref ref val)
  (replace ref (lambda (ref)
                 (erase ref)
                 (create-literal-node val))))

;;; ---------------------------------------------------------------------
;;; Post-CPS optimizer.

(define-generalist simplify dispatch
  (lambda (node)
    (iterate loop ((node node))
      (cond ((node-simplified? node) node)
            (else
             (walk simplify (node-children node))      ; bottom-up
             (set (node-simplified? node) t)
             (let ((parent (node-parent node))
                   (role (node-role node)))
               ((dispatch node) node)
               (if parent (loop (role parent)) node)))))))

(define-specialist ((simplify literal) node) nil)
(define-specialist ((simplify reference) node) nil)

;;; For LAMBDA, we establish the value of the LAMBDA's self-variable,
;;; and delete any unused parameters.

(define-specialist ((simplify lambda) node)
  (map! (lambda (var)
          (cond ((and (used? var) (null? (variable-refs var))) nil)
                (else var)))
        (cdr (lambda-variables node))))

;;; Calls: deal with LET [i.e. ((lambda ...) ...)] and the various primops.

(define-specialist ((simplify call) node)
  (let ((proc (call-proc node)))
    (select (node-variant proc)
      ((lambda-node?)
       (simplify-let node))
      ((literal-node?)
       (let ((primop (literal-value proc)))
         (cond ((not (primop? primop))
                (format t "literal in procedure position - ~s~%" node)
                (replace proc (create-literal-node undefined-effect-primop)))
               ((primop-integrable primop)
                => (lambda (def) (simplify-integrable-call node def)))
               (else
                (type-apply (primop-type primop) node)
                (cond ((primop-simplifier primop)
                       => (lambda (simp) (simp node)))))))))))

(define (type-apply type node)
  (cond ((and (pair? type)
              (eq? (car type) 'proc))
         (walk (lambda (type arg)
                 (cond ((lambda-node? arg)
                        (cond ((and (pair? type)
                                    (or (eq? (car type) 'cont)
                                        (eq? (car type) 'proc)))
                               (walk alter-variable-type
                                     (cdr (lambda-variables arg))
                                     (cdr type)))))))
               (cdr type)
               (call-args node)))))

(define (alter-variable-type var type)
  (cond ((neq? type 'top?)
         (format t "  - Type of ~s is ~s"
                 (variable-unique-name var)
                 type))))

;;; Integrable procedures

(define (simplify-integrable-call node def)
  (replace (call-proc node)
           (lambda (lit)
             (erase lit)
             (let ((node (alpha-integrable def)))
               (convert node)
               node))))

;;; (values cont a b c)
;;;  ==>  (cont a b c)

(define (simplify-values node)
  (let ((cont (car (call-args node))))
    (replace-call-args node (cdr (copy-list (call-args node))))
    (replace (call-proc node)
             (lambda (proc) (erase proc) cont))))

;;; --- Simplify a LET.

;;; Must substitute for the lambda's variables, if possible.  When
;;; we're done with this, all remaining arg nodes will be lambdas
;;; to which there are at least two calls.

;;; (let ((x 1) (y a) (f (lambda ...)))
;;;   ... x ... y ... f ...)

(define (simplify-let let-node)
  (let ((node (call-proc let-node)))
    (cond ((null? (cdr (lambda-variables node)))
           ;; ((lambda () x)) => x
           (replace let-node
                    (lambda (let-node)
                      (erase let-node)
                      (detach (lambda-body node)))))
          (else
           (walk (lambda (var val)
                   (cond ((or (not (lambda-node? val))
                              (null? (cdr (variable-refs var))))
                          (substitute var val t))
                         (else
                          (set (variable-type var) val))))
                 (cdr (lambda-variables node))
                 (call-args let-node))
           (erase-unused-arguments let-node
                                   (cdr (lambda-variables node)))
           (remove-unreferenced-variables node)))))


(define (simplify-Y node)
  ---)


;;; Returns label procedure for this variable.

(define (variable-known var)
  (let ((type (variable-type var)))
    (cond ((and (node? type)
                (lambda-node? type)
                ;; ((lambda (var) ...) type)  - fix later for labels
                (eq? (node-parent (variable-binder var))
                     (node-parent type)))
           type)
          (else nil))))

;;; If true, then the LAMBDA to which this variable is being bound
;;; can always be jumped to (although an environment adjustment may
;;; be needed).

(define (all-refs-are-calls? var)
  (every? (lambda (ref)
            (eq? (node-role ref) call-proc))
          (variable-refs var)))

;;; Flush ignored arguments from calls to a known procedure.

(define (erase-unused-arguments c vars)
  (iterate loop ((vars vars)
                 (args (call-args c))
                 (new-args '()))
    (cond ((null? vars)
           (relate-shorter-call-args c (reverse! new-args)))
          ((not (used? (car vars)))
           (if (not (empty? (car args)))
               (erase-all (detach (car args))))
           (loop (cdr vars)
                 (cdr args)
                 new-args))
          (else
           (loop (cdr vars)
                 (cdr args)
                 (cons (detach (car args)) new-args))))))

;;; Used in conjunction with above.

(define (remove-unreferenced-variables node)
  (iterate loop ((vars (lambda-variables node)) (n 1))
    (cond ((null? (cdr vars)) 'done)
          ((not (cadr vars))
           (set (cdr vars) (cddr vars))
           (loop vars n))
          (else
           (set (variable-number (cadr vars)) n)
           (loop (cdr vars) (fx+ n 1))))))

;;; ---------------------------------------------------------------------
;;; Perform variable substitution

(define (substitute var val detach?)
  (let ((refs (variable-refs var)))
    (format t "  - Substituting: ~a := ~a~%"
            var
            (pp-cps-2 val))
    (replace (car refs)
             (lambda (ref)
               (erase ref)
               (if detach? (detach val) (copy val '()))))
    (walk (lambda (ref)      ; careful - refs list is delq!'ed by erase
            (replace ref (lambda (ref)
                           (erase ref)
                           (copy val '()))))
          (cdr refs))))

;;; ---------------------------------------------------------------------
;;; Copy node structure.

(define-generalist copy dispatch
  (lambda (node rename)
    ((dispatch node) node rename)))

(define-specialist ((copy literal) node rename)
  (create-literal-node (literal-value node)))

(define-specialist ((copy reference) node rename)
  (let ((var (reference-variable node)))
    (cond ((assq var rename)
           => (lambda (z)
                (create-reference-node (cdr z))))
          (else
           (create-reference-node var)))))

(define-specialist ((copy lambda) node rename)
  (let* ((vars (lambda-variables node))
         (new-vars (map (lambda (var)
                          (if (used? var)
                              (let ((new-var (create-variable
                                               (variable-name var))))
                                (set (variable-rep new-var)
                                     (variable-rep var))
                                new-var)
                              nil))
                        vars)))
    (let ((new-node (create-lambda-node new-vars)))
      (relate lambda-body new-node
              (copy (lambda-body node)
                    (append! (map cons vars new-vars) rename)))
      new-node)))

(define-specialist ((copy call) node rename)
  (let* ((args (call-proc+args node))
         (new-node (create-call-node (length args) (call-exits node))))
    (walk (lambda (arg)
            (relate (node-role arg) new-node (copy arg rename)))
          args)
    new-node))

;;; ---------------------------------------------------------------------
;;; Environment analysis.

;;; Propagate referenced variables list upwards.
;;; Set closure depth slot of every lambda node.

(define (env-analyze-top node)
  (env-analyze node (car (lambda-variables node)) 0 '()))

(define (env-analyze node global cdepth loop-env)
  (cond ((lambda-node? node)
         (let ((cdepth (if (eq? (lambda-strategy node) 'strategy/label)
                           cdepth
                           (fx+ cdepth 1))))
           (set (lambda-cdepth node) cdepth)
           (do ((l (call-proc+args (lambda-body node)) (cdr l))
                (env '() (env-analyze (car l) global cdepth env)))
               ((null? l)
                (let ((env (setdiff env (lambda-variables node))))
                  (set (lambda-env node) env)
                  (format t "  - Environment ~s = ~s~%"
                          (lambda-name node)
                          (map variable-unique-name env))
                  ;; Try to make 2nd arg to UNION be the shorter one.
                  (union loop-env env))))))
        ((reference-node? node)
         (adjoin (if (variable-binder (reference-variable node))
                     (reference-variable node)
                     global)
                 loop-env))
        (else loop-env)))

;;; ---------------------------------------------------------------------
;;; Closure analysis.

;;; Prune lambda-env slot of LAMBDA nodes by replacing already-closed-over
;;; variables by references to the environment.

(define (close-analyze-top node free-vars)
  (close-analyze node '() nil (car (lambda-variables node)))
  (set (lambda-env node) free-vars))

(define (close-analyze node cenv via global)
  (cond ((lambda-node? node)
         (xcase (lambda-strategy node)
           ((strategy/heap strategy/stack)
            (let* ((delta (setdiff (lambda-env node) cenv))
                   (int (intersection (lambda-env node) cenv))
                   (cenv (if (not (empty-set? int))
                             (adjoin via delta)
                             delta)))
              (set (lambda-env node) cenv)
              (format t "  - Consed environment ~s = ~s~%"
                      (lambda-name node)
                      (map variable-unique-name cenv))
              (close-analyze-body (lambda-body node)
                                  (union cenv int)
                                  (car (lambda-variables node))
                                  global)))
           (else
            (close-analyze-body (lambda-body node) cenv via global))))
        ((reference-node? node)
         (set (reference-via node)
              (cond ((null? (variable-binder (reference-variable node)))
                     global)
                    ((memq? (reference-variable node) cenv)
                     via)
                    (else nil))))))

(define (close-analyze-body body cenv via global)
  (walk (lambda (arg) (close-analyze arg cenv via global))
        (call-proc+args body)))
                                                   
(define (empty-set? s)
    (null? s))


;;; ---------------------------------------------------------------------
;;; Representation analysis.

(define (lambda-strategy node)
  (let ((p (node-parent node)))
    (cond ((null? p) 'strategy/top)
          ((fx<= (call-arg-number (node-role node))
                 (call-exits p))
           (if (or (open-coded-primop? (call-proc p))
                   (lambda-node? (call-proc p))
                   (eq? (node-role node) call-proc))
               'strategy/label
               'strategy/stack))
          (else
           'strategy/heap))))

(define (open-coded-primop? node)
  (and (literal-node? node) (primop? (literal-value node))))

;;; ---------------------------------------------------------------------
;;; Register allocation.

;;; This works the same way as RABBIT's DEPTH-ANALYZE.

;;; depth = number of next available register.
;;; P = procedure reg = register 0.
;;; SP = continuation register.
;;; 1 = first argument (or return value) register, 2 is 2nd arg, etc.

(define (allocate-top node free-vars)
  (do ((i 0 (fx+ i 1))
       (vars free-vars (cdr vars)))
      ((null? vars)
       (allocate node 0))
    (set (variable-home (car vars)) i)))

(define (allocate node depth)
  (cond ((lambda-node? node)
         (let* ((vars (lambda-variables node))
                (depth
                 (xcase (lambda-strategy node)
                   ((strategy/heap strategy/top)
                    (set (variable-home (car vars)) 'P)
                    (if (cadr vars)
                        (set (variable-home (cadr vars)) 'SP))
                    (walk (lambda (var)
                            (if (used? var)
                                (set (variable-home var)
                                     (fx- (variable-number var) 1))))
                          (cddr vars))
                    (fx- (length vars) 1))
                   ((strategy/stack)
                    (set (variable-home (car vars)) 'P)
                    (walk (lambda (var)
                            (if (used? var)
                                (set (variable-home var)
                                     (variable-number var))))
                          (cdr vars))
                    (length vars))
                   ((strategy/label)
                    (allocate-label node depth)))))
           (walk (lambda (arg) (allocate arg depth))
                 (call-proc+args (lambda-body node)))))))

(define (allocate-label node depth)
  (iterate loop ((vars (cdr (lambda-variables node)))
                 (d depth))
    (cond ((null? vars) d)
          ((and (used? (car vars))
                (not (variable-known (car vars))))
           (set (variable-home (car vars)) d)
          (loop (cdr vars) (fx+ d 1)))
          (else
           (loop (cdr vars) d)))))

;;; ---------------------------------------------------------------------
;;; Generate code.

;;; Returns a list of compilations of procedures.

(define (generate node output)
  (do ((args (reverse (call-proc+args (lambda-body node))) (cdr args))
       (output output (cond ((lambda-node? (car args))
                             (generate (car args) output))
                            (else output))))
      ((null? args)
       (generate-body node output))))

;;; Returns a pair (label . code) where code is a list of instructions.

(define (generate-body node output)
  (cons (cons (lambda-name node)
              (let ((code (generate-call (lambda-body node))))
                (xcase (lambda-strategy node)
                  ((strategy/label) code)
                  ((strategy/heap strategy/top)
                   (generate-header node 'entry-point  'P  code))
                  ((strategy/stack)
                   (generate-header node 'return-point 'SP code)))))
        output))

(define (generate-header node opcode reg code)
  (ignore node reg)     ; vestigial
  `((,opcode) ,@code))

(define (generate-call body)
  (let ((proc (call-proc body)))
    (cond ((literal-node? proc)
           ((or (primop-generate (literal-value proc))
                generate-primop-call)
            body))
          ((lambda-node? proc)
           (generate-label-call body))
          ((variable-known (reference-variable proc))
           => (lambda (proc)
                (generate-label-call body)))
          ((fx= (call-exits body) 0)
           (generate-return body))
          ((fx= (call-exits body) 1)
           (generate-general-call body))
          (else (bug "too many exits - ~s" body)))))

;;; Call a known procedure (label).

(define (generate-label-call node)
  (let* ((proc (call-proc node))
         (label (xselect (node-variant proc)
                  ((reference-node?)
                   (variable-known (reference-variable proc)))
                  ((lambda-node?)
                   proc))))
    (iterate loop ((args (call-args node))
                   (vars (cdr (lambda-variables label)))
                   (code '()))
      (cond ((null? args)
             (reverse! (cons `(jump ,(lambda-name label)) code)))
            (else
             (loop (cdr args)
                   (cdr vars)
                   (cond ((and (used? (car vars))
                               (neq? (variable-rep (car vars)) 'rep/label))
                          (append! (arg-chunk (car args)
                                              (variable-home (car vars)))
                                   code))
                         (else code))))))))

(define (generate-Y-values node)
  `((jump ,(lambda-name (car (call-args node))))))

;;; This is not quite right, because the assignments really ought
;;; to happen in parallel; we may have problems.

(define (generate-general-call node)
  (append! (apply append!
                  (map (lambda (arg)
                         (arg-chunk arg
                                    (fx- (call-arg-number (node-role arg)) 1)))
                       (cdr (call-args node))))
           (arg-chunk (call-proc node) 'P)
           (arg-chunk (car (call-args node)) 'SP)
           (list '(call))))

(define (generate-return node)
  (append! (apply append!
                  (map (lambda (arg)
                         (arg-chunk arg (call-arg-number (node-role arg))))
                       (call-args node)))
           (arg-chunk (call-proc node) 'SP)
           (list '(return))))

;;; Primops

;;; Totally ad-hoc.

(define (generate-primop-call node)
  (receive (code inputs)
           (generate-args (cdr (call-args node)))
    (append! code
     (let ((op (identification (literal-value (call-proc node))))
           (cont (car (call-args node))))
       (xselect (node-variant cont)
         ((literal-node?)
          ;; No continuation, e.g. as in IF
          `((,op ,@inputs)))
         ((lambda-node?)
          (xcase (lambda-strategy cont)
            ((strategy/label)
             (generate-primop-jump cont op inputs))
            ((strategy/stack)
             (generate-primop-return cont op inputs))))
         ((reference-node?)
          (let ((var (reference-variable cont)))
            (cond ((variable-known var)
                   (generate-primop-jump (variable-known var) op inputs))
                  (else
                   (generate-primop-return cont op inputs))))))))))

(define (generate-primop-return proc op inputs)
  `((,op ,@inputs 1)      ;Fix later
    ,@(arg-chunk proc 'SP)
    (return)))

(define (generate-primop-jump proc op inputs)
  `((,op ,@inputs
         ,@(map variable-place (cdr (lambda-variables proc))))
    (jump ,(lambda-name proc))))

;;; Returns two values, a code chunk and a list of places.

(define (generate-args args)
  (iterate loop ((args args) (code '()) (places '()))
    (cond ((null? args)
           (values code (reverse! places)))
          (else
           (receive (new-code place)
                    (arg-place (car args))
             (loop (cdr args)
                   (append! code new-code)
                   (cons place places)))))))

;;; Returns a code chunk.

(define (arg-chunk arg place)
  (cond ((not place) '())
        (else
         (receive (code source)
                  (arg-place arg)
           (append! code (generate-load place source))))))

;;; Returns a code chunk.

(define (generate-load dst src)
  (if (or (eq? src '$) (eq? src dst)) '() `((load ,dst ,src))))

;;; Returns a code chunk and a place.

(define (arg-place arg)
  (xselect (node-variant arg)
    ((literal-node?)
     (let ((lit (literal-value arg)))
       (values '()
               (cond ((eq? lit *undefined-value*) '$)
                     (else `',lit)))))
    ((reference-node?)
     (let ((var (reference-variable arg)))
       (cond ((null? (variable-binder var))
              ;; Reference to top-level var
              (values '() `(r T ,(variable-home var))))
             ((reference-via arg)
              => (lambda (via)
                   (remote-reference var via)))
             ((eq? (variable-rep var) 'rep/label)
              (values '() '??))
             (else
              (values '() (variable-place var))))))
    ((lambda-node?)
     (let ((s (lambda-strategy arg)))
       (xcase s
         ((strategy/heap strategy/stack)
          (let ((dst (if (eq? s 'strategy/heap) 'P 'SP)))
            (values `((make-closure ,(lambda-name arg)
                                    ,@(map variable-place (lambda-env arg))
                                    ,dst))
                    dst)))
         ((strategy/label)
          (values '() `(label ,(lambda-name arg)))))))))

;;; Returns a place.

(define (variable-place var)
  (variable-home var))

;;; Returns a code chunk and a place.

(define (remote-reference var via)
  (iterate loop ((place (variable-place via)) (via via) (moves '()))
    (let ((b (variable-binder via)))
      (cond ((null? b)
             (bug "env lookup lost - ~s" var))
            ((neq? (variable-number via) 0)
             (bug "screwy reference - ~s via ~s" var via))
            ((posq var (lambda-env b))
             => (lambda (z)
                  (values (reverse! moves) `(r ,place ,z))))
            (else
             (loop 'T
                   (car (lambda-env b))
                   (cons (generate-load 'T `(r ,place 0)) moves)))))))

(define (register? place) (symbol? place))

;;; ---------------------------------------------------------------------
;;; Print object code.

(define (pp-code code)
  (walk (lambda (chunk)
          (format t "  ~s :" (car chunk))
          (walk (lambda (z)
                  (format t "~10t~s~%" z))
                (cdr chunk)))
        code))

;;; ---------------------------------------------------------------------
;;; Phase coordination.

(define (test exp)
  (bind ((*variable-id* 0))
    (receive (top-node free-vars)
             (pass-1 (or (and (procedure? exp) (disclose exp)) exp)
                     *standard-syntax-table*)
      (pass-2 top-node free-vars *standard-support-env*))))

(define (pass-2 top-node free-vars support)
  (format t "Simplifying.~%") 
  (early-bind free-vars support)
  (simplify top-node)

  (format t "~%Analyzing.~%")
  (env-analyze-top top-node)
  (close-analyze-top top-node free-vars)

  (format t "~%Final version (~s):~%" (object-hash top-node))
  (pp-cps top-node))

;  (allocate-top top-node free-vars)
;  (let ((output (generate top-node '())))
;    (format t "~%Output (~s):~%" (object-hash output))
;    (pp-code output)))
