(herald top)

(define (support file-spec)
  (let ((filename (->filename file-spec)))
    (with-open-streams ((noise-stream
                         (open (filename-with-type filename 'n) '(out))))
      (bind (((terminal-output)
              (make-broadcast-stream noise-stream (terminal-output))))
        (set *noise-stream* (terminal-output))
        (receive (herald-obj exp)
                 (read-file filename)
            (bind ((*variable-id* 0))
              (let ((support (if (herald-support herald-obj)
                                 (eval (herald-support herald-obj) *orbit-env*)
                                 *standard-support-env*)))
                (receive (tree free-variables defined-variables new-support)
                         (expression->code-tree exp
                                                *standard-syntax-table*
                                                support)
                  (emit-code filename
                             (munge-code-tree tree
                                              free-variables
                                              defined-variables))
                  (emit-support filename new-support)
                  t))))))))

(define (read-file filename)
  (with-open-streams ((input (open (filename-with-type filename 't) '(in))))
    (let* ((first (read input))
           (herald-obj (cond ((or (not (pair? first))
                                  (neq? (car first) 'herald))
                              (error "file ~S has no herald form"
                                     (filename->string filename)))
                             (else
                              (parse-herald (cadr first) (cddr first))))))
      (iterate loop ((forms '()) (read-form (read input)))
        (cond ((eof? read-form)
               (values herald-obj
                       `(lambda () . ,(reverse! forms))))
              (else
               (loop (cons read-form forms) (read input))))))))

(define (expression->code-tree exp syntax support)
  (receive (top-node handle)
           (alpha-top exp syntax)
    (noise "Free variables (~S): ~S~%"
           (object-hash (the-free-variables handle))
           (map variable-name (the-free-variables handle)))
    (assignment-analyze top-node)
    (complexity-analyze top-node)
    (convert top-node)
    (set (node-role top-node) 'top)
    (set (node-parent top-node) nil)
    (orbit-debug "Top-node (~S): ~S~%" (object-hash top-node) top-node)
    (orbit-debug "CPS tree:~%")
    (if *debug-flag*
        (pp-cps top-node *noise-stream*))
    (receive (free-variables defined-variables new-support)
             (early-bind handle support)
      (noise "Free variables (~S): ~S~%"
             (object-hash free-variables)
             (map variable-name free-variables))
      (noise "Defined variables (~S): ~S~%"
             (object-hash defined-variables)
             (map variable-name defined-variables))
      (values top-node free-variables defined-variables new-support))))

(define (munge-code-tree top-node free-variables defined-variables)
  (simplify top-node)
  (orbit-debug "Simplified tree:~%")
  (if *debug-flag*
      (pp-cps top-node *noise-stream*))
  top-node)

(define (emit-code filename tree)
    t)

(define (comfile file-spec)
  (let ((filename (->filename file-spec)))
    (with-open-streams ((noise-stream
                         (open (filename-with-type filename 'n) '(out))))
      (bind (((terminal-output)
              (make-broadcast-stream noise-stream (terminal-output))))
        (set *noise-stream* (terminal-output))
        (set *assembly-output* *noise-stream*)
        (receive (herald-obj exp)
                 (read-file filename)
            (bind ((*variable-id* 0)
                   (*debug-flag* nil))
              (let ((support (if (herald-support herald-obj)
                                 (eval (herald-support herald-obj) *orbit-env*)
                                 *standard-support-env*)))
                (receive (tree free-variables defined-variables new-support)
                         (expression->code-tree exp
                                                *standard-syntax-table*
                                                support)
                 (munge-code-tree1 tree free-variables defined-variables)
                  t))))))))


(define (munge-code-tree1 top-node free-variables defined-variables)
  (simplify top-node)
  (orbit-debug "Simplified tree:~%")
 ; (if *debug-flag*
      (pp-cps top-node *noise-stream*)
  (bind ((*debug-flag* t))
    (live-analyze-top top-node)
    (close-analyze-top top-node free-variables defined-variables)
    (set top-node (car (call-args (lambda-body top-node))))
    (pp-code-header *assembly-output* defined-variables nil)
    (generate-code-top top-node)
    (new-line *assembly-output*)))




(define (emit-support filename support)
  (with-open-streams ((output (open-dump (filename-with-type filename 'sup))))
    (write output
           (support-env->expression (filename-name filename) support))
    t))

(define (orbit-vax-init)
  (orbit-init '(orbit vaxbase)
              '(orbit vaxprimops)
              '(orbit open)
              '(orbit aliases)))

(define (orbit-init . file-specs)
  (let ((s (get-support-environments (make-empty-support-env 'initial)
                                           file-specs)))
    (lset primop/*primop            (get-primop '*primop            s))
    (lset primop/continuation       (get-primop 'continuation       s))
    (lset primop/undefined          (get-primop 'undefined          s))
    (lset primop/undefined-effect   (get-primop 'undefined-effect   s))
    (lset primop/n-ary              (get-primop 'n-ary              s))
    (lset primop/block              (get-primop 'block              s))
    (lset primop/*define            (get-primop '*define            s))
    (lset primop/*lset              (get-primop '*lset              s))
    (lset primop/*define-constant   (get-primop '*define-constant   s))
    (lset primop/*define-integrable (get-primop '*define-integrable s))
    (lset primop/*define-wired      (get-primop '*define-wired      s))
    (lset primop/y                  (get-primop 'y                  s))
    (lset primop/values             (get-primop 'values             s))
    (lset primop/test               (get-primop 'test               s))
    (lset primop/conditional        (get-primop 'conditional        s))
    (lset primop/true?              (get-primop 'true?              s))
    (lset primop/*set-var           (get-primop '*set-var           s))
    (lset primop/*locative          (get-primop '*locative          s))
    (lset primop/make-locative      (get-primop 'make-locative      s))
    (lset primop/contents           (get-primop 'contents           s))
    (lset primop/set-contents       (get-primop 'set-contents       s))
    (lset primop/let-reference      (get-primop 'let-reference      s))
    (lset primop/%make-extend       (get-primop '%make-extend       s))
    (lset primop/%make-pair         (get-primop '%make-pair         s))
    (set *standard-support-env* s)))

(define (get-primop name support-env)
  (let ((support (support-env name)))
    (if support
        (primop-value (support.value support))
        (bug "no support for ~S" name))))

(define (orbit-uninit)
  (lset primop/*primop            '#f)
  (lset primop/continuation       '#f)
  (lset primop/undefined          '#f)
  (lset primop/undefined-effect   '#f)
  (lset primop/n-ary              '#f)
  (lset primop/block              '#f)
  (lset primop/*define            '#f)
  (lset primop/*lset              '#f)
  (lset primop/*define-constant   '#f)
  (lset primop/*define-integrable '#f)
  (lset primop/*define-wired      '#f)
  (lset primop/y                  '#f)
  (lset primop/values             '#f)
  (lset primop/test               '#f)
  (lset primop/conditional        '#f)
  (lset primop/true?              '#f)
  (lset primop/*set-var           '#f)
  (set *standard-support-env* (make-empty-support-env 'nil)))
