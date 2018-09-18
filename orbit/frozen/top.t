(herald top)

(lset *assembly-output* nil)

(define (comfile file)
  (really-comfile file nil nil t))

(define (support file)
  (really-comfile file nil t nil))


(define (really-comfile file-spec debug? support? code?)
  (let ((filename (->filename file-spec)))
    (with-open-streams ((noise-stream
                         (open (filename-with-type filename 'n) '(out))))
      (bind (((terminal-output)
              (make-broadcast-stream noise-stream (terminal-output)))
             (*debug-flag* debug?)
             (*variable-id* 0))
      (bind ((*noise-stream* (terminal-output))
             (*assembly-output* (terminal-output)))
        (format *noise-stream* "~%~A----- beginning t compilation on ~S~2%"
                #\semicolon         ; uluz  Que?
                (filename->string filename))
        (receive (herald-obj exp)
                 (read-file filename)
          (noise "Getting support~%")
          (let ((support (if (herald-support herald-obj)
                             (eval (herald-support herald-obj) *orbit-env*)
                             *standard-support-env*)))
            (receive (tree free-variables)
                     (expression->code-tree exp *standard-syntax-table*)
              (noise "Early binding~%")
              (receive (free-variables defined-variables new-support)
                       (early-bind free-variables support)
                (noise "Free variables (~S): ~S~%"
                       (object-hash free-variables)
                       (map variable-name free-variables))
                (noise "Defined variables (~S): ~S~%"
                       (object-hash defined-variables)
                       (map variable-name defined-variables))
                (munge-code-tree tree)
                (if code?
                    (emit-comex tree
                                (filter used? free-variables)
                                defined-variables))
                (if support?
                    (emit-support filename new-support))
                t)))))))))

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

(define (expression->code-tree exp syntax)
  (noise "Alphatizing~%")
  (receive (top-node handle)
           (alpha-top exp syntax)
    (noise "Free variables (~S): ~S~%"
           (object-hash (the-free-variables handle))
           (map variable-name (the-free-variables handle)))
    (noise "Assignment analysis~%")
    (assignment-analyze top-node)
    (noise "Complexity analysis~%")
    (complexity-analyze top-node)
    (noise "CPS conversion~%")
    (convert top-node)
    (set (node-role top-node) 'top)
    (set (node-parent top-node) nil)
    (orbit-debug "Top-node (~S): ~S~%" (object-hash top-node) top-node)
    (orbit-debug "CPS tree:~%")
    (if *debug-flag*
        (pp-cps top-node *noise-stream*))
    (values top-node (the-free-variables handle))))

(define (munge-code-tree top-node)
  (noise "Simplifying~%")
  (simplify (lambda-body top-node))
  (noise "Simplification done~%")
  (orbit-debug "Simplified tree:~%")
  (if *debug-flag*
      (pp-cps top-node *noise-stream*)))


(define (emit-comex top-node free-variables defined-variables)
  (pp-cps top-node *noise-stream*)
  (bind ((*debug-flag* t))
    (live-analyze-top top-node)
    (receive (a b c)
	     (close-analyze-top top-node free-variables defined-variables)
      (generate-code-top (car (call-args (lambda-body top-node))))
      (create-comex a b c (assemble)))))

(define (emit-support filename support)
  (noise "Writing support file~%")
  (with-open-streams ((output (open-dump (filename-with-type filename 'sup))))
    (write output
           (support-table->expression (filename-name filename) support))
    t))

(define (orbit-vax-init)
  (orbit-init '(orbit vaxbase)
              '(orbit vaxprimops)
              '(orbit open)
              '(orbit aliases)
              '(orbit operation)))

(lset *known-primops*
      '(*primop
        continuation
        undefined
        undefined-effect
        *lset
        *define
        *define-constant
        *define-integrable
        *define-wired
        receive-values values
        y
        conditional
        test
        true?
        *set-var
        *locative
        make-locative
        contents
        set-contents
        let-reference
        %make-pair
        %make-extend
        handler
        proc+handler))

(define (orbit-init . file-specs)
  (orbit-uninit)
  (bind ((*noise-stream* (terminal-output)))
    (set *standard-support-env*
         (get-support-environment '*standard-support-env* '() file-specs))
    (walk (lambda (s)
            (set (*value *orbit-env* (concatenate-symbol 'primop/ s))
                 (get-primop s *standard-support-env*)))
          *known-primops*)
    *standard-support-env*))

(define (get-primop name support-env)
  (let ((support (support-env name)))
    (if support
        (primop-value (support.value support))
        (bug "no support for ~S" name))))

(define (orbit-uninit)
  (set *support-tables* (make-table '*support-tables*))
  (walk (lambda (s)
          (set (*value *orbit-env* (concatenate-symbol 'primop/ s))
               '#f))
        *known-primops*)
  (set *standard-support-env* (make-empty-support-env 'nil)))


(define (cl exp)
  (bind ((*debug-flag* nil)
         (*variable-id* 0))
    (set *noise-stream* (terminal-output))
    (set *assembly-output* *noise-stream*)
    (receive (tree free-variables)
             (expression->code-tree `(lambda () ,exp) *standard-syntax-table*)
      (receive (free-variables defined-variables new-support)
               (early-bind free-variables *standard-support-env*)
        (noise "Free variables (~S): ~S~%"
               (object-hash free-variables)
               (map variable-name free-variables))
        (munge-code-tree tree)
        (emit-comex tree (filter used? free-variables) defined-variables)))))
