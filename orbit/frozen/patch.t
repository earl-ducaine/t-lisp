(herald patch)

(define (early-bind free-variables support)
  (let ((new-support (make-empty-support-table nil)))
    (iterate loop ((to-do free-variables) (free '()) (defined '()))
      (cond ((null? to-do)
             (values (reverse! free) (reverse! defined) new-support))
            (else
             (xcase (early-bind-var (car to-do) new-support support)
               ((defined)
                (loop (cdr to-do) free (cons (car to-do) defined)))
               ((early-bound)
                (loop (cdr to-do) free defined))
               ((free)
                (loop (cdr to-do) (cons (car to-do) free) defined))))))))


(define (early-bind-var var new-support support-env)
  (let* ((defs (find-definitions var))
         (name (variable-name var))
         (support (support-env name)))
    (if (and defs support)
        (warning "shadowing ~S" name))
    (cond ((< 1 (length defs))
           (fix-multiple-definitions var)
           'defined)
          (defs
           (*define-support var new-support (caar defs) (cdar defs))
           'defined)
          (support
           (replace-with-bound-var var (support.variable support)))
          (else
           'free))))

(define (replace-with-bound-var var bound-var)
  (noise "Early binding ~S~%"
         (variable-name var))
  (iterate loop ((refs (variable-refs var)) (result 'early-bound))
    (cond ((null? refs) result)
          ((eq? (node-role (car refs)) call-proc)
           (set (reference-variable (car refs)) bound-var)
           (loop (cdr refs) result))
          (else
           (loop (cdr refs) 'free)))))
