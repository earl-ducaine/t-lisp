(herald foreign
        (support (make-empty-support-env 'nil) vaxbase))


(define-wired foreign
  (primop foreign ()
    ((primop.simplify self node)
     (let ((arg ((call-arg 2) node)))
       (push (literal-value arg) self)
       (replace node arg)))))
