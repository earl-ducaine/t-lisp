(herald oload)

(define *orbit-env*
  (make-locale *standard-env* '*orbit-env*))

(*define *standard-env* '*orbit-env* *orbit-env*)

(define *orbit-files*
   '((orbit syntax)
     (orbit util)
     (orbit sets)
     (orbit defs)
     (orbit new-syntax)
     (orbit alpha)
     (orbit assign)
     (orbit convert)
     (orbit early)
     (orbit simplify)
     (orbit simplifiers)
     (orbit presimplify)
     (orbit primops)
     (orbit closure)
     (orbit reg)
     (orbit vaxemit)
     (orbit vaxgen)
     (orbit support)
     (orbit top)
     (orbit vaxis)
     (orbit fix)
     (orbit patch)
     ))

(*define *orbit-env* '*orbit-files* *orbit-files*)

(set (repl-env) *orbit-env*)
(walk (lambda (f) (load f *orbit-env*))
      *orbit-files*)
