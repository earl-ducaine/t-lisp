(herald oload)

(define *orbit-env*
  (make-locale *standard-env* '*orbit-env*))

(*define *standard-env* '*orbit-env* *orbit-env*)

(define *orbit-files*
   '((orbit syntax)
     (orbit sets)
     (orbit convert)
     (orbit early)
     (orbit orbit)
     (orbit test)
     ))

(*define *orbit-env* '*orbit-files* *orbit-files*)

(set (repl-env) *orbit-env*)

(walk (lambda (f) (load f *orbit-env*))
      *orbit-files*)

