(herald oload)

(define *orbit-env*
  (make-locale *standard-env* '*orbit-env*))

(*define *standard-env* '*orbit-env* *orbit-env*)

(define *orbit-files*
   '((frozen closure)
     (frozen reg)
     (frozen vaxemit)
     (frozen vaxgen)
     ))

(*define *orbit-env* '*orbit-files* *orbit-files*)

(set (repl-env) *orbit-env*)
(load '(frozen syntax))
(walk (lambda (f) (comfile f))
      *orbit-files*)
