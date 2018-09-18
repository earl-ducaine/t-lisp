(herald doload)

(define *orbit-env*
  (make-locale *standard-env* '*orbit-env*))

(*define *standard-env* '*orbit-env* *orbit-env*)

(define *orbit-files*
   '((orbit syntax)      ; rak
     (orbit util)        ; rak
     (orbit sets)        ; OK
     (orbit defs)        ; rak
     (orbit alpha)       ; rak
     (orbit assign)      ; rak
     (orbit convert)     ; rak
     (orbit early)       ; rak
     (orbit simplify)    ; rak + simplify-setter
     (orbit simplifiers) ; rak
     (orbit transform)   ; rak
     (orbit primops)     ; rak + some more operations
     (orbit closure)     ; frozen (for now)
     (orbit reg)         ; frozen + locations
     (orbit vaxemit)     ; frozen, needs *unit-values*
     (orbit vaxgen)      ; procedures from frozen vaxprimops
     (orbit support)     ; rak
     (orbit dtop)         ; rak + changes?
     (orbit fix)         ;
     (orbit patch)
     ))

(*define *orbit-env* '*orbit-files* *orbit-files*)

(set (repl-env) *orbit-env*)
(walk (lambda (f) (load f *orbit-env*))
      *orbit-files*)


(eval '(orbit-vax-init) *orbit-env*)
