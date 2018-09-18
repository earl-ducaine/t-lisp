(herald tcload)

;;; Load a TC into a T.

(define *tc-env*
  (locale tc-env

     (import *t-implementation-env*
             processor-type
             make-aegis-fs
             make-unix-fs
             make-vms-fs)

     (define *ESTABLISH-TARGET-PARAMETERS?* nil)

     (set (load-noisily?) nil)
     (require (tsys files))

     (define (tcload what)
       (load '(tcomp tctop) tc-env)
       (xcase what
         ((am)
          (establish-target-parameters
            (make-aegis-fs '(sample))
            (object nil ((processor-type self) 'mc68000)))) ;kludge
         ((up)
          (establish-target-parameters
            (make-unix-fs '(sample))
            (object nil ((processor-type self) 'pyramid))))
         ((uv)
          (establish-target-parameters
            (make-unix-fs '(sample))
            (object nil ((processor-type self) 'vax11))))
         ((vv)
          (establish-target-parameters
            (make-vms-fs '(sample))
            (object nil ((processor-type self) 'vax11)))))
       (walk (lambda (f) (load f tc-env))
             (cddr *tc-files*))
       (walk (lambda (f) (load f tc-env))
             (xcase what
               ((am)    *m68-tc-files*)
               ((up)    *pyramid-tc-files*)
               ((uv vv) *vax-tc-files*)))
       (walk (lambda (f) (load f tc-env))
             (xcase what
               ((am up) '())
               ((uv) '((tcomp unemit))) ;kludge!
               ((uv) '((tcomp vmsemit))))))

     tc-env))

(define tcload (*value *tc-env* 'tcload))

;;; Are the following still necessary?

;(define mapcar map)  ;temporary patch.
;(define map1 map)
;(import *t-implementation-env* arglist->argspectrum)
