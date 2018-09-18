(define (close-analyze-top node free-variables defined-variables)
    (set *unit-closures* nil)
    (set *unit-templates* nil)

    (let* ((l ((call-arg 1) (lambda-body node)))
           (via (car (lambda-variables l))))
      (close-analyze-body (lambda-body l) nil '() via '() via)
      (set *unit* (create-unit free-variables *unit-templates*
                               *unit-closures* defined-variables))
      (create-environment l *unit* 0)))



