(herald tcompenv)

;;; Load this file into a TC to create the TCOMP support environment.

(walk (lambda (v)
        (*define *scratch-env* v (*value *tc-env* v)))
      '(format-nil
        +map
        mapcar
        expand-cons-a
        *node-types*))

(*define-support-env 'tcomp
                     '((tcomp machax)
                       (tcomp defs)
                       (tcomp node)
                       (tcomp gmax)))

;(set (COMPILER-MACRO-DEFINITION-ENV) *tc-env*)


