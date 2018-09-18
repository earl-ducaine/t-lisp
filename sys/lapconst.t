(HERALD (TSYS LAPCONST T 11)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; LAP constants

;;; Horrible hack for getting all the compiler's symbolic constants defined
;;; in the system itself.

(DEFINE-LOCAL-SYNTAX (DEFINE-LAP-CONSTANTS)
 `(BLOCK ,@(MAP (LAMBDA (Z)
                  `(DEFINE-CONSTANT ,Z ,((*VALUE *TC-ENV* 'CGET) Z 'CONSTANT)))
                (*VALUE *TC-ENV* '*LAP-CONSTANTS*))))

(DEFINE-LAP-CONSTANTS)
