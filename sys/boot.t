(herald (tsys boot t 47)
        (pre-cook)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; System startup

;;; This is where the T startup sequence jumps: right into the
;;; eval-mungeables of this file.  We must not return - the kernel
;;; hasn't prepared for that possibility.  All it has done at this point
;;; is align the stack, set up the heap and SLP, and jump here.

;;; The "setup procedure" for this file has been located as that of the
;;; 0th unit in the vector *THE-INITIAL-UNITS*, which is established by
;;; SYSGEN.

;;; *DYNAMIC-STATE* is set here in BOOT rather than in, say, THROW, because
;;; we may want to be able to reload THROW.

(lap (initial-value *initialized?* null))

(cond ((not *initialized?*)
       (LSET *DYNAMIC-STATE* NIL)
       (LSET *PRE-GC-AGENDA* '())
       (LSET *POST-GC-AGENDA* '())
       (LSET *BREAK-LEVEL* 1)                  ; Number of nested break loops.
       (LSET *SYNTAX-SYSTEM-EXISTS?* NIL)
       (LSET *THE-BOOT-ENV* NIL)               ; gets set by SYSTEM.T
       (LSET *EMBEDDED-SYSTEMS* '())
        
       ;;; Initialization loop
        
       (LSET *TOP-LEVEL*
             (LAMBDA ()
               (SET *TOP-LEVEL* NIL)
               (DO ((I 1 (FX+ I 1)))
                   ((FX= I (VECTOR-LENGTH *THE-INITIAL-UNITS*)) T) ;value is ignored
                 (LET ((U (VREF *THE-INITIAL-UNITS* I)))
                   (SELECT (CODE-MAGIC-NUMBER (UNIT-CODE U))
                     ((%%CORRECT-FASL %%INCORRECT-FASL)
                      (RELOCATE-UNIT U *THE-BOOT-ENV* 'T))
                     ((%%PRECOOKED))
                     (ELSE (ERROR "lossage in initialization - unit = ~S" U)))
                   ((UNIT-THING U))))))
       (set *initialized?* t)
        
       ;;; Throw to top level.
        
       (DEFINE (RESET)
         (**RESET** NIL))
        
       ;;; Top level.
        
       (DEFINE (TOP-LEVEL)
         (CATCH TOP
                (SET **RESET** TOP)
                (*TOP-LEVEL*))
         (SET *BREAK-LEVEL* 0)
         (TOP-LEVEL))
))

(TOP-LEVEL)
