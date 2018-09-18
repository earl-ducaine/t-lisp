(HERALD (TSYS BACKQUOTE T 24)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Backquote

;;; BACKQUOTE: Support for the amazing ` read macro character
;;; Compare reader and syntax-descriptor printer (temporary kludge given
;;; absence of JOIN)

(DEFINE *BACKQUOTE*
  (MACRO-EXPANDER (*BACKQUOTE* X) (EXPAND-BACKQUOTE X)))

(DEFINE *COMMA*
  (MACRO-EXPANDER (*COMMA* X)
    (SYNTAX-ERROR "comma not inside backquote form~%  ,~S" X)))

(DEFINE *COMMA-ATSIGN*
  (MACRO-EXPANDER (*COMMA-ATSIGN* X)
    (SYNTAX-ERROR "\",@\" not inside backquote form~%  ,@~S" X)))

(DEFINE *QUOTE* 'QUOTE)	        ; Change to (T-SYNTAX 'QUOTE) for some fun

(DEFINE (QUOTATION? X)          ; Crash your T compiler if this is integrable
  (AND (PAIR? X)
       (OR (EQ? (CAR X) *QUOTE*)
	   (EQ? (CAR X) (T-SYNTAX 'QUOTE)))
       (PAIR? (CDR X))
       (NULL? (CDDR X))))

;;; Seems to me that with appropriate continuation-passing and/or
;;; multi-value returns, the following could be rewritten to do much
;;; less list consing, e.g. none in the case of `(X Y Z).  KMP claims
;;; it's not worth it, and I tend to believe him.  -JAR

(DEFINE (EXPAND-BACKQUOTE X)
  (COND ((NULL? X) ''())
        ((ATOM? X) (LIST (T-SYNTAX 'QUOTE) X))
        ((EQ? (CAR X) *BACKQUOTE*)
         (EXPAND-BACKQUOTE (EXPAND-BACKQUOTE (CADR X))))
        ((EQ? (CAR X) *COMMA*) (CADR X))       ;  ,mumble
        ((AND (PAIR? (CAR X)) (EQ? (CAAR X) *COMMA-ATSIGN*))
         ;; (,@mumble ...)
         (LET ((SPLICE-IN (CADAR X))
               (TAIL (EXPAND-BACKQUOTE (CDR X))))
              (COND (;(EQUAL TAIL ''())
                     (AND (QUOTATION? TAIL)
                          (EQ? (CADR TAIL) '()))
                     ;; Use FOO rather than (APPEND FOO '())
                     SPLICE-IN)
                    ((AND (PAIR? TAIL) (EQ? (CAR TAIL) 'APPEND))
                     ;; (APPEND FOO (APPEND BAR BAZ)) => (APPEND FOO BAR BAZ)
                     (CONS* 'APPEND SPLICE-IN (CDR TAIL)))
                    (ELSE (LIST 'APPEND SPLICE-IN TAIL)))))
        (ELSE
         (LET ((A (EXPAND-BACKQUOTE (CAR X)))
               (D (EXPAND-BACKQUOTE (CDR X))))
	    (COND ((QUOTATION? D)
		   (COND ((QUOTATION? A)
			  ;; (CONS 'FOO 'BAR) => '(FOO . BAR)
			  (LIST (T-SYNTAX 'QUOTE) (CONS (CADR A) (CADR D))))
			 ((EQ? (CADR D) '())
			  ;; (CONS FOO '()) => (LIST FOO)
			  (LIST 'LIST A))
			 (ELSE
			  ;; (CONS FOO 'BAR) => (CONS* FOO 'BAR)
			  (LIST 'CONS* A D))))
		  ((AND (PAIR? D) (MEMQ (CAR D) '(LIST CONS*)))  
		   ;; (CONS FOO (LIST . BAR)) => (LIST FOO . BAR)
		   (CONS* (CAR D) A (CDR D)))
		  (ELSE (LIST 'CONS* A D)))))))
