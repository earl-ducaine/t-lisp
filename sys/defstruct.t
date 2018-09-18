(HERALD (TSYS DEFSTRUCT T 26)
        (ENV TSYS))

;;; Copyright (c) 1982, 1983, 1984 Yale University

;;;; Structure type definition

;;; Macro layer to sit above STRUCT, perhaps to become hairy in the future.
;;; - Alternate naming strategies
;;; - Initialization
;;; - Restricted fields
;;; - Variant record types
;;; (although why bother, really?).

;;; (DEFINE-STRUCTURE-TYPE SHIP X Y)  ==>
;;; 
;;; (BLOCK (DEFINE SHIP-STYPE  (MAKE-STYPE 'SHIP '(X Y)))
;;;        (DEFINE MAKE-SHIP   (STYPE-CONSTRUCTOR SHIP-STYPE))
;;;        (DEFINE SHIP?       (STYPE-PREDICATOR SHIP-STYPE))
;;;        (DEFINE HANDLE-SHIP (STYPE-HANDLER SHIP-STYPE))
;;;        (DEFINE SHIP-X      (STYPE-SELECTOR SHIP-STYPE 'X))
;;;        (DEFINE SHIP-Y      (STYPE-SELECTOR SHIP-STYPE 'Y))
;;;        SHIP-STYPE)

(DEFINE-SYNTAX (DEFINE-STRUCTURE-TYPE TYPE-ID . SPECS)
  (LET ((STYPE (CONCATENATE-SYMBOL TYPE-ID '-STYPE)))
    `(BLOCK (COND ((OR (NOT (BOUND? ,STYPE))
                       (NOT (STYPE-COMPATIBLE? ,STYPE ',TYPE-ID ',SPECS)))
                   (DEFINE ,STYPE (MAKE-OLD-STYPE ',TYPE-ID ',SPECS))))
            (DEFINE ,(CONCATENATE-SYMBOL 'MAKE- TYPE-ID)
              (STYPE-CONSTRUCTOR ,STYPE))
            (DEFINE ,(CONCATENATE-SYMBOL TYPE-ID '?)
              (STYPE-PREDICATOR ,STYPE))
            (DEFINE ,(CONCATENATE-SYMBOL 'HANDLE- TYPE-ID)
              (STYPE-HANDLER ,STYPE))
            ;; Horrible el hacko definition to permit open-coding.
            ;; There has to be a better way to do this.
            ,@(DO ((S SPECS (CDR S))
                   (I 0 (FX+ I 1))
                   (Z '() (CONS `(,SYNTAX/DEFINE-STRUCTURE-SELECTOR
                                   ,(CONCATENATE-SYMBOL TYPE-ID '- (CAR S))
                                   ,STYPE ,(CAR S) ,I)
                                Z)))
                  ((NULL? S) (REVERSE! Z)))
            ,STYPE)))

;;; Note: at least *STRUCT-OVERHEAD* ought to be a function of the
;;; stype, because it may change from 2 to 0 in the future...

(DEFINE SYNTAX/DEFINE-STRUCTURE-SELECTOR
  (MACRO-EXPANDER (DEFINE-STRUCTURE-SELECTOR SELECTOR STYPE FIELD I)
    (LET ((J (FX+ I *STRUCT-OVERHEAD*)))
      `(,(T-SYNTAX 'BLOCK)
	(,(T-SYNTAX 'DEFINE-INTEGRABLE)
	 ,SELECTOR
	 (,(T-SYNTAX 'IF-INTEGRATED)
	  (,(T-SYNTAX 'LAMBDA) (OBJ)
			       (EXTEND-ELT-FIXED OBJ ,J))
	  (STYPE-SELECTOR ,STYPE ',FIELD)))
	(,(T-SYNTAX 'DECLARE-SETTER) ,SELECTOR
			      (,(T-SYNTAX 'LAMBDA) (OBJ VAL)
				(SET-EXTEND-ELT-FIXED OBJ ,J VAL)))))))

(DEFINE-SYNTAX DEFINE-STRUCTURE-SELECTOR SYNTAX/DEFINE-STRUCTURE-SELECTOR)
