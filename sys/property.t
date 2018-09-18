(HERALD (TSYS PROPERTY T 10)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Property lists
;;; Make these generic?  How to do so without sacrificing efficiency?

(DEFINE PROPERTY
  (OBJECT (LAMBDA (SYM PROP)
            (COND ((%ASSQ PROP (SYMBOL-PLIST (CHECK-ARG SYMBOL? SYM PROPERTY)))
                   => CDR)
                  (ELSE NIL)))
          ((SETTER SELF) SET-PROPERTY)))

;;; Should call REMOVE-PROPERTY if VAL is false?

(DEFINE (SET-PROPERTY SYM PROP VAL)
  (LET ((SYM (CHECK-ARG SYMBOL? SYM SET-PROPERTY)))
    (COND ((%ASSQ PROP (SYMBOL-PLIST SYM))
           => (LAMBDA (Z) (SET (CDR Z) VAL)))
          (ELSE
           (SET (SYMBOL-PLIST SYM) (CONS (CONS PROP VAL) (SYMBOL-PLIST SYM)))))
    VAL))

(DEFINE (REMOVE-PROPERTY SYM PROP)
  (LET ((SYM (CHECK-ARG SYMBOL? SYM REMOVE-PROPERTY)))
    (ITERATE LOOP ((PREV NIL)
                   (L (SYMBOL-PLIST SYM)))
      (COND ((NULL? L) NIL)
            ((EQ? PROP (CAAR L))
             (IF PREV (SET (CDR PREV) (CDR L))
               (SET (SYMBOL-PLIST SYM) (CDR L)))
             (CDAR L))
            (ELSE (LOOP L (CDR L)))))))

(DEFINE GET PROPERTY)
(DEFINE PUT SET-PROPERTY)
