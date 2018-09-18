(HERALD (TSYS CARCDR T 23)
        (ENV () (TSYS OPEN) (TSYS ALIASES)))

;;; Copyright (c) 1983, 1984 Yale University

;;;; CAR/CDR chains

(DEFINE-INTEGRABLE CARCDRABLE? LIST?)

(DEFINE (MAKE-C*R ID REST)
  (LET ((BITS        (MAKE-C*R-BITS REST))
        (SETTER-BITS (MAKE-C*R-BITS (CDR REST)))
        (LAST-ONE (CAR REST)))
    (LET ((THE-SETTER 
           (OBJECT (LAMBDA (SEXPR VALUE)
                     (LET ((PLACE (CARCDR SEXPR SETTER-BITS ID
                                          "attempting to set ~S of ~S")))
                       (COND ((NOT (PAIR? PLACE))
                              (ERROR "attempting to set ~S of ~S"
                                     ID
                                     SEXPR))  ;wrong recovery!!
                             ((EQ? LAST-ONE CAR)
                              (SET (CAR PLACE) VALUE))
                             (ELSE
                              (SET (CDR PLACE) VALUE)))))
                   ((IDENTIFICATION SELF) `(SETTER ,ID)))))
      (OBJECT (LAMBDA (SEXPR)
                (CARCDR SEXPR BITS ID "attempting to take ~S of ~S"))
              ((IDENTIFICATION SELF) ID)
              ((SETTER SELF) THE-SETTER)))))

(DEFINE (CARCDR SEXPR BITS ID ERROR-FORMAT-STRING)
  (ITERATE LOOP ((B BITS)
                 (S SEXPR))
    (COND ((FX= B 1) S)
          ((NOT (CARCDRABLE? S))
           (CARCDR (ERROR ERROR-FORMAT-STRING
                          ID
                          SEXPR)
                   BITS
                   ID
                   ERROR-FORMAT-STRING))
          ((FIXNUM-EVEN? B)
           (LOOP (POINTER-ASH B         -1) (CAR S)))
          (ELSE
           (LOOP (POINTER-ASH (FX- B 1) -1) (CDR S))))))

;;; Assume that length of the CARS&CDRS list is no greater than the number
;;; of bits per fixnum, less two.

(DEFINE (MAKE-C*R-BITS CARS&CDRS)
  (DO ((B 1
          (FX+ (FX* B 2)
               (SELECT (CAR C)
                 ((CAR) 0)
                 ((CDR) 1)
                 (ELSE (ERROR "bogus carcdr list: ~S" CARS&CDRS)))))
       (C CARS&CDRS (CDR C)))
      ((NULL? C) B)))

(DEFINE-LOCAL-SYNTAX (DEFINE-C*R NAME)
  (LET* ((PN (SYMBOL-PNAME NAME))
         (EXPANSION (DO ((I (FX- (STRING-LENGTH PN) 2) (FX- I 1))
                         (Q 'SEXPR `(,(CASE (NTHCHAR PN I)
                                        ((#\A) 'CAR)
                                        ((#\D) 'CDR))
                                     ,Q)))
                        ((FX= I 0) Q))))
    `(BLOCK (DEFINE-INTEGRABLE ,NAME
              (IF-INTEGRATED (LAMBDA (SEXPR) ,EXPANSION)
                             (*DEFINE-C*R ',NAME)))
            (DECLARE-SETTER ,NAME
              (LAMBDA (SEXPR VALUE) (SET ,EXPANSION VALUE))))))

(DEFINE (*DEFINE-C*R NAME)
  (MAKE-C*R NAME (LET ((PN (SYMBOL-PNAME NAME)))
                   (DO ((I (FX- (STRING-LENGTH PN) 2) (FX- I 1))
                        (L '() (CONS (CASE (NTHCHAR PN I)
                                       ((#\A) CAR)
                                       ((#\D) CDR))
                                     L)))
                       ((FX= I 0) L)))))


;(DEFINE-C*R CR)                        ; Useless

;(DEFINE-C*R CAR)                       ; Redundant
;(DEFINE-C*R CDR)

(DEFINE-C*R CAAR)                       ; Lexicographic order by pname
(DEFINE-C*R CADR)
(DEFINE-C*R CDAR)
(DEFINE-C*R CDDR)

(DEFINE-C*R CAAAR)
(DEFINE-C*R CAADR)
(DEFINE-C*R CADAR)
(DEFINE-C*R CADDR)
(DEFINE-C*R CDAAR)
(DEFINE-C*R CDADR)
(DEFINE-C*R CDDAR)
(DEFINE-C*R CDDDR)

(DEFINE-C*R CAAAAR)
(DEFINE-C*R CAAADR)
(DEFINE-C*R CAADAR)
(DEFINE-C*R CAADDR)
(DEFINE-C*R CADAAR)
(DEFINE-C*R CADADR)
(DEFINE-C*R CADDAR)
(DEFINE-C*R CADDDR)
(DEFINE-C*R CDAAAR)
(DEFINE-C*R CDAADR)
(DEFINE-C*R CDADAR)
(DEFINE-C*R CDADDR)
(DEFINE-C*R CDDAAR)
(DEFINE-C*R CDDADR)
(DEFINE-C*R CDDDAR)
(DEFINE-C*R CDDDDR)
