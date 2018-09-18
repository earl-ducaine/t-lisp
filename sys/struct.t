(HERALD (TSYS STRUCT T 59)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Anonymous structure types

;;; (MAKE-STYPE TYPE-ID SPECS) creates a new "structure type" (STYPE).
;;; The TYPE-ID is used for identification purposes only.
;;; The SPECS should be a list of symbols (actually anything will do),
;;; used for identifying the field selectors.
;;; Any of these symbols may be given as the second argument to STYPE-SELECTOR,
;;; which will return the corresponding structure selector.

;;; ---------- Example:
;;;
;;; (DEFINE SHIP-STYPE (MAKE-STYPE 'SHIP '(DIRECTION VELOCITY NAME)))
;;;
;;; (DEFINE SHIP-DIRECTION (STYPE-SELECTOR SHIP-STYPE 'DIRECTION))
;;;
;;; (DEFINE MAKE-SHIP (STYPE-CONSTRUCTOR SHIP-STYPE))
;;;
;;; (SET A-SHIP (MAKE-SHIP))
;;;
;;; (SET (SHIP-DIRECTION A-SHIP) 4.3)
;;; (SHIP-DIRECTION A-SHIP)  =>  4.3
;;;
;;; (DEFINE HANDLE-SHIP (STYPE-HANDLER SHIP-STYPE))  ...


;;; Auxiliary routines.
;;; Bogus definitions to use while MAKE-TEMPLATE doesn't exist.
;;; Sorry about constant use of 0 and 1 indices but what the hell.

;;; %MAKE-STYPE-TEMPLATE - no one looks at SIZE but what the heck.

(DEFINE-INTEGRABLE (%MAKE-STYPE-TEMPLATE SIZE) (CONS NIL SIZE))

(DEFINE-INTEGRABLE %STYPE-TEMPLATE-HANDLER CAR)
(DEFINE-INTEGRABLE %SET-STYPE-TEMPLATE-HANDLER (SETTER CAR))

(DEFINE-INTEGRABLE (%STRUCT-STEM STRUCT) (EXTEND-ELT STRUCT 0))
(DEFINE-INTEGRABLE (%STRUCT-SIZE STRUCT) (EXTEND-ELT STRUCT 1)) ; for GC

(DEFINE-CONSTANT *STRUCT-OVERHEAD* 2)

(DEFINE (%MAKE-STRUCT STEM SIZE)
  (LET ((STRUCT (MAKE-EXTEND-N *STRUCTURE-TEMPLATE* SIZE)))
    (SET (EXTEND-ELT STRUCT 0) (%STYPE-TEMPLATE-HANDLER STEM))  ; kernel wants
    (SET (EXTEND-ELT STRUCT 1) SIZE)            ; GC wants
    STRUCT))

(DEFINE-INTEGRABLE (%STRUCT-WITH-TEMPLATE? OBJ STEM)
  (AND (EXTEND? OBJ)
       (EQ? (EXTEND-TEMPLATE OBJ) *STRUCTURE-TEMPLATE*)
       (EQ? (%STRUCT-STEM OBJ) (%STYPE-TEMPLATE-HANDLER STEM))))        ; gross

(DEFINE-INTEGRABLE (%STRUCT-OF-SAME-TYPE? STRUCT1 STRUCT2)
  (EQ? (%STRUCT-STEM STRUCT1) (%STRUCT-STEM STRUCT2)))

(DEFINE-INTEGRABLE %STRUCT-REF EXTEND-ELT)
(DEFINE-INTEGRABLE %STRUCT-SET (SETTER EXTEND-ELT))

(DEFINE-INTEGRABLE (STRUCTURE? OBJ)
  (AND (EXTEND? OBJ) (EQ? (EXTEND-TEMPLATE OBJ) *STRUCTURE-TEMPLATE*)))

;;; Stuff

(DEFINE-OPERATION (STRUCTURE-TYPE OBJ) NIL)
(DEFINE-OPERATION (SELECTOR-ID OBJ))
(DEFINE-OPERATION (RELEVANT-STYPE OBJ))
(DEFINE-OPERATION (STYPE-PREDICATOR STYPE))
(DEFINE-OPERATION (STYPE-CONSTRUCTOR STYPE))
(DEFINE-OPERATION (STYPE-SELECTOR STYPE ID))
(DEFINE-OPERATION (STYPE-SELECTORS STYPE))
(DEFINE-OPERATION (STYPE-MASTER STYPE))
(DEFINE-OPERATION (STYPE-ID STYPE))
(DEFINE-PREDICATE STYPE?)
(DEFINE-SETTABLE-OPERATION (STYPE-HANDLER STYPE))
(DEFINE SET-STYPE-HANDLER (SETTER STYPE-HANDLER))

(DEFINE-INTEGRABLE (COPY-STRUCTURE STRUCT)
  (LET ((STRUCT (IF-INTEGRATED STRUCT
                               (CHECK-ARG STRUCTURE? STRUCT COPY-STRUCTURE))))
    (COPY-EXTEND STRUCT (EXTEND-TEMPLATE STRUCT) (%STRUCT-SIZE STRUCT))))

(DEFINE (COPY-STRUCTURE! TO-STRUCT FROM-STRUCT)
  (LET ((TO-STRUCT (CHECK-ARG STRUCTURE? TO-STRUCT COPY-STRUCTURE!))
        (FROM-STRUCT (CHECK-ARG STRUCTURE? FROM-STRUCT COPY-STRUCTURE!)))
    (COND ((NOT (%STRUCT-OF-SAME-TYPE? TO-STRUCT FROM-STRUCT))
           (COPY-STRUCTURE! (ERROR "structure types don't match~%  ~S"
                                   `(COPY-STRUCTURE ,TO-STRUCT ,FROM-STRUCT))
                            FROM-STRUCT))
          (ELSE
           (COPY-EXTEND! TO-STRUCT FROM-STRUCT (%STRUCT-SIZE FROM-STRUCT))))))

;;;; MAKE-STYPE

;;; Currently coded using LET and SET instead of LABELS, to get around
;;; compiler bug (!!!!!).  Yuck!

(DEFINE (MAKE-STYPE TYPE-ID SPECS)
  (LET ((SIZE (FX+ (LENGTH SPECS) *STRUCT-OVERHEAD*)))
    (LET ((TEMPLATE (%MAKE-STYPE-TEMPLATE SIZE))
          (STYPE NIL)
          (HANDLR NIL)
          (PREDICATOR NIL)
          (CONSTRUCTOR NIL)
          (SELECTORS NIL)
          (MASTER NIL))
      (SET STYPE
           (OBJECT NIL
                   ((STYPE-PREDICATOR SELF) PREDICATOR)
                   ((STYPE-CONSTRUCTOR SELF) CONSTRUCTOR)
                   ((STYPE-SELECTOR SELF ID)
                    (COND ((MEM (LAMBDA (ID SEL) (EQ? ID (SELECTOR-ID SEL)))
                                ID
                                SELECTORS)
                           => CAR)
                          (ELSE
                           (ERROR "structure type ~S has no such ~
                                  selector name~%  ~S"
                                  TYPE-ID
                                  `(STYPE-SELECTOR ,TYPE-ID ,ID)))))
                   ((STYPE-SELECTORS SELF) SELECTORS)   ; COPY-LIST ?
                   ((STYPE-HANDLER SELF) HANDLR)
                   ((SET-STYPE-HANDLER SELF H)
                    (SET HANDLR (CHECK-ARG HANDLER? H SET-STYPE-HANDLER)))
                   ((STYPE-MASTER SELF) MASTER)
                   ((STYPE-ID SELF) TYPE-ID)
                   ((STYPE? SELF) T)
                   ((PRINT SELF STREAM)
                    (FORMAT STREAM "#{Structure-type~_~S}" TYPE-ID))))
      (SET HANDLR                       ; watch out - there's a HANDLER macro
           (LET ((H (MAKE-MUTABLE-HANDLER TYPE-ID)))
             (DEFINE-METHODS H
	       ((PRINT SELF STREAM) (PRINT-STRUCTURE SELF STREAM))
	       ((STRUCTURE-TYPE SELF) STYPE))
             H))
      (SET PREDICATOR
           (OBJECT (LAMBDA (OBJ)
                     (%STRUCT-WITH-TEMPLATE? OBJ TEMPLATE))
                   ((RELEVANT-STYPE SELF) STYPE)
                   ((PRINT SELF STREAM)
                    (FORMAT STREAM "#{Structure-predicator~_~S}" TYPE-ID))))
      (SET SELECTORS
           (DO ((INDEX *STRUCT-OVERHEAD* (FX+ INDEX 1))
                (SPECS SPECS (CDR SPECS))
                (SELS '()
                      (CONS (MAKE-STRUCT-SELECTOR STYPE
                                                  TEMPLATE
                                                  (CAR SPECS)
                                                  INDEX)
                            SELS)))
               ((NULL? SPECS) (REVERSE! SELS))))
      (SET CONSTRUCTOR
           (OBJECT (LAMBDA ()
                     (COPY-STRUCTURE MASTER))
                   ((RELEVANT-STYPE SELF) STYPE)
                   ((PRINT SELF STREAM)
                    (FORMAT STREAM "#{Structure-constructor~_~S}"
                            TYPE-ID))))
      ;; We say (%HANDLER (=> HANDLR)) instead of just HANDLR so that
      ;; SET-TYPE-HANDLER can work properly (!).
      (%SET-STYPE-TEMPLATE-HANDLER TEMPLATE
                                   (%HANDLER (=> HANDLR)))
      (SET MASTER (%MAKE-STRUCT TEMPLATE SIZE))
      (WALK1 (LAMBDA (SEL)
               (SET (SEL MASTER) (VALUE->NONVALUE SEL)))
             SELECTORS)
      STYPE)))

(DEFINE (PRINT-STRUCTURE STRUCT STREAM)
  (FORMAT STREAM
          "#{Structure~_~S~_~S}"
          (STYPE-ID (STRUCTURE-TYPE STRUCT))
          (OBJECT-HASH STRUCT)))

(DEFINE (MAKE-STRUCT-SELECTOR STYPE TEMPLATE SPEC INDEX)
  (LET ((THE-SETTER
         (LAMBDA (OBJ NEW-VALUE)
           (ITERATE LOOP ((OBJ OBJ))
             (COND ((%STRUCT-WITH-TEMPLATE? OBJ TEMPLATE)
                    (%STRUCT-SET OBJ INDEX NEW-VALUE))
                   (ELSE
                    (LOOP (ERROR '("attempt to alter the ~S component of ~S,~%"
                                 "which is not of structure type ~S, to be ~S")
                                 SPEC
                                 OBJ
                                 (STYPE-ID STYPE)
                                 NEW-VALUE))))))))
    (OBJECT (LAMBDA (OBJ)
              (ITERATE LOOP ((OBJ OBJ))
                (COND ((%STRUCT-WITH-TEMPLATE? OBJ TEMPLATE)
                       (LET ((PROBE (%STRUCT-REF OBJ INDEX)))
                         (COND ((NONVALUE? PROBE)
                                ;; Should check that (nonvalue->value probe)
                                ;;  isn't just some random thing.
                                (ERROR '("attempt to access uninitialized ~S "
                                         "component of ~S")
                                       SPEC
                                       OBJ))
                               (ELSE PROBE))))
                      (ELSE
                       (LOOP
                        (ERROR '("attempt to access the ~S component of ~S,~%"
                                 "which is not of structure type ~S")
                               SPEC
                               OBJ
                               (STYPE-ID STYPE)))))))
            ((SETTER SELF) THE-SETTER)
            ((SELECTOR-ID SELF) SPEC)
            ((RELEVANT-STYPE SELF) STYPE)
            ((PRINT SELF STREAM)
             (FORMAT STREAM "#{Selector~_~S~_~S}"
                     (STYPE-ID STYPE)
                     SPEC)))))

(DEFINE (STYPE-COMPATIBLE? STYPE ID SPECS)
  (AND (STYPE? STYPE)
       (ITERATE LOOP ((S SPECS)
                      (Z (STYPE-SELECTORS STYPE)))
         (COND ((NULL? S) (NULL? Z))
               ((NULL? Z) NIL)
               ((NEQ? (CAR S) (SELECTOR-ID (CAR Z))) NIL)
               (ELSE (LOOP (CDR S) (CDR Z)))))))

(DEFINE (EXHIBIT-STRUCTURE STRUCT)
  (LET ((SELS (STYPE-SELECTORS (STRUCTURE-TYPE STRUCT))))
    (DO ((S SELS (CDR S))
         (I *STRUCT-OVERHEAD* (FX+ I 1)))
        ((NULL? S))
      (LET ((VAL (%STRUCT-REF STRUCT I)))  ;((CAR S) STRUCT)
        (CRAWL-PRINT-COMPONENT (SELECTOR-ID (CAR S))
                               (COND ((NONVALUE? VAL)   ; should be more careful
                                      *EXHIBIT-STRUCTURE-PHOTON*)
                                     (ELSE VAL)))))))

(DEFINE *EXHIBIT-STRUCTURE-PHOTON*      ; ugh
  (OBJECT NIL ((PRINT SELF STREAM) (WRITES STREAM "uninitialized"))))

;;; When implementation changes, calls generated using the old value of
;;; *STRUCT-OVERHEAD* (= 2) should produce error conditions, because
;;; they'll be calling MAKE-OLD-STYPE instead of MAKE-STYPE.

(DEFINE MAKE-OLD-STYPE MAKE-STYPE)


;;; As soon as MAKE-TEMPLATE works, we'll be able to replace the auxiliary
;;; routines defined at the beginning of the file with the following much more
;;; winning  definitions.

(COMMENT 
(DEFINE (%MAKE-STYPE-TEMPLATE SIZE) (MAKE-TEMPLATE NIL NIL NIL SIZE))

(DEFINE-INTEGRABLE %SET-STYPE-TEMPLATE-HANDLER SET-BOGUS-TEMPLATE-HANDLER)

(DEFINE-CONSTANT *STRUCT-OVERHEAD* 0)

(DEFINE-INTEGRABLE %MAKE-STRUCT MAKE-EXTEND-N)

(DEFINE-INTEGRABLE (%STRUCT-WITH-TEMPLATE? OBJ TEMPLATE)
  (AND (EXTEND? OBJ)
       (EQ? (EXTEND-TEMPLATE OBJ) TEMPLATE)))

(DEFINE-INTEGRABLE %STRUCT-REF EXTEND-ELT)
(DEFINE-INTEGRABLE %STRUCT-SET (SETTER EXTEND-ELT))

(DEFINE (%STRUCT-SIZE STRUCT) (something-or-other (EXTEND-TEMPLATE STRUCT)))

(DEFINE (STRUCTURE? OBJ)
  (TRUE? (STRUCTURE-TYPE OBJ)))
)
