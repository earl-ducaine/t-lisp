(HERALD AEGIS
        (PRE-COOK)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; T/AEGIS interface

;;; This is called from INITIALIZE-T.  Do interesting stuff.
;;; This happens as the initial read-eval-print is entered, but before the
;;; patch file and init file are loaded.

(DEFINE (INITIALIZE-OS-STUFF)
  (INITIALIZE-FAULT-HANDLERS)
  (DO ((I (FX- (ARGC) 1) (FX- I 1))
       (L '() (CONS (ARGV-REF I) L)))
      ((FX< I 0) (SET *COMMAND-LINE* L))))

(DEFINE (EXPERIMENTAL?) 
  (OR (STRING-POSQ #\x (CAR *COMMAND-LINE*))
      (STRING-POSQ #\X (CAR *COMMAND-LINE*))))

(DEFINE (THE-T-SYSTEM-DIRECTORY)
  (IF (EXPERIMENTAL?) "~sys/xt" "~sys/t"))

(DEFINE (THE-INIT-FILE-DIRECTORY)
  "~")

;;; A random collection of procedures that interact with AEGIS.

;;; Define a routine with the own variable STS in such a way that you
;;; can ask for the last returned status code.  

(DEFINE-LOCAL-SYNTAX (DEFINE-WITH-STATUS PAT STS . REST)
  (DESTRUCTURE (((PROC . ARGS) PAT))
    `(DEFINE ,PROC
              (LET ((,STS (MAKE-XENOID 0)))
                (OBJECT (NAMED-LAMBDA ,PROC ,ARGS ,@REST)
                        ((LAST-STATUS SELF) ,STS))))))

(DEFINE-OPERATION (LAST-STATUS OBJ))

(DEFINE-INTEGRABLE (ZERO-XENOID? X) (FX= (XENOID->FIXNUM X) 0))

(define (make-uid) (make-bytev 8))

;;; CONT is passed 4 fixnums

(let ((uid (make-bytev 8)))
  (define (uid-gen -c-)
    (call-xenoid nil nil *uid_$gen-xenoid* uid)
    (-c- (fixnum-logand (bref-16 uid 0) #x+FFFF)
         (fixnum-logand (bref-16 uid 2) #x+FFFF)
         (fixnum-logand (bref-16 uid 4) #x+FFFF)
         (fixnum-logand (bref-16 uid 6) #x+FFFF))))

;;; (SET-GUARD uid address[pointer]) -- returns true or false

(define-with-status (set-guard uid address) sts
  (call-xenoid nil nil *mst_$set_guard-xenoid*
    address 32768
    sts #\l #\p uid)
  (zero-xenoid? sts))

;;; (MAKE-EXTERNAL-OBJECT str) - returns a xenoid

(define-constant kg_$name_string 32)

(define (make-external-object str) 
  (let ((name (string-fill (make-string kg_$name_string) #\space))
        (x (make-xenoid 0)))
    (string-upcase!
     (string-replace name str (min (string-length str) kg_$name_string)))
    (call-xenoid 'addr x *kg_$lookup-xenoid* name)))

;;; -C- is passed 3 strings: code, module, subsystem
;;; STATUS is a xenoid or fixnum

(let ((c  (make-xenoid 0))
      (cl (make-xenoid 0))
      (m  (make-xenoid 0))
      (ml (make-xenoid 0))
      (s  (make-xenoid 0))
      (sl (make-xenoid 0)))
  (define (apollo-error-text status -c-)
    (let ((status
           (if (fixnum? status) (make-xenoid (fixnum->pointer status)) status)))
      (call-xenoid nil nil *error_$find_text-xenoid* 
        cl c ml m sl s status)
      (-c- (apollo-string->string c (bref-16 cl 0))
           (apollo-string->string m (bref-16 ml 0))
           (apollo-string->string s (bref-16 sl 0)))))
  )
                                    (bref-16 len 0)
(define-constant *size-of-proc2_$info_t* 36)    ; !!! May change with Apollo
                                                ; releases

;;; Returns a (BYTEV 36)

(define-with-status (process-info) sts
  (let ((uid (make-bytev 8))
        (info (make-bytev *size-of-proc2_$info_t*)))
    (call-xenoid nil nil *proc2_$who_am_i-xenoid* uid)
    (call-xenoid nil nil *proc2_$get_info-xenoid*
      *size-of-proc2_$info_t* sts #\w info uid)
    info))

;;; Command line hacking

(DEFINE (ARGC)
  (LET ((XARGC (MAKE-XENOID 0))
        (XARGV (MAKE-XENOID 0)))
    (CALL-XENOID NIL NIL *PGM_$GET_ARGS-XENOID* XARGV XARGC)
    (FIXNUM-ASHR (XENOID-POINTER XARGC) 13)))

(DEFINE (ARGV-REF N)
  (LET ((STATUS (MAKE-XENOID 0))
        (B (GET-BUFFER)))
    (SET-STRING-LENGTH B (BUFFER-SIZE B))
    (LET ((LEN (CALL-XENOID 'DATA 'FIXNUM *PGM_$GET_ARG-XENOID*
                 N #\s STATUS B #\w)))
      (LET ((S (STRING-REPLACE (MAKE-STRING LEN) B LEN)))
        (RELEASE-BUFFER B)
        S))))

;;; PTR is a xenoid whose ptr points to a string
;;; LEN is a fixnum

(define (apollo-string->string ptr len)
  (let ((kludge (chopy "")))
    (set-string-pointer kludge (xenoid-pointer ptr))
    (set-string-length kludge len)  
    (copy-string kludge)))

;;; HACK-O flonums

(DEFINE (MAKE-FLONUM) (CHANGE-TAG (NEW-CELL) %%PAIR-TAG %%FLONUM-TAG))

(DEFINE (STRING->FLONUM S)
  (LET ((N (MAKE-FLONUM))
        (B (GET-BUFFER-OF-SIZE 50)))
    (SET (STRING-LENGTH B) 50)
    (STRING-FILL B #\SPACE)
    (STRING-REPLACE B S (STRING-LENGTH S))
    (CALL-XENOID NIL NIL *T_$ATOF-XENOID* N B)
    (RELEASE-BUFFER B)
    N))

(LSET *PRINT-FLONUMS-KLUDGILY?* T)

(DEFINE (PRINT-FLONUM-KLUDGILY OBJ STREAM)
  (LET ((B (FLONUM->BUFFER OBJ)))
    ;; What to do?
    (WRITES STREAM B)
    (RELEASE-BUFFER B)))

(DEFINE (FLONUM->BUFFER N)
  (FLONUM->BUFFER! N (GET-BUFFER-OF-SIZE 23)))

;; Expects a buffer of size 23
(DEFINE (FLONUM->BUFFER! N B)
  (SET-STRING-LENGTH B 23)              ; !
  (CALL-XENOID NIL NIL *T_$FTOA-XENOID* B N)
  B)

(DEFINE (MAKE-FL-PROC-1 XENOID ID)
  (OBJECT (LAMBDA (X)
            (LET ((X (CHECK-ARG FLONUM? X ID))
                  (RESULT (MAKE-FLONUM)))
              (CALL-XENOID NIL NIL XENOID RESULT X)
              RESULT))
          ((IDENTIFICATION SELF) ID)))

(DEFINE (MAKE-FL-PROC-2 XENOID ID)
  (OBJECT (LAMBDA (X Y)
            (LET ((X (CHECK-ARG FLONUM? X ID))
                  (Y (CHECK-ARG FLONUM? Y ID))
                  (RESULT (MAKE-FLONUM)))
              (CALL-XENOID NIL NIL XENOID RESULT Y X)
              RESULT))
          ((IDENTIFICATION SELF) ID)))

(DEFINE (MAKE-FL-PROC-3 XENOID ID)
  (OBJECT (LAMBDA (X Y)
            (LET ((X (CHECK-ARG FLONUM? X ID))
                  (Y (CHECK-ARG FLONUM? Y ID)))
              (FIXNUM-ODD? (CALL-XENOID 'DATA 'FIXNUM XENOID Y X))))
          ((IDENTIFICATION SELF) ID)))

(DEFINE SIN    (MAKE-FL-PROC-1 *T_$SIN-XENOID*  'SIN))
(DEFINE COS    (MAKE-FL-PROC-1 *T_$COS-XENOID*  'COS))
(DEFINE TAN    (MAKE-FL-PROC-1 *T_$TAN-XENOID*  'TAN))
;(DEFINE ASIN   (MAKE-FL-PROC-1 *T_$ASIN-XENOID* 'ASIN))
;(DEFINE ACOS   (MAKE-FL-PROC-1 *T_$ACOS-XENOID* 'ACOS))
(DEFINE ATAN   (MAKE-FL-PROC-1 *T_$ATAN-XENOID* 'ATAN))
(DEFINE EXP    (MAKE-FL-PROC-1 *T_$EXP-XENOID*  'EXP))
(DEFINE LOG    (MAKE-FL-PROC-1 *T_$LOG-XENOID*  'LOG))
(DEFINE SQRT   (MAKE-FL-PROC-1 *T_$SQRT-XENOID* 'SQRT))
;;; ... also need power and atan2

(DEFINE (ASIN N)
  (ERROR "ASIN is not yet implemented in Aegis T"))
(DEFINE (ACOS N)
  (ERROR "ACOS is not yet implemented in Aegis T"))

(DEFINE FLONUM-ADD
  (MAKE-FL-PROC-2 *T_$FLADD-XENOID* 'FLONUM-ADD))

(DEFINE FLONUM-SUBTRACT 
  (MAKE-FL-PROC-2 *T_$FLSUB-XENOID* 'FLONUM-SUBTRACT))

(DEFINE FLONUM-MULTIPLY 
  (MAKE-FL-PROC-2 *T_$FLMUL-XENOID* 'FLONUM-MULTIPLY))

(DEFINE FLONUM-DIVIDE
  (MAKE-FL-PROC-2 *T_$FLDIV-XENOID* 'FLONUM-DIVIDE))

(DEFINE FLONUM-LESS?
  (MAKE-FL-PROC-3 *T_$FLLESS-XENOID* 'FLONUM-LESS?))

(DEFINE FLONUM-EQUAL?
  (MAKE-FL-PROC-3 *T_$FLEQUAL-XENOID* 'FLONUM-EQUAL?))

(DEFINE FLONUM-GREATER? 
  (MAKE-FL-PROC-3 *T_$FLGREATER-XENOID* 'FLONUM-GREATER?))

(DEFINE (FLONUM-NOT-EQUAL? A B) (NOT (FLONUM-EQUAL? A B)))
(DEFINE (FLONUM-NOT-LESS? A B) (NOT (FLONUM-LESS? A B)))
(DEFINE (FLONUM-NOT-GREATER? A B) (NOT (FLONUM-GREATER? A B)))

(DEFINE (FIXNUM->FLONUM FX) 
  (LET ((FX (CHECK-ARG FIXNUM? FX FIXNUM->FLONUM))
        (RESULT (MAKE-FLONUM)))
    (CALL-XENOID NIL NIL *T_$FLOAT-XENOID* FX RESULT #\l)
    RESULT))

(DEFINE (FLONUM->FIXNUM FL) 
  (LET ((FL (CHECK-ARG FLONUM? FL FLONUM->FIXNUM)))
    (CALL-XENOID 'DATA 'FIXNUM *T_$FIX-XENOID* FL)))

(DEFINE (LUSER-TYPED-EOF-AT-TOP-LEVEL)
  (FORMAT (ERROR-OUTPUT) "** Use (EXIT) to exit.~%"))

;;; Exit from T, optionally setting the return code

(DEFINE (EXIT . FOO)
  (LET ((Z (IF (NULL? FOO) 0 (CAR FOO))))
    (UNMAP-AREAS)
    (CALL-XENOID NIL NIL *PGM_$EXIT-XENOID* Z #\l)))

;;; Set reckless mode
;;; E.g. (SET (RECKLESSNESS) 'MEDIUM) (also 'LOW AND 'HIGH)

(LSET *RECKLESSNESS* 'LOW)

(DEFINE (SET-ICALL-AND-IRETURN INDEX-OF-NEW-ICALL INDEX-OF-NEW-IRETURN)
  (XSET *THE-SLINK* %%ICALL-INDEX   (XREF *THE-SLINK* INDEX-OF-NEW-ICALL))
  (XSET *THE-SLINK* %%IRETURN-INDEX (XREF *THE-SLINK* INDEX-OF-NEW-IRETURN)))

(DEFINE (SET-RECKLESSNESS FLAG)
  (CASE FLAG
    ((LOW)
     (SET-ICALL-AND-IRETURN %%PARANOID-ICALL-INDEX  %%PARANOID-IRETURN-INDEX))
    ((MEDIUM)
     (SET-ICALL-AND-IRETURN %%ZIPPY-ICALL-INDEX     %%CONFIDENT-IRETURN-INDEX))
    ((HIGH)
     (SET-ICALL-AND-IRETURN %%CONFIDENT-ICALL-INDEX %%CONFIDENT-IRETURN-INDEX)))
  (SET *RECKLESSNESS* FLAG))

(DEFINE RECKLESSNESS
  (OBJECT (LAMBDA () *RECKLESSNESS*)
          ((SETTER SELF) SET-RECKLESSNESS)
          ((IDENTIFICATION SELF) 'RECKLESSNESS)))
