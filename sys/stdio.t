(HERALD (TSYS STDIO T 22)
        (ENV TSYS)
        (SYNTAX-TABLE
         (BLOCK (*REQUIRE 'UNIXMACROS '(TSYS UNIXMACROS) (THE-ENVIRONMENT))
                (ENV-SYNTAX-TABLE (THE-ENVIRONMENT)))))

;;; Copyright (c) 1983, 1984 Yale University

;;;; C Standard I/O library interface

(DEFINE-INTEGRABLE (STDIO-NULL-VALUE) 0)        ; See /usr/include/stdio.h
(DEFINE-INTEGRABLE (STDIO-EOF-VALUE) (FIXNUM->POINTER -1))

(DEFINE (STRING->ASCIZ STRING)
  (LET ((LEN (STRING-LENGTH STRING)))
    (STRING-POINTER (COND ((CHAR= (NTHCHAR STRING LEN) #\NULL)
                           STRING)
                          (ELSE (LET ((NEW-STRING (MAKE-STRING (FX+ 1 LEN))))
				  (STRING-REPLACE NEW-STRING STRING LEN)
				  NEW-STRING))))))

(DEFINE (STRING->ASCIZ! STRING)
  (LET ((LEN (STRING-LENGTH STRING)))
    (STRING-POINTER
     (COND ((CHAR= (NTHCHAR STRING LEN) #\NULL) STRING)
           ((FX< LEN (TEXT-LENGTH (STRING-TEXT STRING)))
            ;; This could really be much more liberal if it wanted to be hairy
            (SET (NTHCHAR STRING LEN) #\NULL)
            STRING)
           (ELSE
	    (LET ((NEW-STRING (MAKE-STRING (FX+ 1 LEN))))
	      (STRING-REPLACE NEW-STRING STRING LEN)
	      NEW-STRING))))))

(DEFINE (ASCIZ->STRING PTR)
  (LET ((KLUDGE (CHOPY "")))
    (SET (STRING-POINTER KLUDGE) PTR)
    (SET (STRING-LENGTH KLUDGE) 32766.)
    (SET (STRING-LENGTH KLUDGE) (%STRING-POSQ #\NULL KLUDGE))
    (COPY-STRING KLUDGE)))

(DEFINE (UNIX-ERROR-STRING)
  (ASCIZ->STRING (XREF (XENOID-POINTER *SYS-ERRLIST-XENOID*) (ERRNO))))  ;Gag!!

;;;; Channel I/O

(define (channel-at-end-of-file? channel)
    (fxn= 0 (call-xenoid *feof-xenoid* channel)))

(define (channel-incurred-error? channel)
    (fxn= 0 (call-xenoid *ferror-xenoid* channel)))

(define (channel->fildes channel)
    (call-xenoid *fileno-xenoid* channel))

;;; Convert T mode keyword list into a string acceptable to stdio's fopen.

(DEFINE (CONVERT-MODES MODES)
  (COND ((MEMQ? 'IN     MODES) "r")
        ((MEMQ? 'OUT    MODES) "w")
        ((MEMQ? 'APPEND MODES) "a")
        (ELSE (ERROR "bad mode list ~S in CONVERT-MODES" MODES))))

(DEFINE (CHANNEL-OPEN NAME MODES)
  (GC-DEFER
   (with-buffer (buf 126)
     (cond ((pointer-equal? 0  (xcall *expand-path-xenoid*
		                      (string-pointer name)
			              (string-pointer buf)))
	    (LET ((PROBE (xcall *FOPEN-XENOID*
                                (string-pointer buf)
                                (STRING-POINTER (CONVERT-MODES MODES)))))
		 (IF (EQ? PROBE (STDIO-NULL-VALUE)) 
		     NIL
		     (MAKE-XENOID PROBE))))
	   (else
            (error "~a" buf))))))

(DEFINE (CHANNEL-OPEN-FILENAME FNAME MODES)
  (CHANNEL-OPEN (FILENAME->STRING FNAME) MODES))

(DEFINE (CHANNEL-CLOSE CHANNEL)
  (GC-DEFER
   (CALL-XENOID *FCLOSE-XENOID* (XENOID-POINTER CHANNEL))
   T))

(DEFINE (CHANNEL-FORCE-OUTPUT CHANNEL)
  (GC-DEFER
   (CALL-XENOID *FFLUSH-XENOID* (XENOID-POINTER CHANNEL))
   T))

(DEFINE (CHANNEL-READC CHANNEL)
  (GC-DEFER
   (LET ((PROBE (CALL-XENOID *FGETC-XENOID* (XENOID-POINTER CHANNEL))))
     (COND ((EQ? PROBE (STDIO-EOF-VALUE))
            (HANDLE-CHANNEL-READ-ERROR CHANNEL CHANNEL-READC))
           (ELSE (POINTER->CHAR PROBE))))))

(DEFINE (CHANNEL-READ-LINE CHANNEL)     ; fgets(s,n,stream)
  (LET ((B (GET-BUFFER-OF-SIZE 500)))
    (SET (STRING-LENGTH B) (BUFFER-SIZE B))       ; Hack
    (LET ((PROBE (CALL-XENOID *FGETS-XENOID*
                              (XENOID-POINTER CHANNEL)
                              (FIXNUM->POINTER (BUFFER-SIZE B))
                              (STRING-POINTER B))))
      (COND ((EQ? PROBE (STDIO-NULL-VALUE))
             (HANDLE-CHANNEL-READ-ERROR CHANNEL CHANNEL-READ-LINE))
            (ELSE (LET ((LEN (OR (%STRING-POSQ #\NEWLINE B)
                              (%STRING-POSQ #\NULL B))))
                 (LET ((S (STRING-REPLACE (MAKE-STRING LEN) B LEN)))
                   (RELEASE-BUFFER B)
                   S)))))))

(DEFINE (HANDLE-CHANNEL-READ-ERROR CHANNEL RETRY)
  (COND ((CHANNEL-AT-END-OF-FILE? CHANNEL) 
         (CALL-XENOID *CLEAR-TTYEOF-XENOID* CHANNEL)
         *EOF*)
        ((NOT (CHANNEL-INCURRED-ERROR? CHANNEL))
         (ERROR "utter strangeness in ~S reading ~S" RETRY CHANNEL))
        ((FX= (ERRNO) 4)                        ;   #define     EINTR   4
         (CALL-XENOID *CLEARERR-XENOID* (XENOID-POINTER CHANNEL))
         (RETRY CHANNEL))
        (ELSE
         ;; Hack differently when signalling system exists... some people
         ;; may be interested in dealing intelligently with i/o errors.
         (ERROR "i/o error - ~A~%  (~S ~S)"
                (UNIX-ERROR-STRING)
                (OR (IDENTIFICATION RETRY) RETRY)
                CHANNEL))))

(DEFINE (CHANNEL-UNREADC CHANNEL CHAR)  ; ungetc(c,stream)
  (CALL-XENOID *UNGETC-XENOID*
               (XENOID-POINTER CHANNEL)
               (CHAR->POINTER CHAR))
  T)

;;; fputc(c,stream)
(DEFINE (CHANNEL-WRITE-CHAR CHANNEL C)
  (GC-DEFER
   (CALL-XENOID *FPUTC-XENOID*
                (XENOID-POINTER CHANNEL)
                (CHAR->POINTER C))
   T))
(DEFINE CHANNEL-WRITEC CHANNEL-WRITE-CHAR)      ; ?

;;; fwrite(ptr,sizeof(*ptr),nitems,stream)
(DEFINE (CHANNEL-WRITE-STRING CHANNEL S)
  (GC-DEFER
   (CALL-XENOID *FWRITE-XENOID*
                (XENOID-POINTER CHANNEL)
                (FIXNUM->POINTER 1)
                (FIXNUM->POINTER (STRING-LENGTH S))
                (STRING-POINTER S))
   T))
(DEFINE CHANNEL-WRITES CHANNEL-WRITE-STRING)    ; ?

;;; fprintf(stream,"%d",num)
(DEFINE (CHANNEL-WRITE-FIXNUM CHANNEL N)
  (GC-DEFER
   (CALL-XENOID *FPRINTF-XENOID*
                (FIXNUM->POINTER N)
                (STRING-POINTER "%d")
                (XENOID-POINTER CHANNEL))
   T))

(DEFINE (CHANNEL-NEWLINE CHANNEL) (CHANNEL-WRITEC CHANNEL #\NEWLINE))

(CHANNEL-NEWLINE *TTYOUT-CHANNEL*)       ; Just for fun.

(DEFINE (CHANNEL? OBJ) (XENOID? OBJ))

(DEFINE (TTY-CHANNEL? CHANNEL)
  (FXN= (CALL-XENOID *ISATTY-XENOID*
                     (FIXNUM->POINTER (CHANNEL->FILDES CHANNEL)))
        0))

(LSET *PRINT-FLONUMS-KLUDGILY?* T)

;;; sprintf(ptr, "%E!", n)

(DEFINE (PRINT-FLONUM-KLUDGILY N STREAM)
  (LET ((B (GET-BUFFER-OF-SIZE 50)))
    (CALL-XENOID *SPRINTF-XENOID*
                 (FLONUM-HIGH-PESO N)
                 (FLONUM-LOW-PESO N)
                 (STRING-POINTER "%E!")
                 (STRING-POINTER B))
    (SET (STRING-LENGTH B) (%STRING-POSQ #\! B))
    (WRITE-STRING STREAM B)
    (RELEASE-BUFFER B)))

(DEFINE (STRING->FLONUM S)
  (CALL-XENOID-YIELDING-FLONUM *ATOF-XENOID* (STRING->ASCIZ S)))

(DEFINE (MAKE-MATH-PROC XENOID ID)
  (OBJECT (LAMBDA (X)
            (LET ((X (CHECK-ARG FLONUM? X ID)))
              (CALL-XENOID-YIELDING-FLONUM XENOID
                                           (FLONUM-HIGH-PESO X)
                                           (FLONUM-LOW-PESO X))))
          ((IDENTIFICATION SELF) ID)))

(DEFINE SIN  (MAKE-MATH-PROC *SIN-XENOID*  'SIN))
(DEFINE COS  (MAKE-MATH-PROC *COS-XENOID*  'COS))
(DEFINE TAN  (MAKE-MATH-PROC *TAN-XENOID*  'TAN))
(DEFINE ASIN (MAKE-MATH-PROC *ASIN-XENOID* 'ASIN))
(DEFINE ACOS (MAKE-MATH-PROC *ACOS-XENOID* 'ACOS))
(DEFINE ATAN (MAKE-MATH-PROC *ATAN-XENOID* 'ATAN))
(DEFINE EXP  (MAKE-MATH-PROC *EXP-XENOID*  'EXP))
(DEFINE LOG  (MAKE-MATH-PROC *LOG-XENOID*  'LOG))
(DEFINE SQRT (MAKE-MATH-PROC *SQRT-XENOID* 'SQRT))
;;; ... also need power and atan2
