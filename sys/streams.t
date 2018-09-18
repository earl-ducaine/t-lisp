(HERALD (TSYS STREAMS T 105)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; I/O stream randomness

(DEFINE *EOF*
  (OBJECT NIL
          ((PRINT SELF STREAM) (WRITES STREAM "#{End-of-file}"))))

;;; Default handlers

(DEFINE HANDLE-STREAM
  (%HANDLER #F
	    ((STREAM? SELF) T)
            ((PRINT-TYPE-STRING SELF) "Stream")))

(DEFINE HANDLE-INPUT-STREAM
  (%HANDLER #F
	    ((INPUT-STREAM? SELF) T)
	    ((PRINT-TYPE-STRING SELF) "Input-stream")
	    (=> HANDLE-STREAM)))

(DEFINE HANDLE-OUTPUT-STREAM
  (%HANDLER #F
	    ((OUTPUT-STREAM? SELF) T)
	    ((PRINT-TYPE-STRING SELF) "Output-stream")
	    (=> HANDLE-STREAM)))
           
;;; Stream hacks

(DEFINE (WITH-OPEN-STREAMS-HANDLER PROC . OPENERS)
  (LET ((STREAMS '()))
    (UNWIND-PROTECT (BLOCK (WALK (LAMBDA (OPENER) (PUSH STREAMS (OPENER)))
                                 ;; Careful - don't use MAP here!
                                 OPENERS)
                           ;; Thanks to Nat for the (SET STREAMS ...)
                           (APPLY PROC (SET STREAMS (REVERSE! STREAMS))))
                    (WALK (LAMBDA (STREAM)
                            ;; Deal with MAYBE-OPEN.
                            (IF STREAM (CLOSE STREAM)))
                          STREAMS))))

(DEFINE (MAKE-OUTPUT-TO-STRING-STREAM)
  (BUFFER->OUTPUT-STREAM (GET-BUFFER)))

(DEFINE (BUFFER->OUTPUT-STREAM BUFFER)
  (OPEN (OBTAIN-FROM-POOL *BUFFER-OUTPUT-STREAM-POOL*) BUFFER))

(DEFINE (GEN-BUFFER-OUTPUT-STREAM)
  (LET ((BUFFER NIL))
    (OBJECT NIL
            ((WRITEC SELF CHAR)   (BUFFER-WRITEC BUFFER CHAR))
            ((WRITES SELF STRING) (BUFFER-WRITES BUFFER STRING))
            ((HPOS SELF) (STRING-LENGTH BUFFER))
            ((LINE-LENGTH SELF) *MAX-FIXNUM*)
            ((OPEN SELF KLUDGE)
             (SET BUFFER (CHECK-ARG STRING? KLUDGE OPEN))
             SELF)
            ((CLOSE SELF)
             (LET ((S (COPY-STRING BUFFER)))
               (RELEASE-BUFFER BUFFER)  ; this is a little weird
               (SET BUFFER NIL)         ; extra security
               (RETURN-TO-POOL *BUFFER-OUTPUT-STREAM-POOL* SELF)
               S))
            (=> HANDLE-OUTPUT-STREAM))))

(DEFINE *BUFFER-OUTPUT-STREAM-POOL*
  (MAKE-POOL '*BUFFER-OUTPUT-STREAM-POOL* GEN-BUFFER-OUTPUT-STREAM))

;;; blah blah...

(DEFINE (MAKE-OUTPUT-WIDTH-STREAM)
  (OPEN (OBTAIN-FROM-POOL *OUTPUT-WIDTH-STREAM-POOL*) 0))

(DEFINE (GEN-OUTPUT-WIDTH-STREAM)
  (LET ((COUNT 0))
    (OBJECT NIL
            ((WRITEC SELF CHAR) (IGNORE CHAR) (SET COUNT (FX+ COUNT 1)))
            ((WRITES SELF STRING)
             (SET COUNT (FX+ COUNT (STRING-LENGTH STRING))))
            ((CLOSE SELF)
             (RETURN-TO-POOL *OUTPUT-WIDTH-STREAM-POOL* SELF)
             COUNT)
            ((OPEN SELF KLUDGE)
             (SET COUNT KLUDGE)
             SELF)
            (=> HANDLE-OUTPUT-STREAM))))

(DEFINE *OUTPUT-WIDTH-STREAM-POOL*
  (MAKE-POOL '*OUTPUT-WIDTH-STREAM-POOL* GEN-OUTPUT-WIDTH-STREAM))

;;; Hack for pretty-printer.

(DEFINE (PRINT-WIDTH-GREATER? OBJ N)
  (CATCH ABORT
         (LET ((ABORT (NO-OP ABORT))    ; kludge around complr bugs
               (COUNT 0))
           (PRINT OBJ
                  (OBJECT NIL
                          ((WRITEC SELF CHAR)
                           (IGNORE CHAR)
                           (SET COUNT (FX+ COUNT 1))
                           (IF (FX> COUNT N) (ABORT T)))
                          ((WRITES SELF STRING)
                           (SET COUNT (FX+ COUNT (STRING-LENGTH STRING)))
                           (IF (FX> COUNT N) (ABORT T)))
                          (=> HANDLE-OUTPUT-STREAM)))
           NIL)))

;;; Hack for CRAWL.

(DEFINE (PRINT-ONE-LINE OBJ STREAM)
  (CATCH ABORT
         (LET ((ABORT (NO-OP ABORT)))
           (PRINT OBJ
                  (OBJECT NIL
                          ((WRITEC SELF CHAR) (WRITEC STREAM CHAR))
                          ((WRITES SELF STRING) (WRITES STREAM STRING))
                          ((HPOS SELF) (HPOS STREAM))
                          ((NEWLINE SELF)       ; Called from SPACE
                           (WRITES SELF " ---")
                           (ABORT NIL)))))))


;;; Hack for FORMAT.

(LET ((+SPACES+ (STRING-FILL (MAKE-STRING 100.) #\SPACE)))

(DEFINE (WRITE-SPACES STREAM N)         ; maybe should be operation?
  (DO ((I N (FX- I (STRING-LENGTH +SPACES+))))
      ((FX<= I (STRING-LENGTH +SPACES+))
       (WRITES STREAM
               (STRING-SLICE +SPACES+ 0 I)))    ; too bad this conses.
    (WRITES STREAM +SPACES+)))
)

;;; (STRING->INPUT-STREAM string) returns an input stream which, on successive
;;;  READC's, yields successive characters of the string.
;;; Maybe there should be a pool of these input streams, and a stream could
;;;  recycle itself when it got closed.
;;; Maybe we should have WITH-INPUT-FROM-STRING.

(DEFINE (STRING->INPUT-STREAM STRING)
  (LET ((S (CHOPY (CHECK-ARG STRING? STRING STRING->INPUT-STREAM)))
        (BUFFERED-CHAR)
        (BUFFERED-CHAR? NIL)
        (RT *STANDARD-READ-TABLE*))
    (OBJECT NIL
            ((READC SELF) (COND (BUFFERED-CHAR?
                                 (SET BUFFERED-CHAR? NIL)
                                 BUFFERED-CHAR)
                                ((STRING-EMPTY? S)
                                 (SET BUFFERED-CHAR *EOF*))
                                (ELSE
                                 (SET BUFFERED-CHAR (CHAR S))
                                 (CHDR! S)
                                 BUFFERED-CHAR)))
            ((UNREADC SELF) (SET BUFFERED-CHAR? T))
            ((PEEKC SELF) (COND (BUFFERED-CHAR? BUFFERED-CHAR)
                                ((STRING-EMPTY? S) *EOF*)
                                (ELSE (CHAR S))))
            ((CLEAR-INPUT SELF)
             (SET BUFFERED-CHAR? NIL))
            ((CLOSE SELF) (SET S ""))
            ((STREAM-READ-TABLE SELF) RT)
            ((SET-STREAM-READ-TABLE SELF NEW) (SET RT NEW))
            ((PRINT-TYPE-STRING SELF) "String-input-stream")
            (=> HANDLE-INPUT-STREAM))))

(DEFINE (READ-OBJECTS-FROM-STRING STRING)
  (LET ((Z (STRING->INPUT-STREAM STRING)))
    (ITERATE LOOP ((L '()))
      (LET ((OBJ (READ Z)))
        (IF (EOF? OBJ)
            (REVERSE! L)
          (LOOP (CONS OBJ L)))))))

(DEFINE (MAKE-BROADCAST-STREAM . STREAMS)
  (OBJECT NIL
          ((WRITEC SELF CH)
           ;; (WALK (LAMBDA (STREAM) (WRITEC STREAM CH)) STREAMS)
           (DO ((S STREAMS (CDR S)))
               ((NULL? S) T)
             (WRITEC (CAR S) CH)))
          ((WRITES SELF STRING)
           ;; (WALK (LAMBDA (STREAM) (WRITES STREAM STRING)) STREAMS)
           (DO ((S STREAMS (CDR S)))
               ((NULL? S) T)
             (WRITES (CAR S) STRING)))
          ((HPOS SELF) (HPOS (CAR STREAMS)))
          ((SET-HPOS SELF POS)
           (WALK (LAMBDA (STREAM) (SET-HPOS STREAM POS)) STREAMS))
          ((NEWLINE SELF)
           (DO ((S STREAMS (CDR S)))
               ((NULL? S) T)
             (NEWLINE (CAR S))))
          ((FRESH-LINE SELF)
           (DO ((S STREAMS (CDR S)))
               ((NULL? S) T)
             (FRESH-LINE (CAR S))))
          ((PRINT SELF STREAM)
           (FORMAT STREAM "#{Broadcast-stream~_~S}" STREAMS))
          (=> HANDLE-OUTPUT-STREAM)))

;;; This is kind of yicky also.

(DEFINE (MAKE-ECHO-STREAM ISTREAM OSTREAM)
  (LET ((BUFFERRED-CHAR)
        (BUFFERRED-CHAR? NIL))
    (OBJECT NIL
            ((READC SELF)
             (COND (BUFFERRED-CHAR?
                    (SET BUFFERRED-CHAR? NIL)
                    BUFFERRED-CHAR)
                   (ELSE
                    (LET ((C (READC ISTREAM)))
                      (WRITEC OSTREAM C)
                      (SET BUFFERRED-CHAR C)
                      C))))
            ((UNREADC SELF)
             (SET BUFFERRED-CHAR? T))
            ((STREAM-READ-TABLE SELF) (STREAM-READ-TABLE ISTREAM))
            ((SET-STREAM-READ-TABLE SELF NEW)
             (SET-STREAM-READ-TABLE ISTREAM NEW))
            ((CLEAR-INPUT SELF)
             (SET BUFFERRED-CHAR? NIL))
            (=> HANDLE-INPUT-STREAM))))

;;; Hack for LOAD-TRANSDUCE.

(DEFINE (CONS-STREAM OBJ STREAM)
  (LET ((FLAG NIL))
    (OBJECT NIL
	    ((READ SELF)
	     (COND (FLAG (READ STREAM))
		   (ELSE (BLOCK0 OBJ (SET FLAG T) (SET OBJ NIL)))))
	    ((STREAM-FILENAME SELF)
	     (STREAM-FILENAME STREAM))
	    (=> HANDLE-STREAM))))

;;; Hack for no apparent reason.

(DEFINE (EMPTY-STREAM)
  (OBJECT NIL
	  ((READ SELF) *EOF*)
	  (=> HANDLE-STREAM)))

;;; Transcript facility.

(LSET +TRANSCRIPT+ NIL)
(LSET +TERMINAL-INPUT+ NIL)
(LSET +TERMINAL-OUTPUT+ NIL)

(DEFINE (TRANSCRIPT-ON FILENAME)
  (COND (+TRANSCRIPT+
         (FORMAT (TERMINAL-OUTPUT)
                 ";Transcript file ~A is already open.~%"
                 (STREAM-FILENAME +TRANSCRIPT+))
         NIL)
        (ELSE
         (SET +TRANSCRIPT+ (OPEN FILENAME '(OUT)))
         (FORMAT +TRANSCRIPT+ ";;; T transcript file ~A~%~%" FILENAME)
         (SET +TERMINAL-INPUT+ (TERMINAL-INPUT))
         (SET +TERMINAL-OUTPUT+ (TERMINAL-OUTPUT))
         (SET (TERMINAL-INPUT)
              (MAKE-ECHO-STREAM (TERMINAL-INPUT) +TRANSCRIPT+))
         (SET (TERMINAL-OUTPUT)
              (MAKE-BROADCAST-STREAM (TERMINAL-OUTPUT) +TRANSCRIPT+))
         'OK)))

(DEFINE (TRANSCRIPT-OFF)
  (COND (+TRANSCRIPT+
         (SET (TERMINAL-OUTPUT) +TERMINAL-OUTPUT+)
         (SET (TERMINAL-INPUT)  +TERMINAL-INPUT+)
         (FORMAT +TRANSCRIPT+ "~&~%;;; End of transcript file~%")
         (CLOSE +TRANSCRIPT+)
         (SET +TRANSCRIPT+ NIL)
         (SET +TERMINAL-INPUT+ NIL)
         (SET +TERMINAL-OUTPUT+ NIL)
         'OK)
        (ELSE NIL)))


;;; This doesn't really belong here, but it's really hard to say just where it
;;; DOES belong.

(DEFINE (CONCATENATE-SYMBOL . THINGS)
  (LET ((B (GET-BUFFER)))
    (LET ((S (BUFFER->OUTPUT-STREAM B)))
      (DO ((Z THINGS (CDR Z)))
          ((NULL? Z)
           (BLOCK0 (STRING->SYMBOL B)
                   (CLOSE S)))
        (DISPLAY (CAR Z) S)))))

