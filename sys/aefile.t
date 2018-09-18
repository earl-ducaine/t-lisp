(HERALD AEFILE
        (PRE-COOK)
        (ENV TSYS (TSYS AEGIS)))

;;; Copyright (c) 1983, 1984 Yale University

;;;; AEGIS file manipulation

;;; Local macros replicated from AEGIS.T

(DEFINE-LOCAL-SYNTAX (DEFINE-WITH-STATUS PAT STS . REST)
  (DESTRUCTURE (((PROC . ARGS) PAT))
    `(DEFINE ,PROC
              (LET ((,STS (MAKE-XENOID 0)))
                (OBJECT (NAMED-LAMBDA ,PROC ,ARGS ,@REST)
                        ((LAST-STATUS SELF) ,STS))))))

(DEFINE-LOCAL-SYNTAX (ENUMERATED TYPE VALUES)
  (DO ((I 0 (FX+ I 1))
       (VALUES VALUES (CDR VALUES))
       (RESULT '() (CONS `(DEFINE-CONSTANT ,(CAR VALUES) ,I) RESULT)))
      ((NULL? VALUES) `(BLOCK 
                        (SET ,TYPE ,VALUES)
                        ,@(REVERSE! RESULT)))))

;;; Horrible hacks for terminal i/o.  Assume for now same as error i/o.

(DEFINE *TTYIN-CHANNEL*  *STDERRIN-XENOID*)
(DEFINE *TTYOUT-CHANNEL* *STDERR-XENOID*)

;;; The EOF token,  & a predicate for it.
(DEFINE *EOF* (OBJECT NIL ((PRINT SELF STREAM) (FORMAT STREAM "#{Eof}"))))

;;; Predicate used by CLOSE (?)

(DEFINE (CHANNEL? OBJ) (XENOID? OBJ))

;;; Used by FS stuff to create local file system access.

(define (local-fs-creator) make-aegis-fs)

;;; Permit redundant close-parens.  See CHANNEL->INPUT-STREAM.

(define-with-status (tty-channel? channel) sts
  (fixnum-odd? (call-xenoid 'data 'fixnum *stream_$isavt-xenoid* sts channel)))

;;; (FILE-CREATE path) - returns a UID (bytev 8) or NIL

(DEFINE-CONSTANT NAME_$ALREADY_EXISTS #X+E0003)

(DEFINE-WITH-STATUS (NAME-CR-FILE PATH) STS
  (LET ((UID (MAKE-UID)))
    (CALL-XENOID NIL NIL *NAME_$CR_FILE-XENOID*
      STS UID #\s PATH)
    (COND ((NOT (ZERO-XENOID? STS))
           (ERROR "(NAME-CR-FILE ~S) - failed" PATH))
          (ELSE UID))))

;;; (FILE-DELETE path) - returns T/NIL

(DEFINE-WITH-STATUS (NAME-DELETE-FILE PATH) STS
  (CALL-XENOID NIL NIL *NAME_$DELETE_FILE-XENOID*
               STS #\s PATH)
  (ZERO-XENOID? STS))
      
;;; (MS-MAP path length[pointer] offset[fixnum] 
;;;   (lambda (mapped-at[pointer] mapped-length[pointer])
;;;     ...))

(DEFINE-WITH-STATUS (MS-MAP PATH LENGTH OFFSET CONT) STS
  (LET ((MAPPED-AT (MAKE-XENOID 0))
        (MAPPED-LENGTH (MAKE-XENOID 0)))
    (CALL-XENOID 'ADDR MAPPED-AT *MS_$MAPL-XENOID*
      OFFSET                            ; position in object
      LENGTH                            ; how much to map
      MS_$NR_XOR_1W                     ; concurrency
      MS_$WRX                           ; access  
      -1                                ; extend OK = TRUE
      STS MAPPED-LENGTH #\w #\w #\w #\p #\l #\s PATH)
    (COND ((ZERO-XENOID? STS)
           (CONT (XENOID-POINTER MAPPED-AT) (XENOID-POINTER MAPPED-LENGTH)))
          (ELSE 
           (ERROR "(MS-MAP ~S ~S ~S ~S) - failed" PATH LENGTH OFFSET CONT)))))

(ENUMERATED MS_$CONC_MODE_T (MS_$NR_XOR_1W
                             MS_$COWRITERS))

(ENUMERATED MS_$ACC_MODE_T (MS_$R
                            MS_$RX
                            MS_$WR
                            MS_$WRX))

;;; (MS-UNMAP address[pointer] length[pointer]) - return true or false

(DEFINE-WITH-STATUS (MS-UNMAP ADDRESS LENGTH) STS
  (CALL-XENOID NIL NIL *MS_$UNMAP-XENOID*
    ADDRESS LENGTH 
    STS #\p #\p)
  (ZERO-XENOID? STS))

(DEFINE-WITH-STATUS (FILE-TRUNCATE UID LEN) STS
  (CALL-XENOID NIL NIL *FILE_$TRUNCATE-XENOID* LEN STS #\p UID)
  (ZERO-XENOID? STS))

;;; Generate an obsure (and probably unique) pathname.

(define (gen-pathname prefix)
  (uid-gen (lambda (x1 x2 x3 x4)
             (format nil "~a~x~x.~x~x" prefix x1 x2 x3 x4))))

(ENUMERATED STREAM_$PARM1_T (STREAM_$KEY STREAM_$REC STREAM_$CHR STREAM_$EOF))
(ENUMERATED STREAM_$PARM2_T (STREAM_$RELATIVE STREAM_$ABSOLUTE))

(ENUMERATED STREAM_$OPOS_T 
  (STREAM_$READ STREAM_$WRITE STREAM_$OVERWRITE STREAM_$UPDATE STREAM_$APPEND))

(ENUMERATED STREAM_$OMODE_T
  (STREAM_$NO_CONC_WRITE STREAM_$CONTROLLED_SHARING STREAM_$UNREGULATED))

;;(enumerated stream_$ir_opt
;;  (stream_$use_strid stream_$name_conditional stream_$name_unconditional))

;;; Open up a channel -- sets MOVE mode by default.

(DEFINE-WITH-STATUS (CHANNEL-OPEN NAME MODES) STS
  (GC-DEFER
   (LET ((ACCESS (CONVERT-MODES MODES))
         (STRID (MAKE-XENOID 0)))
     (LET ((OPENER (IF (MEMQ? 'IN MODES) 
                       *STREAM_$OPEN-XENOID*
                     *STREAM_$CREATE-XENOID*)))
       (CALL-XENOID NIL NIL OPENER
         ACCESS STREAM_$NO_CONC_WRITE STS STRID #\w #\w #\s NAME)
       (COND ((NOT (ZERO-XENOID? STS)) NIL)
             ((MEMQ? 'LOCATE MODES) STRID)
             (ELSE (CHANNEL-SET-MOVE-MODE STRID) STRID))))))

(DEFINE (CONVERT-MODES MODES)
  (COND ((MEMQ? 'IN     MODES) STREAM_$READ)
        ((MEMQ? 'OUT    MODES) STREAM_$OVERWRITE)
        ((MEMQ? 'APPEND MODES) STREAM_$APPEND)
        (ELSE (ERROR "bad mode list~%  (~S ~S)" 'CONVERT-MODES MODES)
	      STREAM_$READ)))

(DEFINE-WITH-STATUS (CHANNEL-SET-MOVE-MODE CHANNEL) STS
  (CALL-XENOID 'DATA STS *T_$SET_MOVE-XENOID* CHANNEL)
  (ZERO-XENOID? STS))

;;; Open a file, given a filename for it.
;;; This hacks generation number stuff, if appropriate.

(define (channel-open-filename fname modes)
  (let ((name (filename->string fname))
        (desired-gen (filename-generation fname)))
    (cond ((null? desired-gen)
           (cond ((memq? 'out modes)
                  (increment-generation-number fname)))
           (channel-open name modes))
          (else
           (cond ((memq? 'out modes)
                  ;; Output: clobber generation number to be that given.
                  (set-generation-number fname desired-gen)
                  (channel-open name modes))
                 (else
                  ;; Input, append: complain if what's there isn't what's wanted
                  (cond ((fx= desired-gen (get-generation-number fname))
                         (channel-open name modes)))))))))

;;; Probe file existence.

(let ((uid (make-uid)))

(define (local-file-exists? fname)
  (true? (name-resolve! (filename->string (->filename fname)) uid))))
       
(define (local-probe-generation fname)
  (cond ((local-file-exists? fname)
         (get-generation-number fname))
        (else nil)))

;;; Generation number manipulation

(define (get-generation-number fname)
  (let ((uid (name-resolve (generation-name fname))))
    (if uid (bref-32 uid 0) 0)))

(define (increment-generation-number fname)
  (set-generation-number fname nil))

;;; GEN should be either a fixnum or nil; nil means to increment whatever's
;;; there.  Value returned is irrelevant.

(define (set-generation-number fname gen)
  (let ((name (generation-name fname)))
    (let ((uid (name-drop name)))
      (name-add name
                (cond (uid
                       (set (bref-32 uid 0)
                            (or gen (fx+ (bref-32 uid 0) 1)))
                       uid)
                      (else
                       (let ((uid (make-uid)))
                         (set (bref-32 uid 0) (or gen 1))
                         uid)))))))

;;; Return string for name in Aegis file system under which the file's
;;; generation number is catalogued.

;;; This could probably benefit from a little caching.

(define (generation-name fname)
  (filename->string (make-filename nil
                                   (filename-dir fname)
                                   (format nil "generation_dir/~a"
                                           (filename-name fname))
                                   (filename-type fname))))

;;; Naming server calls:

;;; NAME-ADD - returns true if success.

(define-with-status (name-add name uid) status
  (call-xenoid nil nil *name_$add-xenoid* status uid #\s name)
  (zero-xenoid? status))

;;; NAME-DROP - returns UID of dropped name, if any.

(define-with-status (name-drop name) status
  (let ((uid (make-uid)))
    (call-xenoid nil nil *name_$drop-xenoid* status uid #\s name)
    (if (zero-xenoid? status) uid nil)))

;;; NAME-RESOLVE - return UID or false.

(define (name-resolve pathname)
  (name-resolve! pathname (make-uid)))

(define-with-status (name-resolve! pathname uid) sts
  (call-xenoid nil nil *name_$resolve-xenoid* sts uid #\s pathname)
  (if (zero-xenoid? sts) uid nil))


;;; Close a stream.

(DEFINE-WITH-STATUS (CHANNEL-CLOSE STRID) STS
  (GC-DEFER
   (CALL-XENOID NIL NIL *STREAM_$CLOSE-XENOID* STS STRID)
   (ZERO-XENOID? STS)))

;;; This is not the right thing -- null WRITESs will terminate the
;;; current record,  often that should cause a flushbuf -- but what
;;; it we didn't want to terminate the damn record?

(DEFINE (CHANNEL-FORCE-OUTPUT CHANNEL)
  (GC-DEFER (CHANNEL-WRITES CHANNEL "")))

;;; Read a "line" from a file (a "line" from the printed representation)

(DEFINE +SEEK-KEY+ (MAKE-BYTEV 12))    
(DEFINE +BUFFER+ (MAKE-XENOID 0))
(DEFINE +RETPTR+ (MAKE-XENOID 0))
(DEFINE +RETLEN+ (MAKE-XENOID 0))

(DEFINE (CHANNEL-READ-LINE CHANNEL)
  (LET ((B (%CHANNEL-READ-LINE CHANNEL (GET-BUFFER))))
    (IF (EOF? B) B
      (LET ((L (STRING-LENGTH B)))
        (IF (AND (FX> L 0) (CHAR= (NTHCHAR B (FX- L 1)) #\NEWLINE))
            (SET (STRING-LENGTH B) (FX- L 1)))
        (BLOCK0 (COPY-STRING B)
                (RELEASE-BUFFER B))))))

;;; Read a "line" into the given buffer.  Extend the buffer if necessary.
;;;  (ASSERT (=0? (STRING-BASE BUFFER)))

(DEFINE-WITH-STATUS (%CHANNEL-READ-LINE CHANNEL BUFFER) STS
  (LET ((SIZE (BUFFER-SIZE BUFFER)))
    (CALL-XENOID NIL NIL *STREAM_$GET_REC-XENOID*
      BUFFER SIZE
      STS +SEEK-KEY+ +RETLEN+ +RETPTR+ #\l #\p CHANNEL)
    (LET ((LEN (XENOID->FIXNUM +RETLEN+)))
      (COND ((FXN= (XENOID->FIXNUM STS) 0) *EOF*)
            ((NEQ? (XENOID-POINTER +RETPTR+) (STRING-POINTER BUFFER))
             (ERROR "stream not in move mode")  ; Very gross problem.
             *EOF*)
            ((FX>= LEN 0)
             (SET (STRING-LENGTH BUFFER) LEN)
             BUFFER)
            ;; Extend buffer and re-read to get remainder of record.
            ;; (LEN is negative so SIZE minus LEN is greater than SIZE.)
            (ELSE
	     (LET ((FOO (FX- SIZE LEN)))
	       (SET (STRING-LENGTH BUFFER) SIZE)
	       (ENSURE-BUFFER-SIZE BUFFER FOO)
	       (SET (STRING-LENGTH BUFFER) FOO))
	     (CALL-XENOID NIL NIL *STREAM_$GET_REC-XENOID*
			  (NTHCHDR BUFFER SIZE) (FX- 0 LEN)
			  STS +SEEK-KEY+ +RETLEN+ +RETPTR+ #\l #\p CHANNEL)
	     BUFFER)))))

;;; lowest level IO ... READC, WRITEC, and WRITES All routines that do reading,
;;; except CHANNEL-READC, depend on the stream being in MOVE MODE

(DEFINE-WITH-STATUS (CHANNEL-READC CHANNEL) STS
  (CALL-XENOID NIL NIL *STREAM_$GET_BUF-XENOID*
    +BUFFER+ 1
    STS +SEEK-KEY+ +RETLEN+ +RETPTR+ #\l #\p CHANNEL)
  (COND ((FXN= (XENOID->FIXNUM STS) 0) *EOF*)
        (ELSE
	 (ASCII->CHAR 
	  (FIXNUM-LOGAND ((PRIMOP BYTEV-ELT-8) (XENOID-POINTER +RETPTR+) 0)
			 255)))))

(DEFINE-WITH-STATUS (CHANNEL-WRITEC CHANNEL CH) STS
  (LET ((WRITER (IF (EQ? CH #\NEWLINE)
                    *STREAM_$PUT_REC-XENOID*
                  *STREAM_$PUT_CHR-XENOID*)))
    (SET-XENOID-POINTER +BUFFER+ (POINTER-ASHL (CHAR->ASCII CH) 21))
    (CALL-XENOID NIL NIL WRITER
      +BUFFER+ 1
      STS +SEEK-KEY+ #\l #\p CHANNEL)
    (ZERO-XENOID? STS)))

(DEFINE-WITH-STATUS (CHANNEL-WRITES CHANNEL STRING) STS
  (CALL-XENOID NIL NIL *STREAM_$PUT_CHR-XENOID*
    STRING (STRING-LENGTH STRING)
    STS +SEEK-KEY+ #\l #\p CHANNEL)
  (ZERO-XENOID? STS))

;;; This will terminate a record according to Apollo standard

(DEFINE (CHANNEL-NEWLINE CHANNEL) 
  (GC-DEFER (CHANNEL-WRITEC CHANNEL #\NEWLINE))) 

(CHANNEL-NEWLINE *TTYOUT-CHANNEL*)       ; Just for fun.

(DEFINE-WITH-STATUS (CHANNEL-SEEK CHANNEL POSITION) STS
  (CALL-XENOID NIL NIL *STREAM_$SEEK-XENOID*
    STREAM_$CHR STREAM_$ABSOLUTE (FX+ POSITION 1)
    STS #\l #\w #\w CHANNEL)
  (ZERO-XENOID? STS))

(DEFINE-WITH-STATUS (CHANNEL-SEEK-RELATIVE CHANNEL OFFSET) STS
  (CALL-XENOID NIL NIL *STREAM_$SEEK-XENOID*
    STREAM_$CHR STREAM_$RELATIVE OFFSET
    STS #\l #\w #\w CHANNEL)
  (ZERO-XENOID? STS))

;;; Read BYTE-COUNT bytes into ADDRESS (that is a real machine address).
;;; e.g. (SET X (MAKE-XENOID 0)) (CHANNEL-READ C X 4) 
;;;        reads 4 bytes from the apollo stream, C, into the xenoid.
;;; Because CALL-XENOID is hacked, passing a string as ADDRESS will
;;; result in bytes being read into the string text - you probably
;;; want to use a bytev though - they work too.

(DEFINE-WITH-STATUS (CHANNEL-READ CHANNEL ADDRESS BYTE-COUNT) STS
  (CALL-XENOID NIL NIL *STREAM_$GET_BUF-XENOID*
    ADDRESS BYTE-COUNT
    STS +SEEK-KEY+ +RETLEN+ +RETPTR+ #\l #\p CHANNEL)
  (COND ((AND (ZERO-XENOID? STS)
              (FX= BYTE-COUNT (XENOID->FIXNUM +RETLEN+)))
         T)
        (ELSE (ERROR "CHANNEL-READ failed"))))
