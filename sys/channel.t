(HERALD (TSYS CHANNEL T 61)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Channel interface

;;; This module defines ways to create I/O streams out of the more primitive
;;; "channel" abstraction.  Channels are presumably provided at a low level
;;; by operating-system dependent routines, whereas this interface should be
;;; relatively independent of that sort of hair.

;;; Given a string (!) naming a local file, return a stream for it.

(DEFINE (MAYBE-OPEN-LOCAL FS FNAME MODES)
  (IGNORE FS)
  (LET ((FOO (LAMBDA (COERCER)
               (LET ((CHANNEL (CHANNEL-OPEN-FILENAME FNAME MODES)))
                 (COND (CHANNEL
                        (LET ((STREAM (COERCER CHANNEL FNAME)))
                          (SET-TABLE-ENTRY *OPEN-CHANNEL-TABLE*
                                           CHANNEL
                                           (OBJECT-HASH STREAM))
                          STREAM))
                       (ELSE NIL))))))
    (COND ((MEMQ? 'IN MODES)
           (FOO CHANNEL->INPUT-STREAM))
          ((OR (MEMQ? 'OUT MODES)
               (MEMQ? 'APPEND MODES))
           (FOO CHANNEL->OUTPUT-STREAM))
          (ELSE
           (MAYBE-OPEN-LOCAL FS FNAME
                             (ERROR "(~S ~S ~S): bad mode spec"
                                    'OPEN FNAME MODES))))))

;;; Future plans include:
;;;  bufferred i/o; update mode; re-openability; seeking & telling.

(DEFINE (CHANNEL->INPUT-STREAM CHANNEL FNAME)
  (LET ((BUFFERED-CHAR)
        (BUFFERED-CHAR? NIL)
        (H 0)
        (V 0)
        (RT *STANDARD-READ-TABLE*))
    (OBJECT NIL
            ((READ-CHAR SELF)
             (COND (BUFFERED-CHAR?
                    (SET BUFFERED-CHAR? NIL))
                   ((NOT CHANNEL)
                    (CLOSED-CHANNEL-LOSSAGE READ-CHAR SELF))
                   (ELSE
                    (LET ((C (CHANNEL-READC CHANNEL)))
                      (COND ((CHAR= C #\NEWLINE)
                             (SET V (FX+ V 1))
                             (SET H 0))
                            (ELSE (SET H (FX+ H 1))))
                      (SET BUFFERED-CHAR C))))
             BUFFERED-CHAR)

            ((UNREAD-CHAR SELF)
             (SET BUFFERED-CHAR? T))
            ((STREAM-READ-TABLE SELF) RT)
            ((SET-STREAM-READ-TABLE SELF NEW-READ-TABLE)
             (SET RT NEW-READ-TABLE))
            ((READ-LINE SELF)
             ;; With JRM's fix.
             (SET V (FX+ V 1))         ; grossmeout [sic]
             ;; (CHANNEL-READ-LINE CHANNEL)    <-- Old code. New code follows:
             (set h 0)         ; Set HPOS (consistent with READC).
             (cond ((NOT CHANNEL)
                    (CLOSED-CHANNEL-LOSSAGE READ-LINE SELF))
                   (buffered-char?
                    (set buffered-char? nil)
                    (cond ((char= buffered-char #\newline)
                           (make-string 0))    ; Is it safe to use "" here?
                          (else (string-append         ; *sigh*
                                  (char->string buffered-char)
                                  (channel-read-line channel)))))
                   (else (channel-read-line channel))))
            ((INTERACTIVE-STREAM? SELF) (TTY-CHANNEL? CHANNEL))
            ((CLEAR-INPUT SELF) (SET BUFFERED-CHAR? NIL))
            ;; ((CLEAR-INPUT SELF) (CHANNEL-CLEAR-INPUT CHANNEL)) ?
            ((VPOS SELF)        V)
            ((HPOS SELF)        H)
            ((STREAM-OPEN? SELF) (TRUE? CHANNEL))
            ((CLOSE SELF)
             (COND ((EQ? CHANNEL *TTYIN-CHANNEL*)
                    (ERROR "attempt to close terminal input stream"))
                   (CHANNEL
                    (BLOCK0 (CLOSE-STREAM-CHANNEL SELF CHANNEL)
                            (SET CHANNEL NIL)))))
            ((PRINT SELF STREAM)
             (FORMAT STREAM "#{Input-stream~_~S~_~S}" (OBJECT-HASH SELF) FNAME))
            ((STREAM->CHANNEL SELF) CHANNEL)
            ((STREAM-FILENAME SELF) FNAME)
            (=> HANDLE-INPUT-STREAM))))

(DEFINE (CHANNEL->OUTPUT-STREAM CHANNEL FNAME)
  (LET ((H 0)
        (V 0)
        (RT *STANDARD-READ-TABLE*))
    (OBJECT NIL
            ((WRITE-CHAR SELF CHAR)
             (SET H (FX+ H 1)) ;but what about tab?
             (IF (NOT CHANNEL)
                 (CLOSED-CHANNEL-LOSSAGE WRITE-CHAR SELF)
                 (CHANNEL-WRITEC CHANNEL CHAR)))
            ((WRITE-STRING SELF STRING)
             (SET H (FX+ H (STRING-LENGTH STRING)))
             (IF (NOT CHANNEL)
                 (CLOSED-CHANNEL-LOSSAGE WRITE-STRING SELF)
                 (CHANNEL-WRITES CHANNEL STRING)))
            ((FORCE-OUTPUT SELF)  (CHANNEL-FORCE-OUTPUT CHANNEL))
            ((NEWLINE SELF)       (CHANNEL-NEWLINE CHANNEL)
                                  (SET H 0)
                                  T)
            ((STREAM-READ-TABLE SELF) RT)       ; ?!
            ((SET-STREAM-READ-TABLE SELF NEW-READ-TABLE)
             (SET RT NEW-READ-TABLE))
            ((HPOS SELF)          H)
            ((VPOS SELF)          V)
            ((INTERACTIVE-STREAM? SELF) (TTY-CHANNEL? CHANNEL))
            ((STREAM-OPEN? SELF) (TRUE? CHANNEL))
            ((CLOSE SELF)
             (COND ((EQ? CHANNEL *TTYOUT-CHANNEL*)       ; kludge
                    (ERROR "attempt to close terminal output stream"))
                   (CHANNEL
                    (BLOCK0 (CLOSE-STREAM-CHANNEL SELF CHANNEL)
                            (SET CHANNEL NIL)))))
            ((PRINT SELF STREAM)
             (FORMAT STREAM "#{Output-stream~_~S~_~S}"
                     (OBJECT-HASH SELF) FNAME))
            ((STREAM->CHANNEL SELF) CHANNEL)
            ((STREAM-FILENAME SELF) FNAME)
            (=> HANDLE-OUTPUT-STREAM))))

(DEFINE (CLOSED-CHANNEL-LOSSAGE . REST)
  (ERROR "attempt to perform I/O operation on a closed stream~%  ~S"
         REST))

(LSET *THE-TERMINAL-INPUT-STREAM*
  (CHANNEL->INPUT-STREAM  *TTYIN-CHANNEL*  "terminal input"))

(LSET *THE-TERMINAL-OUTPUT-STREAM*
  (CHANNEL->OUTPUT-STREAM *TTYOUT-CHANNEL* "terminal output"))

;;; GC hook: arrange to close open channels for streams to which there are no
;;;  pointers.

(LSET *OPEN-CHANNEL-TABLE* (MAKE-TABLE '*OPEN-CHANNEL-TABLE*))

;;; Any channel for which there is no longer a stream must get closed.

(DEFINE (GC-CLOSE-UNREFERENCED-CHANNELS)
  (WALK-TABLE *OPEN-CHANNEL-TABLE*
              (LAMBDA (CHANNEL H)
                (COND ((NOT (OBJECT-UNHASH H))
                       (CLOSE-STREAM-CHANNEL NIL CHANNEL)
                       (IF *GC-NOISILY?*
                           (GC-FORMAT GC-OUTPUT ";Stream closed: ~S~%"
                                      CHANNEL)))))))

;;; If *POST-GC-AGENDA* doesn't have at least one element, then we're really
;;; losing.

(APPEND! *POST-GC-AGENDA*
         (LIST (CONS 'GC-CLOSE-UNREFERENCED-CHANNELS
                     GC-CLOSE-UNREFERENCED-CHANNELS)))

;;; Close the channel associated with a channel-stream.

(DEFINE (CLOSE-STREAM-CHANNEL STREAM CHANNEL)
  (IGNORE STREAM)           ;Why is this passed at all?
  (BLOCK0 (CHANNEL-CLOSE CHANNEL)
          (SET-TABLE-ENTRY *OPEN-CHANNEL-TABLE* CHANNEL NIL)))
