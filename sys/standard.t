(HERALD (TSYS STANDARD T 31)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Standard I/O streams

;;; E.g. (READ (STANDARD-INPUT))
;;; (BIND (((TERMINAL-INPUT) FOO-STREAM)) ...)

;;; Bugs: There are no TERMINAL-IO or STANDARD-IO streams.

;;; "Switch-streams" (horrible name, but that's how they go now) are sort of
;;; like synonyms, sort of like switches... pretty yucky.

(DEFINE-LOCAL-SYNTAX (DEFINE-STREAM-SWITCH SWITCH-VAR STREAM-VAR INITIAL-VALUE)
  `(BLOCK (DEFINE ,SWITCH-VAR
            (MAKE-STREAM-SWITCH (LOCATIVE ,STREAM-VAR)))
          (LSET ,STREAM-VAR ,INITIAL-VALUE)))

(DEFINE (MAKE-STREAM-SWITCH LOC)
  (LET ((DEPENDENTS '()))
    (OBJECT (LAMBDA () (VCELL-CONTENTS LOC))    ; !!
            ((SETTER SELF)
             (LAMBDA (STREAM)
               (LET ((PREV (SWAP (CONTENTS LOC) STREAM)))
                 ;; BIND standard output, set terminal output, unbind,
                 ;; you lose big.
                 (WALK (LAMBDA (D)
                         (COND ((EQ? (D) PREV)  ; ??
                                (SET (D) STREAM))))
                       DEPENDENTS)
                 STREAM)))
            ((SET-DEPENDENT SELF D)
             (COND ((NOT (MEMQ? D DEPENDENTS))
                    (PUSH DEPENDENTS D))))
            ((PRINT-TYPE-STRING SELF) "Stream-switch")
            ((IDENTIFICATION SELF) (VCELL-ID LOC)))))

(DEFINE-OPERATION (SET-DEPENDENT SWITCH OTHER-SWITCH))  ; INITIALIZE-REPL calls

;;; --- Terminal input and output

(DEFINE-STREAM-SWITCH TERMINAL-INPUT  *TERMINAL-INPUT*
  *THE-TERMINAL-INPUT-STREAM*)

(DEFINE-STREAM-SWITCH TERMINAL-OUTPUT *TERMINAL-OUTPUT*
  *THE-TERMINAL-OUTPUT-STREAM*)

;;; --- Standard input and output

;;; Bug: we really want to be asking if stdin and ttyin are the "same."
;;; Problem is that T's and Apollo's concepts of standard input may differ
;;; radically.

(DEFINE-STREAM-SWITCH STANDARD-INPUT  *STANDARD-INPUT*
  (TERMINAL-INPUT))

(DEFINE-STREAM-SWITCH STANDARD-OUTPUT *STANDARD-OUTPUT*
  (TERMINAL-OUTPUT))

(SET-DEPENDENT TERMINAL-INPUT  STANDARD-INPUT)
(SET-DEPENDENT TERMINAL-OUTPUT STANDARD-OUTPUT)

(DEFINE (REINITIALIZE-STREAMS)
  (SET (STANDARD-INPUT)  (TERMINAL-INPUT))
  (SET (STANDARD-OUTPUT) (TERMINAL-OUTPUT)))

;;; --- Other streams

(DEFINE-STREAM-SWITCH ERROR-OUTPUT *ERROR-OUTPUT*
  (TERMINAL-OUTPUT))

(SET-DEPENDENT TERMINAL-OUTPUT ERROR-OUTPUT)

(DEFINE-STREAM-SWITCH DEBUG-OUTPUT *DEBUG-OUTPUT*
  (ERROR-OUTPUT))

(SET-DEPENDENT ERROR-OUTPUT DEBUG-OUTPUT)
