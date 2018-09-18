(herald (tsys obsolete t 61)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;; Definitions for backwards compatibility.

(DEFINE-LOCAL-SYNTAX (DEFINE-OBSOLETE OLD NEW)
  `(SET ,OLD (%OBSOLETE ',OLD ',NEW ,NEW)))

(DEFINE (%OBSOLETE OLD-NAME NEW-NAME NEW)
  (LET ((WARNED? NIL))
    (OBJECT (LAMBDA ARGS
              (COND ((AND (NOT WARNED?) *MENTION-OBSOLETE-REFERENCES?*)
                     (FORMAT (ERROR-OUTPUT)
   '("~&;** Warning: calling a system procedure under an obsolete name.~%"
     ";** ~S has been renamed to be ~S.~%")
			     OLD-NAME NEW-NAME)
                     (SET WARNED? T)))
              (APPLY NEW ARGS))
            ((PRINT-TYPE-STRING SELF) "Obsolete")
            ((IDENTIFICATION SELF) OLD-NAME)
	    ((SETTER SELF) (SETTER NEW)))))

(LSET *MENTION-OBSOLETE-REFERENCES?* T)

;;; Add new entries to bottom of list, with date added.

;(DEFINE-OBSOLETE PRINTWIDTH       PRINT-WIDTH)
;(DEFINE-OBSOLETE DISPLAYWIDTH     DISPLAY-WIDTH)
;(DEFINE-OBSOLETE CHARACTER?      CHAR?)
;(DEFINE-OBSOLETE CHARACTER->ASCII CHAR->ASCII)
;(DEFINE-OBSOLETE ASCII->CHARACTER ASCII->CHAR)
;(DEFINE-OBSOLETE CHARACTER->DIGIT CHAR->DIGIT)
;(DEFINE-OBSOLETE DIGIT->CHARACTER DIGIT->CHAR)
;(DEFINE-OBSOLETE CHARACTER-NAME          CHAR-NAME)
;(DEFINE-OBSOLETE NAME-CHARACTER          NAME-CHAR)
;(DEFINE-OBSOLETE HANDLE-CHARACTER HANDLE-CHAR)
;(DEFINE-OBSOLETE STYPE-ORIGINAL   STYPE-MASTER)
;(DEFINE-OBSOLETE ID               IDENTITY)
;(DEFINE-OBSOLETE COPY             COPY-TREE)
;(DEFINE-OBSOLETE CALLABLE?        PROCEDURE?)

;(DEFINE-OBSOLETE READ-OBJECT-IGNORING-CLOSE-BRACKETS READ-OBJECT)
;(DEFINE-OBSOLETE THE-SYNTAX-TABLE          REPL-READ-TABLE)
;(DEFINE-OBSOLETE MAKE-VANILLA-SYNTAX-TABLE MAKE-VANILLA-READ-TABLE)
;(DEFINE-OBSOLETE COPY-SYNTAX-TABLE         COPY-READ-TABLE)
;(DEFINE-OBSOLETE READ-OBJECT-REFUSING-EOF  READ-REFUSING-EOF)  ; T 2.5
;(DEFINE-OBSOLETE DONT-PRINT?               REPL-WONT-PRINT?)
;(DEFINE-OBSOLETE WARN-IF-REDEFINING?        PRINT-ENV-WARNINGS?) ; 2.7
(DEFINE-OBSOLETE RIGHT-BRACKET-SYNTAX       LIST-TERMINATOR)     ; ephemeral

(define-obsolete gen-id      generate-symbol)		; 2.9 - 29 May 84
(define-obsolete generate-id generate-symbol)		; ditto
(define-obsolete symbolconc  concatenate-symbol)	; ditto
(define-obsolete howlong        integer-length)		; 2.9 - 7 July 84

(DEFINE *MAX-CHARACTER-CODE*      *NUMBER-OF-CHAR-CODES*)
;(DEFINE *SYNTAX-ESCAPE-CHAR*      *ESCAPE-CHAR*)       ; ?

;(DEFINE (THE-SYMBOL-TABLE) *THE-SYMBOL-TABLE*)

(DEFINE GEN-ID-COUNT        ; Used by TC.  (obs 21 Nov 83)
  (OBJECT (LAMBDA ()
            (SYMBOL-GENERATOR-COUNT GENERATE-SYMBOL))
          ((SETTER SELF)
           (LAMBDA (VAL)
             (SET (SYMBOL-GENERATOR-COUNT GENERATE-SYMBOL) VAL)))))

(DEFINE *THE-STANDARD-READ-TABLE* *STANDARD-READ-TABLE*)        ; Old name.

(DEFINE REM          REMOVE-PROPERTY)       ; Gone from 4th ed manual
(DEFINE REM-PROPERTY REMOVE-PROPERTY)       ; ditto

;;; Moribund entry points.

(DEFINE (MAKE-VANILLA-READ-TABLE)
  (MAKE-READ-TABLE *VANILLA-READ-TABLE* NIL))

(DEFINE (MAKE-STANDARD-READ-TABLE)      ; In 3rd ed manual but not 4th
  (MAKE-READ-TABLE *STANDARD-READ-TABLE* NIL))

(DEFINE (COPY-READ-TABLE TABLE) (MAKE-READ-TABLE TABLE NIL))

;;; Called from macro expansions.

(DEFINE-OBSOLETE WITH-OPEN-FILES-HANDLER        WITH-OPEN-STREAMS-HANDLER)
(DEFINE-OBSOLETE OUTPUT-TO-STRING-STREAM-STRING CLOSE)
(DEFINE-OBSOLETE OUTPUT-WIDTH-STREAM-COUNT      CLOSE)

;;; Ambivalence

(DEFINE *THE-SYSTEM-ENV* *T-IMPLEMENTATION-ENV*)
(DEFINE STREAM->CHANNEL STREAM-CHANNEL) ; ??
(DEFINE FRESHLINE FRESH-LINE)

;(DEFINE SI STANDARD-INPUT)
;(DEFINE SO STANDARD-OUTPUT)

(DEFINE-SYNTAX (REFERENCE ENV ID)
  `(*VALUE ,ENV ',ID))

(DEFINE *REFERENCE *VALUE)

(DEFINE (MDEBUG)
  (LET ((TABLE (ENV-SYNTAX-TABLE (REPL-ENV))))
    (ITERATE LOOP ((** '**))
      (FORMAT (TERMINAL-OUTPUT) "~&mdebug: ")
      (LET ((FORM (READ (TERMINAL-INPUT)))
            (DO-IT (LAMBDA (FORM)
                     (FRESH-LINE (TERMINAL-OUTPUT))
                     (LET ((** (MACRO-EXPAND FORM TABLE)))
                       (PRETTY-PRINT ** (TERMINAL-OUTPUT))
                       (LOOP **)))))
        (COND ((EQ? FORM '**) (DO-IT **))
              ((OR (EOF? FORM) (ATOM? FORM)) *REPL-WONT-PRINT*)
              (ELSE (DO-IT FORM)))))))

;;; Moribund entry point (4th edition manual, also still used everywhere?)

(define (invoke-macro-expander desc exp)
  (expand-macro-form desc
                     (check-special-form-syntax desc exp)   ;?
                     *standard-syntax-table*))

;;; Auxiliary for kludgey LOCALE macro.

(define (*locale id sexpr env)
  (let ((new-env (make-locale env id)))
    (if id (set-contents (env-lookup new-env id t t) new-env))
    (eval sexpr new-env)))

(define-syntax (**backquote-marker** form)
  (labels (((convert form)
            (cond ((atom? form) form)
                  ((eq? (car form) '**backquote-marker**)
                   (cons *backquote* (convert (convert (cdr form)))))
                  ((eq? (car form) '**backquote-comma-marker**)
                   (cons *comma* (cdr form)))
                  ((eq? (car form) '**backquote-atsign-marker**)
                   (cons *comma-atsign* (cdr form)))
                  (else
                   (cons (convert (car form))
                         (convert (cdr form)))))))
     (expand-backquote (convert form))))

(let ((z (env-syntax-table *t-implementation-env*)))
  (define-syntax set-var    (syntax-table-entry z 'set-variable-value))
  (define-syntax define-var (syntax-table-entry z 'define-variable-value)))

(declare-tsys-exports
  '(generate-id             ; see tree.t
    gen-id
    symbolconc
    env-lookup		                 ; In manual 3rd but not 4th
    *reference                           ; ? expansion of (reference ...)
    with-open-files-handler              ; expansion of (with-open-files ...)
    output-to-string-stream-string
    output-width-stream-count
    copy-read-table                      ; 3rd ed. of manual
    make-standard-read-table             ; ditto
    right-bracket-syntax
    rem
    rem-property
    cons-from-freelist		; In 3rd ed. but not 4th
    return-to-freelist		; ditto
    return-list-to-freelist	; ditto
    fixnum-howlong		; 2.9 - 12 July 84
    invoke-macro-expander	; 2.8
    ))

(declare-tsys-syntax-exports
  '(**backquote-marker**        ; T 2.8
    reference                   ; In 3rd ed, not 4th
    set-var			; T 2.9
    define-var
    ))
