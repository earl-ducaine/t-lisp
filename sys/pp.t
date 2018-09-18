(HERALD (TSYS PP T 108)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; A pretty-printer

(DEFINE-OPERATION (PRETTY-PRINT OBJ STREAM)
  (PRINT OBJ STREAM))

;;; Handler for lists.

(DEFINE-METHODS HANDLE-PAIR
  ((PRETTY-PRINT OBJ STREAM) (PP-LIST OBJ STREAM)))

(DEFINE (PP-LIST X STREAM)
  (COND ((READ-MACRO-INVERSE X)
         => (LAMBDA (INVERSE)
              (WRITES STREAM INVERSE)
              (PRETTY-PRINT (CADR X) STREAM)))
        ((PRINT-WIDTH-GREATER? X (FX- (LINE-LENGTH STREAM) (HPOS STREAM)))
         ;; If ordinary print won't win...
         (PP-LIST-VERTICALLY X STREAM))
        (ELSE
         (PP-LIST-HORIZONTALLY X STREAM))))

(DEFINE (PP-LIST-VERTICALLY   X STREAM)
  (MAYBE-PP-LIST-VERTICALLY T X STREAM))

(DEFINE (PP-LIST-HORIZONTALLY X STREAM)
  (MAYBE-PP-LIST-VERTICALLY NIL X STREAM))

(DEFINE (MAYBE-PP-LIST-VERTICALLY VERTICAL? LIST STREAM)
  (WRITEC STREAM #\LEFT-PAREN)
  (IF (NULL? LIST) (WRITEC STREAM #\RIGHT-PAREN)
    (LET ((OLD-HPOS (HPOS STREAM)))
      (COND ((AND (SYNTAX-DESCRIPTOR? (CAR LIST))
		  (IDENTIFICATION (CAR LIST)))
	     => (LAMBDA (ID) (PRINT ID STREAM)))   ;Hack!
	    (ELSE
	     (PRETTY-PRINT (CAR LIST) STREAM)))
      (IF (AND VERTICAL? ;Heuristic for things like DO, COND, ...
               (PAIR? (CAR LIST))
               (NOT (NULL? (CDR LIST))))
          (INDENT-NEWLINE (FX- OLD-HPOS 1) STREAM))
      (LET ((OLD-HPOS (FX+ (HPOS STREAM) 1)))
        (ITERATE TAIL ((FLAG NIL) (L (CDR LIST)))
          (COND ((PAIR? L)
                 (COND (FLAG (INDENT-NEWLINE OLD-HPOS STREAM))
                       (ELSE (WRITEC STREAM #\SPACE)))    ; Not (SPACE STREAM)!
                 (PRETTY-PRINT (CAR L) STREAM)
                 (TAIL VERTICAL? (CDR L)))
                (ELSE
                 (COND ((NOT (NULL? L))
                        (FORMAT STREAM " . ")
                        (IF FLAG (INDENT-NEWLINE OLD-HPOS STREAM))
                        (PRETTY-PRINT L STREAM)))
                 (WRITEC STREAM #\)))))))))

;;; Utility: go to given column on a new line.

(DEFINE (INDENT-NEWLINE X STREAM)
  (NEWLINE STREAM)
  (SET-HPOS STREAM X))

;;; Find printed representation for internal representation of read macro.

(DEFINE (READ-MACRO-INVERSE X)
  (COND ((AND (PAIR? X)
              (PAIR? (CDR X))
              (NULL? (CDDR X)))
         (SELECT (CAR X)
           ((*QUOTE*)        "'")
           ((*BACKQUOTE*)    "`")
           ((*COMMA*)        ",")
           ((*COMMA-ATSIGN*) ",@")
           (ELSE NIL)))
        (ELSE NIL)))

;;; "User interface" stuff

(DEFINE (*PP-SYMBOL SYMBOL ENV)
  (*PP (COND ((SYNTAX-TABLE-ENTRY (ENV-SYNTAX-TABLE ENV) SYMBOL)
              => IDENTITY)
             ((ENV-LOOKUP ENV SYMBOL NIL NIL)
              => (LAMBDA (LOC)
                   (LET ((VAL (CONTENTS LOC)))
                     (COND ((NONVALUE? VAL) 'UNBOUND)
                           (ELSE VAL)))))
             (ELSE 'UNBOUND))))

(DEFINE (*PP OBJ)
  (LET ((OBJ (OR (DISCLOSE OBJ) OBJ)))
    (LET ((STREAM (TERMINAL-OUTPUT)))
      (FRESH-LINE STREAM)
      (COND ((AND (PROCEDURE? OBJ) (WHERE-DEFINED OBJ))
             => (LAMBDA (WHERE) (FORMAT STREAM ";See ~A." WHERE)))
            (ELSE
             (PRETTY-PRINT OBJ STREAM)))
      (FRESH-LINE STREAM)
      *REPL-WONT-PRINT*)))
