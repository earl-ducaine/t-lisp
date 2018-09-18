(HERALD (TSYS VMWRITE T 68)
        (ENV TSYS)
        (PRE-COOK))                     ; for safety during GC

;;; Copyright (c) 1983, 1984 Yale University

;;;; PRINT and FORMAT for the "virtual machine"

;;; Need (SYMBOL-PNAME sym)
;;;     (SYMBOL? object)
;;;     (CHAR? object)
;;;     ... other random predicates ...
;;;     (CHANNEL-WRITES channel string)
;;;     (CHANNEL-WRITEC channel char)
;;;     (VCELL-HAS-VALUE? symbol)       - these are for ZBOUND?
;;;     (SYMBOL-VCELL symbol)           - not essential

;;; We herein EXPLICITLY AVOID any dependence on entities, operations, or
;;; FORMAT.  We must not throw away the scaffolding!
;;; At some point the "real" printer will take over and we can just stop
;;; loading this one, and let it lie dormant until the next transportation
;;; effort.

(DEFINE (ZWRITEC STREAM CHAR)
  (COND ((CHANNEL? STREAM) (CHANNEL-WRITEC STREAM CHAR))
        ((NULL? STREAM) (PUSH *Z-CHAR-ACCUM* CHAR))
        ((NEQ? WRITEC ZWRITEC) (WRITEC STREAM CHAR))
        (ELSE (ERROR "illegal channel - (ZWRITEC ~S ~S)" STREAM CHAR))))

(DEFINE (ZWRITES STREAM STRING)
  (COND ((CHANNEL? STREAM) (CHANNEL-WRITES STREAM STRING))
        ((NULL? STREAM)
         (DO ((I 0 (FX+ I 1)))
             ((FX>= I (STRING-LENGTH STRING)))
           (PUSH *Z-CHAR-ACCUM* (NTHCHAR STRING I))))
        ((NEQ? WRITES ZWRITES) (WRITES STREAM STRING))
        (ELSE (ERROR "illegal channel - (ZWRITES ~S ~S)" STREAM STRING))))

(DEFINE (ZNEWLINE STREAM) (ZWRITEC STREAM #\NEWLINE))
(DEFINE (ZSPACE   STREAM) (ZWRITEC STREAM #\SPACE))
(DEFINE (ZFORCE-OUTPUT STREAM) (CHANNEL-FORCE-OUTPUT STREAM))

(DEFINE (ZPRINT FORM STREAM)
  (ZWRITE STREAM FORM))

(LSET *ZOUTPUT-RADIX* 10.)

(DEFINE (ZWRITE STREAM FORM)
  (SET *ZOUTPUT-RADIX* 10.)
  (SUB-ZWRITE STREAM FORM))

(DEFINE (SUB-ZWRITE STREAM FORM)
  (COND ((NOT (REASONABLE? FORM))
         (SUB-ZWRITE-RANDOM STREAM "Unreasonable" FORM))
        ((STRING? FORM) (ZWRITEC STREAM #\")
                        (ZWRITES STREAM FORM)
                        (ZWRITEC STREAM #\"))
        ((NULL? FORM) (ZWRITES STREAM "()"))
        ((CHAR? FORM)
         (COND ((CHAR= FORM #\RUBOUT) (ZWRITES STREAM "#\\RUBOUT"))
               ((CHAR= FORM #\SPACE) (ZWRITES STREAM "#\\SPACE"))
               ((CHAR< FORM #\SPACE)
                (ZWRITES STREAM "#^")
                (ZWRITEC STREAM (POINTER-ADD FORM #o100)))
               (ELSE (ZWRITES STREAM "#\\") (ZWRITEC STREAM FORM))))
        ((FIXNUM? FORM)
         (SUB-ZWRITE-FIXNUM STREAM FORM))
        ((PAIR? FORM) (SUB-ZWRITE-LIST STREAM FORM))
        ((EXTEND? FORM) (SUB-ZWRITE-EXTEND STREAM FORM))
        ((NONVALUE? FORM)
         (SUB-ZWRITE-OBJECT STREAM "Nonvalue" (NONVALUE->VALUE FORM)))
        (ELSE
	 (SUB-ZWRITE-RANDOM STREAM
			    (COND ((TEMPLATE?   FORM) "Template")
				  ((FLONUM?     FORM) "Flonum")
				  ((MISC?       FORM) "Misc")
				  (ELSE               "Random"))
			    FORM))))

(DEFINE (SUB-ZWRITE-LIST STREAM L)
  (COND ((NULL? L) (ZWRITES STREAM "()"))
        (ELSE
         (ZWRITEC STREAM #\()
         (ITERATE LOOP ((L L))
           (SUB-ZWRITE STREAM (CAR L))
           (COND ((ATOM? (CDR L))
                  (COND ((NOT (NULL? (CDR L)))
                         (ZWRITES STREAM " . ")
                         (SUB-ZWRITE STREAM (CDR L))))
                  (ZWRITEC STREAM #\)))
                 (ELSE
                  (ZWRITEC STREAM #\SPACE)
                  (LOOP (CDR L))))))))

(DEFINE (SUB-ZWRITE-FIXNUM STREAM N)
  (COND ((FIXNUM-EQUAL? N 0) (ZWRITEC STREAM #\0))
        ((FIXNUM-LESS? N 0)
         (ZWRITEC STREAM #\-)           ; lose on most negative fixnum
         (SUB-ZWRITE-FIXNUM-1 STREAM (FIXNUM-SUBTRACT 0 N)))
        (ELSE (SUB-ZWRITE-FIXNUM-1 STREAM N))))

(DEFINE (SUB-ZWRITE-FIXNUM-1 STREAM N)
  (COND ((FIXNUM-LESS? N *ZOUTPUT-RADIX*)
         (ZWRITEC STREAM (ZDIGIT->CHAR N)))
        (ELSE (SUB-ZWRITE-FIXNUM-1 STREAM (FIXNUM-DIVIDE N *ZOUTPUT-RADIX*))
           (ZWRITEC STREAM
                    (ZDIGIT->CHAR (FIXNUM-REMAINDER N *ZOUTPUT-RADIX*))))))

(DEFINE (ZDIGIT->CHAR N)
  (COND ((FX< N 10) (CHAR+ N #\0))
        (ELSE (CHAR+ (FX- N 10) #\A))))

(DEFINE (SUB-ZWRITE-EXTEND STREAM FORM)
  (COND ((SYMBOL? FORM)
         (ZWRITES STREAM (SYMBOL-PNAME FORM)))
        ((VECTOR? FORM) 
         (SUB-ZWRITE-OBJECT STREAM "Vector" (VECTOR-LENGTH FORM)))
        ((VCELL? FORM)
         (SUB-ZWRITE-OBJECT STREAM "Vcell" (VCELL-ID FORM)))
        ((XENOID? FORM)
         (SUB-ZWRITE-RANDOM STREAM "Xenoid" (XENOID-POINTER FORM)))
        (ELSE
         (SUB-ZWRITE-RANDOM STREAM
                            (COND ((UNIT?   FORM) "Unit")
                                  (ELSE           "Extend"))
                            FORM))))

(DEFINE (SUB-ZWRITE-RANDOM STREAM TYPE-STRING OBJ)
; (SUB-ZWRITE-OBJECT STREAM TYPE-STRING (POINTER-ADDRESS OBJ))
  (ZWRITES STREAM "#{")
  (ZWRITES STREAM TYPE-STRING)
  (ZWRITEC STREAM #\SPACE)
  (SUB-ZWRITE-HEX STREAM (POINTER->FIXNUM OBJ))
  (ZWRITEC STREAM #\}))

(DEFINE (SUB-ZWRITE-OBJECT STREAM TYPE-STRING ID)
  (ZWRITES STREAM "#{")
  (ZWRITES STREAM TYPE-STRING)
  (ZWRITEC STREAM #\SPACE)
  (SUB-ZWRITE STREAM ID)
  (ZWRITEC STREAM #\}))

(DEFINE (SUB-ZWRITE-HEX STREAM OBJ)
  (LET ((SAVE *ZOUTPUT-RADIX*))
    (SET *ZOUTPUT-RADIX* 16.)
    (SUB-ZWRITE STREAM OBJ)
    (SET *ZOUTPUT-RADIX* SAVE)))

(LSET *Z-CHAR-ACCUM* 0)

(DEFINE (ZFORMAT STREAM FMT . STUFF)
  (LET ((FMT (CHOPY (COND ((STRING? FMT) FMT)
                          ((PAIR? FMT) (CAR FMT))
                          (ELSE "~%###losing format string!###~%"))))
        (SAVE *Z-CHAR-ACCUM*)
        (STREAM (IF (EQ? STREAM T) ZTERMINAL-OUTPUT STREAM)))
    (SET *Z-CHAR-ACCUM* '())
    (ITERATE LOOP ((STUFF STUFF)) 
      (COND ((STRING-EMPTY? FMT) T)
            ((CHAR= (CHAR FMT) #\~)
             (CHDR! FMT)
             (LET ((OP (CHAR FMT)))
               (CHDR! FMT)
               (CASE OP
                 ((#\& #\%) (ZNEWLINE STREAM) (LOOP STUFF))
                 ((#\S #\s #\D #\d)
                  (ZWRITE STREAM (CAR STUFF)) (LOOP (CDR STUFF)))
                 ((#\A #\a)
                  ((COND ((STRING? (CAR STUFF)) ZWRITES)
                         ((CHAR? (CAR STUFF)) ZWRITEC)
                         (ELSE ZWRITE))
                   STREAM (CAR STUFF))
                  (LOOP (CDR STUFF)))
                 ((#\C #\c) (ZWRITEC STREAM (CAR STUFF)) (LOOP (CDR STUFF)))
                 ((#\X #\x) (SUB-ZWRITE-HEX STREAM (CAR STUFF))
                            (LOOP (CDR STUFF)))
                 ((#\~) (ZWRITEC STREAM #\~) (LOOP STUFF))
                 ((#\_) (ZWRITEC STREAM #\SPACE) (LOOP STUFF))
                 ((#\LINEFEED #\RETURN #\SPACE)
                  (ITERATE SKIP ()
                    (COND ((ZWHITESPACE? (CHAR FMT))
                           (CHDR! FMT) (SKIP))
                          (ELSE (LOOP STUFF)))))
                 (ELSE
                  (ZFORMAT STREAM "###~~~C unknown format op###" OP)
                  (LOOP STUFF)))))
            (ELSE
             (ZWRITEC STREAM (CHAR FMT))
             (CHDR! FMT)
             (LOOP STUFF))))
    (BLOCK0 (COND ((NULL? STREAM)
                   (BACKWARDS-LIST->STRING *Z-CHAR-ACCUM*))
                  (ELSE NIL))
            (SET *Z-CHAR-ACCUM* SAVE))))

(DEFINE (BACKWARDS-LIST->STRING L)
  (LET ((LEN (LENGTH L)))
    (LET ((STR (%MAKE-STRING LEN)))
      (DO ((I (FX- LEN 1) (FX- I 1))
           (L L (CDR L)))
          ((FX< I 0) STR)
        (SET (NTHCHAR STR I) (CAR L))))))
