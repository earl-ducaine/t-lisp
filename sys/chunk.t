(HERALD (TSYS CHUNK T 50)
        (ENV TSYS)
        (PRE-COOK))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Chunk manipulation semi-primitives

;;; There should be no calls to the code-chunk access primops outside of
;;; this module (and maybe the GC).  Beware.

;;; (a) Chunk info extractors.

(DEFINE-INTEGRABLE (CHUNK-PROCEDURE? CHUNK)
  (FIXNUM-BIT? (CHUNK-SUBTYPE-BITS CHUNK) %%PROCEDURE?-BIT-POS))

(DEFINE-INTEGRABLE (CHUNK-ENTITY? CHUNK)
  (FIXNUM-BIT? (CHUNK-SUBTYPE-BITS CHUNK) %%ENTITY?-BIT-POS))

(DEFINE-INTEGRABLE (CHUNK-FRAME? CHUNK)
  (FIXNUM-BIT? (CHUNK-SUBTYPE-BITS CHUNK) %%RETURN?-BIT-POS))

(DEFINE-INTEGRABLE (CHUNK-STANDARD-GC? CHUNK)
  (FIXNUM-BIT? (CHUNK-SUBTYPE-BITS CHUNK) %%STANDARD-GC?-BIT-POS))

(DEFINE-INTEGRABLE (CHUNK-DEFINER? CHUNK)
  (FIXNUM-BIT? (CHUNK-SUBTYPE-BITS CHUNK) %%DEFINER?-BIT-POS))

(DEFINE-INTEGRABLE (CHUNK-LEXPR? CHUNK)
  (FIXNUM-BIT? (CHUNK-SUBTYPE-BITS CHUNK) %%LEXPR?-BIT-POS))

;;; Basic thing called by GET-METHOD.

(DEFINE (HANDLE-EXTEND OBJ STATE)
  (LET ((TEM (EXTEND-TEMPLATE OBJ)))
    (COND ((NOT (TEMPLATE? TEM))
	   (%DISPATCH-NEXT OBJ STATE))          ;Corrupt pointer.
          (ELSE
           (LET* ((CHUNK (TEMPLATE-CHUNK TEM))
                  (BITS (CHUNK-SUBTYPE-BITS CHUNK)))
             (COND ((FIXNUM-BIT? BITS %%ENTITY?-BIT-POS)
                    ((POINTER-SUBTRACT
                      (CHANGE-TAG TEM %%TEMPLATE-TAG %%EXTEND-TAG)
                      (IF (FIXNUM-BIT? BITS %%PROCEDURE?-BIT-POS)
                          (CHUNK-EXTRA-SLOT-1 CHUNK)
                        (CHUNK-EXTRA-SLOT-0 CHUNK)))
                     OBJ
                     STATE))
                   (ELSE
		    (%DISPATCH-NEXT OBJ STATE))))))))

;;; (b) Template info extractors.

;;; Given a template, find out in what unit it lives.

(DEFINE (TEMPLATE-UNIT TEM)
  (POINTER-SUBTRACT (CHANGE-TAG TEM %%TEMPLATE-TAG %%EXTEND-TAG)
                    (CHUNK-UNIT-OFFSET (TEMPLATE-CHUNK TEM))))

;;; Given a template, get its gc-method.

(DEFINE (TEMPLATE-GC-METHOD TEM)
  (POINTER-SUBTRACT (CHANGE-TAG TEM %%TEMPLATE-TAG %%EXTEND-TAG)
                    (CHUNK-GC-METHOD-OFFSET (TEMPLATE-CHUNK TEM))))

;;; Given a template, get its superior template.

(DEFINE (TEMPLATE-SUPERIOR-TEMPLATE TEM)
  (LET ((OFFSET (CHUNK-SUPERIOR-OFFSET (TEMPLATE-CHUNK TEM))))
    (IF (FX= OFFSET 0) NIL
      (POINTER-SUBTRACT TEM OFFSET))))

;;; Given a template for a defining thing, get the value cell of the variable
;;; which is defined by the thing.

(DEFINE (TEMPLATE-DEFINEE TEM)
  (LET ((CHUNK (TEMPLATE-CHUNK TEM)))
    (IF (CHUNK-DEFINER? CHUNK)
        (XREF (TEMPLATE-UNIT TEM)
              (COND ((AND (CHUNK-PROCEDURE? CHUNK) (CHUNK-ENTITY? CHUNK))
                     (CHUNK-EXTRA-SLOT-2 CHUNK))
                    ((OR (CHUNK-PROCEDURE? CHUNK) (CHUNK-ENTITY? CHUNK))
                     (CHUNK-EXTRA-SLOT-1 CHUNK))
                    (ELSE
                     (CHUNK-EXTRA-SLOT-0 CHUNK))))
      NIL)))

(DEFINE-INTEGRABLE (PROCEDURE-TEMPLATE? TEM)
  (GC-DEFER (CHUNK-PROCEDURE? (TEMPLATE-CHUNK TEM))))

(DEFINE-INTEGRABLE (FRAME-TEMPLATE? TEM)
  (GC-DEFER (CHUNK-FRAME? (TEMPLATE-CHUNK TEM))))

(DEFINE-INTEGRABLE (ENTITY-TEMPLATE? TEM) 
  (GC-DEFER (CHUNK-ENTITY? (TEMPLATE-CHUNK TEM))))

;;; (c) Object predicates.

(DEFINE (EXTEND-PROCEDURE? OBJ) 
  (AND (EXTEND? OBJ) (PROCEDURE-TEMPLATE? (EXTEND-TEMPLATE OBJ))))

(DEFINE (ENTITY? OBJ)
  (AND (EXTEND? OBJ) (ENTITY-TEMPLATE? (EXTEND-TEMPLATE OBJ))))

(DEFINE (FRAME? OBJ)
  (AND (EXTEND? OBJ) (FRAME-TEMPLATE? (EXTEND-TEMPLATE OBJ))))

;;; (d) Extend info extractors.

;;; Extract argspectrum info for an extend as (min#args . lexpr?)

(DEFINE (EXTEND-ARGSPECTRUM PROC)
  (ENFORCE PROCEDURE? PROC)
  (LET ((CHUNK (TEMPLATE-CHUNK (EXTEND-TEMPLATE PROC))))
    (CONS (CHUNK-NARGS CHUNK) (CHUNK-LEXPR? CHUNK))))

;;; Extract size info as (ptr-size . scratch-size)

(DEFINE (EXTEND-SIZE-INFO OBJ)
  (PROCLAIM EXTEND? OBJ)
  (LET ((CHUNK (TEMPLATE-CHUNK (EXTEND-TEMPLATE OBJ))))
    (IF (CHUNK-STANDARD-GC? CHUNK)
        (CONS (CHUNK-POINTER-COUNT CHUNK) (CHUNK-SCRATCH-COUNT CHUNK))
      'NONSTANDARD)))

;;; Get the definee (vcell) for a definer.

(DEFINE-INTEGRABLE (EXTEND-DEFINEE OBJ)
  (TEMPLATE-DEFINEE (EXTEND-TEMPLATE OBJ)))

;;; Kludgey definition - a "correct" one would check for nonstandard GC method.
;;; Unfortunately, such nonstandard methods aren't set up at the moment.

(DEFINE (STANDARD-FRAME? FRAME)
  (NOT (NULL? (TEMPLATE-SUPERIOR-TEMPLATE (EXTEND-TEMPLATE FRAME)))))

;;; Three values of interest:
;;; (a)   # of argument slots;
;;; (b)   # of scratch-mem slots; and
;;; (c)   # of pointer-mem slots.
;;; Note that (a) is always even.  Sum of (b) and (c) is always odd.
;;; The continuation VALUES is passed four arguments: the frame itself,
;;;  and these three numbers.
;;; It is wrong to call this unless the frame is a standard-format frame.

(DEFINE (GET-FRAME-SIZE-INFO FRAME VALUES)
  (LET* ((TEM (EXTEND-TEMPLATE FRAME))
         (CHUNK (TEMPLATE-CHUNK TEM))
         (SUPER (TEMPLATE-CHUNK (TEMPLATE-SUPERIOR-TEMPLATE TEM))))
    (VALUES FRAME
            (FX+ (LET ((Z (CHUNK-POINTER-COUNT CHUNK)))
                   (IF (FX= Z 255.) -1 Z))
                 1)
            (CHUNK-SCRATCH-MEM-SIZE SUPER)
            (CHUNK-POINTER-MEM-SIZE SUPER))))

;;; Winning CRAWL auxiliary.

(DEFINE (EXHIBIT-TEMPLATE TEM)
  (LET ((S (TERMINAL-OUTPUT))
        (CH (TEMPLATE-CHUNK TEM)))
    (FORMAT S '(" Address = #x~X~%"
                " Chunk address = #x~X~%"
                " Unit offset = ~S~%"
                " Superior offset = ~S~%")
            (POINTER->INTEGER TEM)
            (POINTER->INTEGER CH)
            (CHUNK-UNIT-OFFSET CH)
            (CHUNK-SUPERIOR-OFFSET CH))
    (COND ((CHUNK-STANDARD-GC? CH)
           (FORMAT S " Size: ~S ptr, ~S scr~%"
                   (CHUNK-POINTER-COUNT CH)
                   (CHUNK-SCRATCH-COUNT CH)))
          (ELSE
           (FORMAT S " Nonstandard GC offset = ~S~%"
                   (CHUNK-GC-METHOD-OFFSET CH))))
    (COND ((CHUNK-FRAME? CH)
           (FORMAT S " Frame~%")))
    (COND ((CHUNK-PROCEDURE? CH)
           (FORMAT S " Procedure; ~A ~S arg~P expected~%"
                   (IF (CHUNK-LEXPR? CH) "at least" "exactly")
                   (CHUNK-NARGS CH)
                   (CHUNK-NARGS CH))
           (FORMAT S " Ptr-mem size ~S, scr-mem ~S~%"
                   (CHUNK-POINTER-MEM-SIZE CH)
                   (CHUNK-SCRATCH-MEM-SIZE CH))))
    (COND ((CHUNK-ENTITY? CH)
           (FORMAT S " Handler offset = ~S~%" 
                   (IF (CHUNK-PROCEDURE? CH)
                       (CHUNK-EXTRA-SLOT-1 CH)
                     (CHUNK-EXTRA-SLOT-0 CH)))))
    (COND ((CHUNK-DEFINER? CH)
           (FORMAT S " Definee offset = ~S~%"
                   (COND ((AND (CHUNK-PROCEDURE? CH) (CHUNK-ENTITY? CH))
                          (CHUNK-EXTRA-SLOT-2 CH))
                         ((OR (CHUNK-PROCEDURE? CH) (CHUNK-ENTITY? CH))
                          (CHUNK-EXTRA-SLOT-1 CH))
                         (ELSE
                          (CHUNK-EXTRA-SLOT-0 CH))))))))
