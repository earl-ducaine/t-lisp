(HERALD (TSYS FRAME T 47)
        (ENV TSYS))        

;;; Copyright (c) 1983, 1984 Yale University

;;;; Stuff for hacking stack frames

;;; FRAME-PREVIOUS returns frame in stack to which the given stack frame
;;; will return.  If there is no such "superior", return null.

(DEFINE-OPERATION (FRAME-PREVIOUS FRAME)
  (LET ((FRAME (CHECK-ARG STANDARD-FRAME? FRAME FRAME-PREVIOUS)))
    (%XLOC FRAME (FX+ (FRAME-SIZE FRAME) 1))))

;;; Format of V-frame is:
;;;  xref -1:  return point
;;;        0:  zero, if alignment was necessary
;;;   0 or 1:  pointer to previous frame
;;;      ...:  stuff
;;;        N:  template for previous frame

(DEFINE (VFRAME-PREVIOUS FRAME)
  (LET ((PROBE (XREF FRAME 0)))
    (IF (POINTER-EQUAL? PROBE 0)
	(XREF FRAME 1)
	PROBE)))

(DEFINE (VFRAME-SIZE FRAME)
  (FX- (FIXNUM-ASHL (POINTER-SUBTRACT (VFRAME-PREVIOUS FRAME) FRAME) 1) 1))

;;; Returns an upper bound for valid XREF indices into the frame; that is,
;;; the size of the frame, in pesos.  This is always odd.
;;; Avoid depending on ENFORCE's existence!

(DEFINE (FRAME-SIZE FRAME)
  (GET-FRAME-SIZE-INFO FRAME
                       (LAMBDA (FRAME X Y Z)
                         (IGNORE FRAME)
                         (LET ((SUM (FX+ (FX+ X Y) Z)))
                           (COND ((NOT (FIXNUM-ODD? SUM))
                                  (*ENFORCE FIXNUM-ODD? SUM))
                                 (ELSE SUM))))))

(DEFINE (FRAME-ANY PRED FRAME) ; Like ANY but hacks a frame instead of a list.
  (COND ((STANDARD-FRAME? FRAME)
         (GET-FRAME-SIZE-INFO FRAME
                              (LAMBDA (FRAME X Y Z)
                                (LET ((END (FX+ Z (FX+ X Y))))
                                  (ITERATE LOOP ((I (FX+ X Y)))
                                    (COND ((FX>= I END) NIL)
                                          ((PRED (XREF FRAME I)))
                                          (ELSE (LOOP (FX+ I 1)))))))))
        (ELSE NIL)))


(DEFINE HANDLE-STACK-BASE
  (%HANDLER ((FRAME-PREVIOUS FRAME) (IGNORE FRAME) NIL)
            ((GET-ENVIRONMENT FRAME)   (IGNORE FRAME) NIL)
            ((FRAME-PRINT-SYNOPSIS FRAME STREAM) (IGNORE FRAME STREAM) NIL)
            ((PRINT-TYPE-STRING SELF) "Stack-base")))

(DEFINE HANDLE-VFRAME
  (%HANDLER ((FRAME-PREVIOUS FRAME) (VFRAME-PREVIOUS FRAME))
            ((GET-ENVIRONMENT FRAME)   (IGNORE FRAME) NIL)
            ((FRAME-PRINT-SYNOPSIS FRAME STREAM) (IGNORE FRAME STREAM) NIL)
            ((PRINT-TYPE-STRING SELF) "V-frame")))

(DEFINE HANDLE-MAGIC-FRAME
  (%HANDLER ((GET-ENVIRONMENT FRAME)   (IGNORE FRAME) NIL)
            ((FRAME-PRINT-SYNOPSIS FRAME STREAM) (IGNORE FRAME STREAM) NIL)
            ((PRINT-TYPE-STRING SELF) "Dynamic-state-transition")))
