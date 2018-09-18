(herald (tsys unfault t 42)
        (env tsys))

;;; Copyright (c) 1983, 1984 Yale University

;;;; Fault and interrupt handling

;;; Hack to allow different fault frame representations on the VAX and 68K.

(define fault-reg
  (object (lambda (frame regnum)
            (let ((val (xref frame regnum)))
              (if (fx= regnum %%sp) (pointer-add val 1) val)))
          ((setter self) set-fault-reg)))

(define (set-fault-reg frame regnum val)
  (xset frame regnum
        (if (fx= regnum %%sp) (pointer-subtract val 1) val)))

;;; AP points to [nargs, sig, code, scp]

(define (get-signal-number frame)
  (mref-32 (fault-reg frame %%ap) 1))

;;; Called by "handle-unix-signal"

(define (*unix-signal-fault* frame type)
  (ignore type)
  (let ((aborting? t)
	(signal (get-signal-number frame)))
    (unwind-protect (block (if (not *bsd4.2?*)
			       ;; Reinstate the handler (4.1)
			       (call-xenoid *signal-xenoid*
					    (xenoid-pointer
					     *handle-unix-signal-xenoid*)
					    (fixnum->pointer signal)))
			   ((vref *unix-signal-handlers* signal) frame)
			   (set aborting? nil))
		    ;; Unwind action
		    (if aborting? (signal-unblock signal))
		    (set *re-enter-on-fault?* t))
    (cleanup-interrupt-frame frame)))

(define (signal-unblock signal)
  (set *re-enter-on-fault?* t)		;prepare to get queued interrupt
  (call-xenoid *sigunblock-xenoid* (fixnum->pointer signal)))

(define *number-of-signals* 27)   ;4.2

(define *unix-signal-handlers*
  (vector-fill (make-vector (fx+ *number-of-signals* 1))
	       'default))

(define signal-handler
  (object (lambda (signal) (vref *unix-signal-handlers* signal))
	  ((setter self) set-signal-handler)))

(define (set-signal-handler signal value)
  (set (vref *unix-signal-handlers* signal) value)
  (call-xenoid *signal-xenoid*
	       (case value
		     ((default) (fixnum->pointer 0))
		     ((ignore)  (fixnum->pointer 1))
		     (else
		      (xenoid-pointer *handle-unix-signal-xenoid*)))
	       (fixnum->pointer signal))
  value)

(define *signals*
  ;;    s = synchronous, a = asynchronous
  ;;	signal# synch? description			  action
          '(( 1 a "hangup")      	                 ;error
	   ;( 2 a "interrupt")			         ;breakpoint
	   ;( 3 a "quit")			         ;zbreakpoint
	    ( 4 s "illegal instruction")                 ;error
	    ( 5 s "trace/BPT trap")	  	         ;error
	    ( 6 s "IOT instruction")	       	         ;error
	    ( 7 s "EMT instruction")	       	         ;error
	    ( 8 s "floating point exception")            ;error
	   ;( 9 a "kill")                                ;can't-handle
	    (10 s "memory protection violation")         ;error
	    (11 s "reference to non-existent memory")    ;error
	    (12 s "bad argument to a system call")       ;error
	    (13 a "broken pipe")       		         ;error
	    (14 a "alarm clock")       		         ;error
	   ;(15 a "software termination signal")         ;exit
	   ;(16 a "urgent condition on socket")          ;default
	   ;(17 a "stop")	                         ;can't-handle
	   ;(18 a "stop signal generated from keyboard") ;default
	   ;(19 a "continue after stop")                 ;default
	   ;(20 a "child status has changed")            ;default
	   ;(21 a "background read attempted")	         ;default
	   ;(22 a "background write attempted")          ;default
	   ;(23 a "i/o is possible")		         ;default
	    (24 a "cpu time limit exceeded")	         ;error
	    (25 a "file size limit exceeded")	         ;error
	    (26 a "virtual time alarm")		         ;error
	    (27 a "profiling timer alarm")               ;error
	    ))

(define (initialize-interrupt-handlers)
  (do ((l *signals* (cdr l)))
      ((null? l))
    (let ((z (car l)))
      (set-signal-handler (car z)
			  (lambda (frame)
			    (signal-unblock (get-signal-number frame))
			    (error (caddr z))
			    (xcase (cadr z)
				   ((s) (not-proceedable))
				   ((a) nil)))))))

;;; Special cases to deal with.

(define-constant SIGINT  2)
(define-constant SIGQUIT 3)
(define-constant SIGTERM 15)

(set-signal-handler SIGINT
      (lambda (frame)
	(signal-unblock (get-signal-number frame))
	(breakpoint "Interrupt")))

(set-signal-handler SIGQUIT
      (lambda (frame)
	(signal-unblock (get-signal-number frame))
	(zbreakpoint)))

(set-signal-handler SIGTERM
      (lambda (frame)
	(ignore frame)
	(set-signal-handler SIGTERM 'default)
	(exit)))


;;; LIM_STACK = 4
;;; Defaults to 512000 bytes.
;;; **RESET** was determined empirically to be about 4692 bytes into the
;;; stack, well within the margin of 8K.

(lset *stack-margin* 1000)              ; 8000 bytes

(lset *stack-guard* 0)

(define (initialize-stack-guard)
  (let ((lim (pointer-address (call-xenoid *vlimit-xenoid*
                                           (fixnum->pointer -1)
                                           (fixnum->pointer 4)))))
    (set *stack-guard*
         (fx- (pointer-address (escape-procedure-frame **reset**))
              (fx- lim (fx* *stack-margin* 3))))
    (reset-stack-guard)))

(define (reset-stack-guard)             ; Called by top level (see REPL.T)
  (xset (the-slink) %%stack-limit-index
        *stack-guard*))

(define (*icall-stack-overflow-fault* frame type)
  (ignore frame type)
  (set *re-enter-on-fault?* t)
  (xset (the-slink) %%stack-limit-index
        (fx- (xref (the-slink) %%stack-limit-index)
             *stack-margin*))
  (set *stack-overflow-pending?* nil)
  (error "stack overflow"))
