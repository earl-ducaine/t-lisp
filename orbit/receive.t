(herald receive
        (syntax-table *orbit-syntax-table*))

;;; Multiple-value stuff for T 2

(lset **a1** '**a1**)
(lset **a2** '**a2**)
(lset **a3** '**a3**)
(lset **a4** '**a4**)
(lset **a5** '**a5**)

(define (losing-receive **nvals** nvars)
  (error "wrong number of return values - expected ~s, received ~s"
         nvars **nvals**))

(define (receive-values receiver source)
  (let ((return-info (source)))
    (case (car return-info)
	  ((0) (receiver))
	  ((1) (receiver **A1**))
	  ((2) (receiver **A1** **A2**))
	  ((3) (receiver **A1** **A2** **A3**))
	  ((4) (receiver **A1** **A2** **A3** **A4**))
	  ((5) (receiver **A1** **A2** **A3** **A4** **A5**))
	  (else (error "too many values for receive-values to handle")))))
  
