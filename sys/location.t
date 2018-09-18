(HERALD (TSYS LOCATION T 20)
        (ENV TSYS))

;;; Copyright (c) 1983, 1984 Yale University

;;;; MODIFY-LOCATION and friends

;;; In
;;;     (MODIFY-LOCATION FORM (LAMBDA (FETCH STORE) VALUE))
;;; the continuation is called, being passed an access procedure FETCH of no
;;; arguments which fetches the value in the location indicated by
;;; FORM, and an update procedure STORE of one argument, which will store a
;;; new value in that place.

(DEFINE-SYNTAX (MODIFY-LOCATION FORM CONT)
  (COND ((SYMBOL? FORM)
         (LET ((NEW-VALUE (GENERATE-SYMBOL 'VALUE)))
           `(,CONT (,(t-syntax 'LAMBDA) () ,FORM)
                   (,(t-syntax 'LAMBDA) (,NEW-VALUE)
					(,(t-syntax 'SET) ,FORM ,NEW-VALUE)))))
        ;; ATOM case should err.
        ;; What about special form case?
        (ELSE
         (LET ((NEW-FORM (MAP (LAMBDA (SUBFORM)
                                 (IGNORE SUBFORM)
                                 (GENERATE-SYMBOL 'SUBFORM))
                              FORM)))
           `(,(t-syntax 'LET) ,(MAP LIST NEW-FORM FORM)
              (,CONT (,(t-syntax 'LAMBDA) () ,NEW-FORM)
                     (,(t-syntax 'LAMBDA) (NEW-VALUE)
			  (,(t-syntax 'SET) ,NEW-FORM NEW-VALUE))))))))

(DEFINE-SYNTAX (MODIFY FORM PROC)
  (LET ((FETCH (GENERATE-SYMBOL 'FETCH))
        (STORE (GENERATE-SYMBOL 'STORE)))
    `(,(t-syntax 'MODIFY-LOCATION)
      ,FORM
      (,(t-syntax 'LAMBDA) (,FETCH ,STORE)
			   (,STORE (,PROC (,FETCH)))))))

(DEFINE-SYNTAX (SWAP FORM NEW-VALUE)
  (LET ((FETCH (GENERATE-SYMBOL 'FETCH))
        (STORE (GENERATE-SYMBOL 'STORE)))
    `(,(t-syntax 'MODIFY-LOCATION)
      ,FORM
      (,(t-syntax 'LAMBDA) (,FETCH ,STORE)
			   (,(t-syntax 'BLOCK0) (,FETCH)
						(,STORE ,NEW-VALUE))))))

(DEFINE-SYNTAX (EXCHANGE FORM-1 FORM-2)
  (LET ((FETCH-1 (GENERATE-SYMBOL 'FETCH))
        (STORE-1 (GENERATE-SYMBOL 'STORE))
        (FETCH-2 (GENERATE-SYMBOL 'FETCH))
        (STORE-2 (GENERATE-SYMBOL 'STORE)))
    `(,(t-syntax 'MODIFY-LOCATION) ,FORM-1
       (,(t-syntax 'LAMBDA) (,FETCH-1 ,STORE-1)
         (,(t-syntax 'MODIFY-LOCATION) ,FORM-2
           (,(t-syntax 'LAMBDA) (,FETCH-2 ,STORE-2)
             (,STORE-1 (,(t-syntax 'BLOCK0) (,FETCH-2)
                               (,STORE-2 (,FETCH-1))))))))))

(DEFINE-SYNTAX (INCREMENT FORM)
  `(,(t-syntax 'MODIFY) ,FORM 1+))

(DEFINE-SYNTAX (DECREMENT FORM)
  `(,(t-syntax 'MODIFY) ,FORM -1+))

(DEFINE-SYNTAX (PUSH FORM THING)
  (LET ((FETCH (GENERATE-SYMBOL 'FETCH))
        (STORE (GENERATE-SYMBOL 'STORE)))
    `(,(t-syntax 'MODIFY-LOCATION) ,FORM
       (,(t-syntax 'LAMBDA) (,FETCH ,STORE)
         (,STORE (CONS ,THING (,FETCH)))))))

(DEFINE-SYNTAX (POP FORM)
  `(,(t-syntax 'MODIFY-LOCATION) ,FORM
     (,(t-syntax 'LAMBDA) (FETCH STORE)
       (,(t-syntax 'LET) ((TEMP (FETCH)))
         (STORE (CDR TEMP))
         (CAR TEMP)))))
