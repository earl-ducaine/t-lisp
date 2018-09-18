(HERALD SETS)

;;; Copyright (c) 1980, 1981 Lawrence Livermore Laboratories
;;; Modifications copyright (c) 1983, 1984 Yale University

;;; Utility operations on sets: ADJOIN, UNION, INTERSECTION, REMQ, SETDIFF

;;; Add an element X to a set S.

(DEFINE (ADJOIN X S)
  (COND ((MEMQ? X S) S)
        (ELSE (CONS X S))))

;;; Union of two sets.

(DEFINE (UNION X Y)
  (COND ((NULL? X) Y)
        (ELSE (DO ((Z Y (CDR Z))
                   (U X (ADJOIN (CAR Z) U)))
                  ((NULL? Z) U)))))

;;; Intersection of two sets.

(DEFINE (INTERSECTION X Y)
  (COND ((NULL? X) '())
        ((NULL? Y) '())
        ((MEMQ (CAR X) Y)
         (CONS (CAR X) (INTERSECTION (CDR X) Y)))
        (ELSE (INTERSECTION (CDR X) Y))))

(DEFINE (INTERSECTION? X Y)
  (COND ((NULL? X) NIL)
        ((NULL? Y) NIL)
        ((MEMQ (CAR X) Y) T)
        (ELSE (INTERSECTION? (CDR X) Y))))

;;; Remove an element X from a set S, non-destructively.

(DEFINE (SETREMQ X S)
  (COND ((NULL? S) S)
        ((EQ? (CAR S) X) (CDR S))
        (ELSE (LET ((Y (SETREMQ X (CDR S))))
                (COND ((EQ? Y (CDR S)) S)
                      (ELSE (CONS (CAR S) Y)))))))

;;; Difference of two sets: (SETDIFF A B) = {X|XA   XB}

(DEFINE (SETDIFF X Y)
  (DO ((Z X (CDR Z))
       (W '() (COND ((MEMQ (CAR Z) Y) W)
                    (ELSE (CONS (CAR Z) W)))))
      ((NULL? Z) W)))
