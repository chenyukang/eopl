;; For list-sum,formulate a succinct representation of the continuations,
;; like the one for fact/k above.

(define list-sum
  (lambda (lst)
    (list-sum/k lst 0)))

(define list-sum/k
  (lambda (lst cont)
    (if (null? lst)
	cont
	(list-sum/k (cdr lst)
		    (+ (car lst) cont)))))

(list-sum '())
(list-sum '(1))
(list-sum '(1 2 3))


