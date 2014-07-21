(load "../libs/init.scm")

;; using insert sort here

(define insert
  (lambda (lst elem)
    (cond ((null? lst) (list elem))
	  ((< elem (car lst))
	   (cons elem lst))
	  (else (cons (car lst)
		      (insert (cdr lst) elem))))))

(define sort-rec
  (lambda (prev now)
    (if (null? now)
	prev
	(sort-rec (insert prev (car now))
		  (cdr now)))))
(define sort
  (lambda (lst)
    (sort-rec '() lst)))


(equal?? (sort '()) '())
(equal?? (sort '(1 2 3 4)) '(1 2 3 4))
(equal?? (sort '(4 3 2 1)) '(1 2 3 4))
(equal?? (sort '(8 2 5 2 3)) '(2 2 3 5 8))
