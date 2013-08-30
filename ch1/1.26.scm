(load "../libs/init.scm")

(define up
  (lambda (lst)
    (if (null? lst)
	'()
	(let ((now (car lst)))
	  (if (and (list? now) (not (null? now)))
	      (cons (car now)
		    (if (null? (cdr now))
			(up (cdr lst))
			(up (cons (cdr now) (cdr lst)))))
	      (cons now (up (cdr lst))))))))


(equal?? (up '((1 2) (3 4))) '(1 2 3 4))
(equal?? (up '((x (y)) z)) '(x (y) z))
(equal?? (up '()) '())
(equal?? (up '(a b (c))) '(a b c))
