(load "../libs/init.scm")

(define filter-in
  (lambda (pred lst)
    (if (null? lst)
	'()
	(if (pred (car lst))
	    (cons (car lst)
		  (filter-in pred (cdr lst)))
	    (filter-in pred (cdr lst))))))


;;(car '(a 2 (1 3) b 7))
;;(number? (car '(a 2 (1 3) b 7)))
(filter-in number? '(a))
(filter-in number? '(1))
(equal?? (filter-in number? '(a 2 (1 3) b 7)) '(2 7))
(equal?? (filter-in symbol? '(a (b c) 17 foo)) '(a foo))
(equal?? (filter-in number? '()) '())
	       
