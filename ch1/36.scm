(load "../libs/init.scm")

(define number-elements
	(lambda (lst)
		(if (null? lst) '()
			(g (list 0 (car lst)) (number-elements (cdr lst))))))

(define g
	(lambda (node lst)
		(if (null? lst)
			(list node)
			(cons node
				(g-plus-one (car lst) (cdr lst))))))

(define g-plus-one
	(lambda (node lst)
		(if (null? lst)
			(list (list (+ (car node) 1) (cadr node)))
			(cons (list (+ (car node) 1) (cadr node))
				(g-plus-one (car lst) (cdr lst))))))

(equal?? (number-elements '(a b c d)) '((0 a) (1 b) (2 c) (3 d)))

(equal?? (number-elements '(az by cx dw ev)) '((0 az) (1 by) (2 cx) (3 dw) (4 ev)))
