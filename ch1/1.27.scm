(load "../libs/init.scm")

(define flatten
  (lambda (lst)
    (if (null? lst)
	'()
	(if (list? (car lst))
	    (cons (flatten (car lst))
		  (flatten (cdr lst)))
	    (cons (car lst)
		  (flatten (cdr lst)))))))
		    

(flatten '(a b c))
(a b c)
(flatten '((a) () (b ()) () (c)))
(a b c)
(flatten '((a b) c (((d)) e)))
(a b c d e)
(flatten '(a b (() (c))))
(a b c)
