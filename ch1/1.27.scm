(load "../libs/init.scm")



(define flatten
  (lambda (x)
    (cond ((null? x) '())
	  ((not (pair? x)) (list x))
	  (else (append (flatten (car x))
			(flatten (cdr x)))))))

(equal?? (flatten '(a b c)) '(a b c))
(equal?? (flatten '(b ())) '(b))
(equal?? (flatten '((a) () (b ()) () (c))) '(a b c))
(equal?? (flatten '((a b) c (((d)) e))) '(a b c d e))
(equal?? (flatten '(a b (() (c)))) '(a b c))
