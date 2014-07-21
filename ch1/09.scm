(load "../libs/scheme48-init.scm")

(define remove
  (lambda (s los)
    (if (null? los)
	'()
	(if (eqv? (car los) s)
	    (remove s (cdr los))
	    (cons (car los) (remove s (cdr los)))))))

(equal?? (remove 'a '(a a b c e a)) '(b c e))
(equal?? (remove 'a '(a a)) '())
