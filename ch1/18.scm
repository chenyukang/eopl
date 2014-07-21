(load "../libs/init.scm")

(define swapper
  (lambda (l r list)
    (if (null? list)
	'()
	(let ((now (car list)))
	  (if (symbol? now)
	      (cond
	       ((equal? now l)
		(cons r (swapper l r (cdr list))))
	       ((equal? now r)
		(cons l (swapper l r (cdr list))))
	       (else
		(cons now (swapper l r (cdr list)))))
	       (cons (swapper l r now)
		     (swapper l r (cdr list))))))))

(equal?? (swapper 'a 'd '(a b c d)) '(d b c a))

(equal?? (swapper 'a 'd '(a d () c d))
	 '(d a () c a))

(equal?? (swapper 'x 'y '((x) y (z (x))))
	 '((y) x (z (y))))
