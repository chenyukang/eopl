(load "../libs/init.scm")

(define down
  (lambda (list)
    (if (null? list)
	'()
	(cons
	 (cons (car list) '())
	 (down (cdr list))))))


(equal?? (down '(a b)) '((a) (b)))
(equal?? (down '(a)) '((a)))

(equal?? (down '(a (more (complicated)) object))
	 '((a) ((more (complicated))) (object)))
