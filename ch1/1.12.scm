(load "../libs/init.scm")

(define subst
  (lambda (new old slist)
    (if (null? slist)
	'()
	(cons
	 (let ((sexp (car slist)))
	   (if (symbol? sexp)
	       (if (eqv? sexp old)
		   new
		   sexp)
	       '()))
	 (subst new old (cdr slist))))))




(equal?? (subst 'a 'b '(a b c d e)) '(a a c d e))
(equal?? (subst 'a 'b '(b)) '(a))
(equal?? (subst 'a 'b '(b b b)) '(a a a))
