(load "../libs/init.scm")

(define exists?
  (lambda (pred lst)
    (if (null? lst)
	#f
	(if (pred (car lst))
	    #t
	    (exists? pred (cdr lst))))))


(equal?? (exists? number? '(a b c 3 e)) #t)
(equal?? (exists? number? '(a b c d e)) #f)


