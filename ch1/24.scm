(load "../libs/init.scm")

(define every?
  (lambda (pred lst)
    (if (null? lst)
	#t
	(if (not (pred (car lst)))
	    #f
	    (every? pred (cdr lst))))))

(equal?? (every? number? '(a b c 3 e)) #f)
(equal?? (every? number? '(1 2 3 4 5)) #t)
	    
