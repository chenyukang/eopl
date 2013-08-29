;;EOPL excerice 2.5
(define empty-env
  (lambda() '()))

(define extend-env
  (lambda (var val env)
    (cons (cons var val)
	  env)))

(define apply-env
  (lambda (env search-var)
    (cond
     ((null? env)
      (report-no-binding-found search-var))
     ((eqv? (caar env) search-var)
      (cdr (car env)))
     (else
      (apply-env (cdr env) search-var)))))

(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "No binding for: " search-var)))

(define e
  (extend-env 'd 6
    (extend-env 'y 8
       (extend-env 'x 7
	  (extend-env 'y 14
	    (empty-env))))))

(define equal??
  (lambda (x y)
    (if (equal? x y)
	(display "pass\n")
	(display "fail\n"))))

(equal?? (apply-env e 'd) 6)
(equal?? (apply-env e 'y) 8)
(equal?? (apply-env e 'x) 7)
(display (apply-env e 'd))
