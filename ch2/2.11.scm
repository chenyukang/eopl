(load "../libs/init.scm")


(define empty-env
  (lambda() '()))


(define extend-env
  (lambda (var val env)
    (cons (list
	   (list var) (list val))
          env)))

(define extend-env*
  (lambda (var-list val-list env)
    (if (null? var-list)
	env
	(cons (list var-list val-list)
	      env))))


(define apply-current
  (lambda (vars vals search-var)
    (if (null? vars)
	(cons #f '())
	(if (eqv? (car vars) search-var)
	    (cons #t (car vals))
	    (apply-current (cdr vars) (cdr vals) search-var)))))

(define apply-env
  (lambda (env search-var)
    (if (null? env)
     (report-no-binding-found search-var)
     (let ((val (apply-current (caar env) (cadar env) search-var)))
       (if (car val) (cdr val)
	   (apply-env (cdr env) search-var))))))

(define report-no-binding-found
  (lambda (search-var)
    (error 'apply-env "No binding for: " search-var)))


(define has-binding?
  (lambda (env var)
    (if (null? env)
	#f
	(let ((val (apply-current (caar env) (cadar env) var)))
	  (if (car val)
	      #t
	      (has-binding? (cdr env) var))))))


(define e (empty-env))
(equal?? e '())

(define e (extend-env 'z 10 e))
(equal?? e '(((z) (10))))

(define e (extend-env* '(a b c d) '(1 2 3 4) e))
(equal?? e '(((a b c d) (1 2 3 4)) ((z) (10))))

(equal?? (apply-env e 'z) 10)
(equal?? (apply-env e 'd) 4)

(equal?? (has-binding? e 'z) #t)
(equal?? (has-binding? e 'd) #t)
(equal?? (has-binding? e 'm) #f)
