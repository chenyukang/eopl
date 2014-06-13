(load "../libs/init.scm")

(define id?
  (lambda (symbol)
    (not (and (symbol? symbol)
              (eqv? symbol 'lambda)))))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

;;((list-of number?) '(1 2 3))

(define-datatype lc-expr lc-expr?
  (var-expr
   (var id?))
  (lambda-expr
   (bound-vars (list-of id?))
   (body      lc-expr?))
  (app-expr
   (rator lc-expr?)
   (rands  (list-of lc-expr?))))

(define parse
  (lambda (exp)
    (cond
     ((eqv? exp 'lambda)
      (error 'parse "lambda is not a valid id"))
     ((symbol? exp)
      (var-expr exp))
     ((and (pair? exp)
	   (eqv? (car exp) 'lambda))
      (lambda-expr (cadr exp) (parse (caddr exp))))
     ((pair? exp)
      (app-expr (parse (car exp))
		(map parse (cdr exp))))
     (else
      (error 'parse "parse error")))))


(equal?? (parse 'a) '(var-expr a))
(equal?? (parse '(lambda (a) (+ a b)))
         '(lambda-expr (a) (app-expr (var-expr +) ((var-expr a) (var-expr b)))))
(equal?? (parse '(+ a b c))
         '(app-expr (var-expr +) ((var-expr a) (var-expr b) (var-expr c))))

(parse '(a b c))
(parse '(lambda b c))
