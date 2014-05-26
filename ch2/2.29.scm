#lang eopl

(define id?
  (lambda (symbol)
    (and (symbol? symbol)
         (not (eqv? symbol 'lambda)))))

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
      (eopl:error 'parse "lambda is not a valid id"))
     ((symbol? exp)
      (var-expr exp))
     ((and (pair? exp)
	   (eqv? (car exp) 'lambda))
      (lambda-expr (cadr exp) (parse (caddr exp))))
     ((pair? exp)
      (app-expr (parse (car exp))
		(map parse (cdr exp))))
     (else
      (eopl:error 'parse "parse error")))))

(parse 'a) ;;#(struct:var-expr a)
(parse '(lambda (a) (+ a b)));;#(struct:lambda-expr (a) #(struct:app-expr #(struct:var-expr +) (#(struct:var-expr a) #(struct:var-expr b))))
(parse '(+ a b c));;#(struct:app-expr #(struct:var-expr +) (#(struct:var-expr a) #(struct:var-expr b) #(struct:var-expr c)))

(parse '(a b c));;#(struct:app-expr #(struct:var-expr a) (#(struct:var-expr b) #(struct:var-expr c)))
(parse '(lambda (b) c));;#(struct:lambda-expr (b) #(struct:var-expr c))
