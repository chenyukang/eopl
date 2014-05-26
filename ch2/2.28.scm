#lang eopl
(require racket/format)

(define id?
  (lambda (symbol)
    (and (symbol? symbol)
	 (not (eqv? symbol 'lambda)))))

(define-datatype lc-expr lc-expr?
  (var-expr
   (var id?))
  (lambda-expr
   (bound-var id?)
   (body      lc-expr?))
  (app-expr
   (rator lc-expr?)
   (rand  lc-expr?)))

(define unparse
  (lambda (exp)
    (cases lc-expr exp
	 (var-expr (var)
		   (symbol->string var))
	 (lambda-expr (bound-var body)
		      (~a (list "lambda (" bound-var ")" (unparse body))))
	 (app-expr (rator rand)
		   (~a (list (unparse rator) (unparse rand)))))))

(define expA (var-expr 'a))
(define expB (var-expr 'b))
(define app (app-expr expA expB))
(define lexp (lambda-expr 'a app))
(unparse app);;"(a b)"
(unparse lexp);;"(lambda ( a ) (a b))"


(define lexp2
  (lambda-expr 'x
       (lambda-expr 'y
            (lambda-expr 'x
                (app-expr
                      (lambda-expr 'x
				   (app-expr (var-expr 'x)
					     (var-expr 'y)))
		      (var-expr 'x))))))

(unparse lexp2);;"(lambda ( x ) (lambda ( y ) (lambda ( x ) ((lambda ( x ) (x y)) x))))"
