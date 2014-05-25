#lang eopl

(define identifier?
  (lambda (x)
    (if (and (symbol? x)
	     (not (eqv? x 'lambda)))
	#t
	#f)))

(define-datatype lc-exp lc-exp?
  (var-expr
   (var identifier?))
  (lambda-expr
   (bound-var identifier?)
   (body lc-exp?))
  (app-expr
   (rator lc-exp?)
   (rand lc-exp?)))
(identifier? 'a);;#t
(identifier? 'lambda);;#f
(identifier? 1);;#f
(identifier? '(1 2));;#f
