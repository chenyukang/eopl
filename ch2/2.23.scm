(load "../libs/init.scm")

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
