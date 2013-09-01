(load "../libs/init.scm")


;; I won't give a picture for the AST, but to construct such too AST, the code is
;; as below

(define id?
  (lambda (symbol)
    (not (and (symbol? symbol)
              (eqv? symbol 'lambda)))))

(define-datatype lc-expr lc-expr?
  (var-expr
   (var id?))
  (lambda-expr
   (bound-var id?)
   (body      lc-expr?))
  (app-expr
   (rator lc-expr?)
   (rand  lc-expr?)))


(define expA
  (app-expr
   (lambda-expr
	       (var-expr 'a)
	       (var-expr 'b))
   (var-expr 'c)))

(define expB
  (lambda-expr 'x
	      (lambda-expr 'y
			  (lambda-expr 'x
				      (app-expr
				       (lambda-expr 'x
						   (app-expr (var-expr 'x)
							    (var-expr 'y)))
				       (var-expr 'x))))))
