#lang eopl


;; I won't give a picture for the AST, but to construct such too AST, the code is
;; as below

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


(define expA
  (app-expr
      (lambda-expr
	      'a
	       (var-expr 'b))
   (var-expr 'c)))
;#(struct:app-expr #(struct:lambda-expr a #(struct:var-expr b)) #(struct:var-expr c))
(define expB
  (lambda-expr 'x
	      (lambda-expr 'y
			  (lambda-expr 'x
				      (app-expr
				       (lambda-expr 'x
						   (app-expr (var-expr 'x)
							    (var-expr 'y)))
				       (var-expr 'x))))))
;;#(struct:lambda-expr
;;  x
;;  #(struct:lambda-expr
;;    y
;;    #(struct:lambda-expr
;;      x
;;      #(struct:app-expr
;;        #(struct:lambda-expr x #(struct:app-expr #(struct:var-expr x) #(struct:var-expr y)))
;;        #(struct:var-expr x)))))
