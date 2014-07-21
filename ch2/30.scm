(load "../libs/init.scm")

(define identifier? symbol?)

(define-datatype lc-expr lc-expr?
  (var-expr
   (var identifier?))
  (lambda-expr
   (bound-var identifier?)
   (body lc-expr?))
  (app-expr
   (rator lc-expr?)
   (rand lc-expr?)))

;; parse-expression : Schemeval -> Lcexp
;; some fix on parse-expression on page 53
(define parse-expression
  (lambda (datum)
    (cond
     ((symbol? datum)
      (when (eqv? datum 'lambda)
	    (error 'parse "lambda is not valid symbol"))
      (var-expr datum))
     ((pair? datum)
      (if (eqv? (car datum) 'lambda)
	  (if (not (= (length datum) 3))
	      (error 'parse "lambda requires args and body")
	      (if (not (list? (cadr datum)))
		  (error 'parse "lamdba's args should be a list")
		  (if (not (= (length (cadr datum)) 1 ))
		      (error 'parse "lamdba's args should contains only one arg")
		      (lambda-expr (car (cadr datum))
				   (parse-expression (caddr datum))))))
	  (if (not (= (length datum) 2))
	      (error 'parse "app-expr contains only rator and rand")
	      (app-expr
	       (parse-expression (car datum))
	       (parse-expression (cadr datum))))))
     (else
      (error 'parse "error for ~s" datum)))))

;;(parse-expression 'lambda) -> error
;;(parse-expression '(a b c)) -> error
(equal?? (parse-expression 'a) '(var-expr a))
(equal?? (parse-expression '(a b)) '(app-expr (var-expr a) (var-expr b)))
(equal?? (parse-expression '(lambda (a) (a b))) '(lambda-expr a (app-expr (var-expr a) (var-expr b))))
