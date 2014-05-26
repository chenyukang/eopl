#lang eopl

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
	    (eopl:error 'parse "lambda is not valid identifier"))
      (var-expr datum))
     ((pair? datum)
      (if (eqv? (car datum) 'lambda)
          (if (not (= (length datum) 3))
              (eopl:error 'parse "lambda requires args and body")
              (if (not (list? (cadr datum)))
                  (eopl:error 'parse "lamdba's args should be a list")
                  (if (not (= (length (cadr datum)) 1 ))
                      (eopl:error 'parse "lamdba's args should contains only one arg")
                      (lambda-expr (car (cadr datum))
                                   (parse-expression (caddr datum))))))
          (if (not (= (length datum) 2))
              (eopl:error 'parse "app-expr contains only rator and rand")
              (app-expr
               (parse-expression (car datum))
               (parse-expression (cadr datum))))))
     (else
      (eopl:error 'parse "error for ~s" datum)))))

;;(parse-expression 'lambda) -> error
;;(parse-expression '(a b c)) -> error
(parse-expression 'a) ;;#(struct:var-expr a)
(parse-expression '(a b)) ;;#(struct:app-expr #(struct:var-expr a) #(struct:var-expr b))
(parse-expression '(lambda (a) (a b))) ;;#(struct:lambda-expr a #(struct:app-expr #(struct:var-expr a) #(struct:var-expr b)))
(parse-expression 'lambda);; parse: lambda is not valid symbol
(parse-expression '(lambda (a b) c));; parse: lamdba's args should contains only one arg
(parse-expression '(lambda (a) (b c d)));;parse: app-expr contains only rator and rand
