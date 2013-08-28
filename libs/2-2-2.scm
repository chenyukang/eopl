(let ((time-stamp "Time-stamp: <2000-12-11 16:51:40 wand>"))
  (eopl:printf "2-2-2.scm ~a~%" (substring time-stamp 13 29)))

(define-datatype expression expression? 
  (var-exp
    (id symbol?))
  (lambda-exp
    (id symbol?)
    (body expression?))
  (app-exp
    (rator expression?)
    (rand expression?)))

(define occurs-free?
  (lambda (var exp)
    (cases expression exp
      (var-exp (id) (eqv? id var))
      (lambda-exp (id body)
        (and (not (eqv? id var))
             (occurs-free? var body)))
      (app-exp (rator rand)
        (or (occurs-free? var rator)
            (occurs-free? var rand))))))

(define unparse-expression
  (lambda (exp)
    (cases expression exp
      (var-exp (id) id)
      (lambda-exp (id body) 
        (list 'lambda (list id)
          (unparse-expression body)))
      (app-exp (rator rand)
        (list (unparse-expression rator)
              (unparse-expression rand))))))

(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
         (lambda-exp (caadr datum)
           (parse-expression (caddr datum)))
         (app-exp
           (parse-expression (car datum))
           (parse-expression (cadr datum)))))
      (else (eopl:error 'parse-expression
              "Invalid concrete syntax ~s" datum)))))

