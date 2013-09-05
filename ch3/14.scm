(load-relative "../libs/init.scm")
(load-relative "../libs/environments.scm")


;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)
    (expression (identifier) var-exp)
    (expression (number) const-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("*" "(" expression "," expression ")") mult-exp)
    (expression ("/" "(" expression "," expression ")") div-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
    ))

;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-datatype expression expression?
  (var-exp
   (id symbol?))
  (const-exp
   (num number?))
  (zero?-exp
   (expr expression?))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (if-exp
   (predicate-exp expression?)
   (true-exp expression?)
   (false-exp expression?))
  (minus-exp
   (body-exp expression?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (mult-exp
   (exp1 expression?)
   (exp2 expression?))
  (div-exp
   (exp1 expression?)
   (exp2 expression?))
  (let-exp
   (var symbol?)
   (value expression?)
   (body expression?))
  (bool-exp
   (exp expression?))
  (cond-exp
   (conds (list-of expression?))
   (acts  (list-of expression?))))


;;new stuff
(define cond-val
  (lambda (conds acts env)
    (cond ((null? conds)
           (error 'cond-val "No conditions got into #t"))
          ((not (zero? (value-of (car conds) env)))
           (value-of (car acts) env))
          (else
           (cond-val (cdr conds) (cdr acts) env)))))

(define expval-extractor-error
  (lambda (variant value)
    (error 'expval-extractors "Looking for a ~s, found ~s"
           variant value)))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

(define init-env
  (lambda ()
    (extend-env
     'i 1
     (extend-env
      'v 5
      (extend-env
       'x 10
       (empty-env))))))

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-env))))))

;; used as map for the list
(define apply-elm
  (lambda (env)
    (lambda (elem)
      (value-of elem env))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp (num) num)
           (var-exp (var) (apply-env env var))

           (diff-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (val2 (value-of exp2 env)))
		       (- val1 val2)))
           (add-exp (exp1 exp2)
                    (let ((val1 (value-of exp1 env))
                          (val2 (value-of exp2 env)))
		      (+ val1 val2)))
           (mult-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (val2 (value-of exp2 env)))
		       (* val1 val2)))
           (div-exp (exp1 exp2)
                    (let ((val1 (value-of exp1 env))
                          (val2 (value-of exp2 env)))
		      (/ val1 val2)))
           (zero?-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
			(if (zero? val1)
			    1
			    0)))
	   (equal?-exp (exp1 exp2)
                       (let ((val1 (value-of exp1 env))
                             (val2 (value-of exp2 env)))
			 (if (= val1 val2)
			     1
			     0)))
           (less?-exp (exp1 exp2)
                      (let ((val1 (value-of exp1 env))
                            (val2 (value-of exp2 env)))
			(if (< val1 val2)
			    1
			    0)))
           (greater?-exp (exp1 exp2)
                         (let ((val1 (value-of exp1 env))
                               (val2 (value-of exp2 env)))
			   (if (> val1 val2)
			       1
			       0)))
           (if-exp (exp1 exp2 exp3)
                   (let ((val1 (value-of exp1 env)))
                     (if (not (zero? val1))
                         (value-of exp2 env)
                         (value-of exp3 env))))
           (minus-exp (body-exp)
                      (let ((num (value-of body-exp env)))
			(- 0 num)))
           (let-exp (var exp1 body)
                    (let ((val1 (value-of exp1 env)))
                      (value-of body
                                (extend-env var val1 env))))
           (cond-exp (conds acts)
                     (cond-val conds acts env))
           )))

;;
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(run "x")
(run "v")
(run "i")
(run "10")
(run "-(1, 2)")
(run "-(1, x)")

;; (run "foo") -> error

(run  "if zero?(-(11, 11)) then 3 else 4")
(run "minus(4)")
(run  "if zero?(-(11, 11)) then minus(3) else minus(4)")

(run "+(1, 2)")
(run "+(+(1,2), 3)")
(run "/(1, 2)")
(run "*(*(1,2), *(10, 2))")

(run "if less?(1, 2) then 1 else 2")
(run "if greater?(2, 1) then minus(1) else minus(2)")

(run "1")

(run "less?(1, 2)")
(run "cond less?(1, 2) ==> 2 end")
(run "cond less?(2, 1) ==> 1 greater?(2, 2) ==> 2  greater?(3, 2) ==> 3 end")
;; (run "cond less?(2, 1) ==> 1 end")  ==> error
