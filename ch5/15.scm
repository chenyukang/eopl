(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/letrec-cases.scm")

;; use list for represent a continuation
;; based on 14
;; seems meanless represent as list,
;; it's not diffcult, but just using list implemented the datetype of continuation
;; the apply-cont seems complicated.

 ;;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
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

    (expression (number) const-exp)

    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("*" "(" expression "," expression ")")
     multi-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)

    (expression
     ("letrec"
      identifier "(" (arbno identifier) ")" "=" expression
      "in" expression)
     letrec-exp)

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))


(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?)))

;;; extractors:

(define expval->num
  (lambda (v)
    (cases expval v
           (num-val (num) num)
           (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
           (bool-val (bool) bool)
           (else (expval-extractor-error 'bool v)))))

(define expval->proc
  (lambda (v)
    (cases expval v
           (proc-val (proc) proc)
           (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (error 'expval-extractors "Looking for a ~s, found ~s"
           variant value)))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;
(define identifier? symbol?)

(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont
   (saved-cont continuation?))
  (let-exp-cont
   (var identifier?)
   (body expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (diff1-cont
   (exp2 expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (diff2-cont
   (val1 expval?)
   (saved-cont continuation?))
  (multi1-cont
   (exp2 expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (multi2-cont
   (val1 expval?)
   (saved-cont continuation?))
  (rator-cont
   (rands (list-of expression?))
   (saved-vals (list-of expression?))
   (saved-env environment?)
   (saved-cont continuation?))
  (rand-cont
   (val1 expval?)
   (rands (list-of expression?))
   (saved-vals (list-of expval?))
   (saved-env environment?)
   (saved-cont continuation?)))

;; new stuff, to compute the size of a continuation
(define continuation-size
  (lambda (saved-size cont)
    (cases continuation cont
	   (end-cont ()
		     (+ saved-size 1))
	   (zero1-cont (saved-cont)
		       (continuation-size (+ saved-size 1)
					  saved-cont))
	   (let-exp-cont (v b env saved-cont)
			 (continuation-size (+ saved-size 1)
					    saved-cont))
	   (if-test-cont (e2 e3 env saved-cont)
			 (continuation-size (+ saved-size 1)
					    saved-cont))
	   (diff1-cont (e2 env saved-cont)
		       (continuation-size (+ saved-size 1)
					  saved-cont))
	   (diff2-cont (v1 saved-cont)
		       (continuation-size (+ saved-size 1)
					  saved-cont))
	   (multi1-cont (e2 env saved-cont)
		       (continuation-size (+ saved-size 1)
					  saved-cont))
	   (multi2-cont (v1 saved-cont)
			(continuation-size (+ saved-size 1)
					   saved-cont))
	   (rator-cont (rs ss env saved-cont)
		       (continuation-size (+ saved-size 1)
					  saved-cont))
	   (rand-cont (p rs ss env saved-cont)
		      (continuation-size (+ saved-size 1)
					 saved-cont)))))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (bvar (list-of symbol?))
   (body expression?)
   (env environment?)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec
   (p-name symbol?)
   (b-var (list-of symbol?))
   (p-body expression?)
   (saved-env environment?)))

(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;
(define apply-env
  (lambda (env search-sym)
    (cases environment env
           (empty-env ()
                      (error 'apply-env "No binding for ~s" search-sym))
           (extend-env (var val saved-env)
                       (if (eqv? search-sym var)
                           val
                           (apply-env saved-env search-sym)))
           (extend-env-rec (p-name b-var p-body saved-env)
                           (if (eqv? search-sym p-name)
                               (proc-val (procedure b-var p-body env))
                               (apply-env saved-env search-sym))))))


;; value-of-program : Program -> FinalAnswer
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of/k exp1 (init-env) (end-cont))))))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
           (const-exp (num) (apply-cont cont (num-val num)))
           (var-exp (var) (apply-cont cont (apply-env env var)))
           (proc-exp (vars body)
                     (apply-cont cont
                                 (proc-val (procedure vars body env))))
           (letrec-exp (p-name b-vars p-body letrec-body)
                       (value-of/k letrec-body
                                   (extend-env-rec p-name b-vars p-body env)
                                   cont))
           (zero?-exp (exp1)
                      (value-of/k exp1 env
                                  (zero1-cont cont)))
           (let-exp (var exp1 body)
                    (value-of/k exp1 env
                                (let-exp-cont var body env cont)))
           (if-exp (exp1 exp2 exp3)
                   (value-of/k exp1 env
                               (if-test-cont exp2 exp3 env cont)))
           (diff-exp (exp1 exp2)
                     (value-of/k exp1 env
                                 (diff1-cont exp2 env cont)))
           (call-exp (rator rands)
                     (value-of/k rator env
                                 (rator-cont rands '() env cont)))
           (multi-exp (exp1 exp2)
                      (value-of/k exp1 env
                                  (multi1-cont exp2 env cont)))
           )))

;; apply-cont : Cont * ExpVal -> FinalAnswer
;; Page: 148
(define apply-cont
  (lambda (cont val)
    (begin
      (printf "size: ~a ~%" (continuation-size 0 cont))
      (cases continuation cont
	     (end-cont ()
		       (begin
			 (printf
			  "End of computation.~%")
			 val))
	     (zero1-cont (saved-cont)
			 (apply-cont saved-cont
				     (bool-val
				      (zero? (expval->num val)))))
	     (let-exp-cont (var body saved-env saved-cont)
			   (value-of/k body
				       (extend-env var val saved-env) saved-cont))
	     (if-test-cont (exp2 exp3 saved-env saved-cont)
			   (if (expval->bool val)
			       (value-of/k exp2 saved-env saved-cont)
			       (value-of/k exp3 saved-env saved-cont)))
	     (diff1-cont (exp2 saved-env saved-cont)
			 (value-of/k exp2
				     saved-env (diff2-cont val saved-cont)))
	     (diff2-cont (val1 saved-cont)
			 (let ((num1 (expval->num val1))
			       (num2 (expval->num val)))
			   (apply-cont saved-cont
				       (num-val (- num1 num2)))))
	     ;;new stuff
	     (multi1-cont (exp2 saved-env saved-cont)
			  (value-of/k exp2
				      saved-env (multi2-cont val saved-cont)))

	     (multi2-cont (val1 saved-cont)
			  (let ((num1 (expval->num val1))
				(num2 (expval->num val)))
			    (apply-cont saved-cont
					(num-val (* num1 num2)))))

	     (rator-cont (rands saved-vals saved-env saved-cont)
			 (value-of/k (car rands) saved-env
				     (rand-cont val (cdr rands)
						saved-vals saved-env saved-cont)))

	     (rand-cont (proc-val rands saved-vals saved-env saved-cont)
			(if (null? rands)
			    (let ((proc (expval->proc proc-val)))
			      (apply-procedure/k proc
						 (append saved-vals (list val))
						 saved-cont))
			    (value-of/k (car rands) saved-env
					(rand-cont proc-val (cdr rands)
						   (append saved-vals (list val))
						   saved-env saved-cont))))
	     )))
  )

;;new stuff
(define extend-env*
  (lambda (vars vals saved-env)
    (if (null? vars)
	  saved-env
	  (extend-env* (cdr vars) (cdr vals)
		       (extend-env (car vars) (car vals) saved-env)))))

;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 args cont)
    (cases proc proc1
	     (procedure (vars body saved-env)
			(value-of/k body
				    (extend-env* vars args saved-env)
				    cont)))))
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))


(add-test! '(multi-arg  "(proc(x,y) -(x, y) 10 20)" -10))
(add-test! '(multi-arg-letrec "letrec f(x y) = -(x, y) in (f 11 33)" -22))

(run "*(2, 3)")

;; (define fact
;;   (lambda (n)
;;     (if (zero? n) 1 (* n (fact (- n 1))))))

;; ==>
(run "letrec fact(x) =
         if zero?(x) then 1 else *((fact -(x, 1)), x) in
         (fact 50)")


;; (define fact-iter
;;   (lambda (n a)
;;     (if (zero? n) a (fact-iter (- n 1) (* a n)))))

;; (define fact-v2
;;   (lambda (n)
;;     (fact-iter n 1)))

;; ==>
(run "letrec fact-iter(n a) =
         if zero?(n)
               then a
            else (fact-iter -(n, 1) *(n, a)) in
          letrec fact(n) =
             (fact-iter n 1)
         in (fact 50)")


(run-all)
