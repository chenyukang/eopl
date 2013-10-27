(load-relative "../libs/init.scm")
(load-relative "./test.scm")
(load-relative "./cps.scm")
(load-relative "./data-structures.scm")
(load-relative "./cps-cases.scm")
;;(load-relative "./cps-out.scm")


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
     ("+" "(" (separated-list expression ",") ")")
     sum-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression
     ("letrec"
      (arbno identifier "(" (arbno identifier) ")"
	     "=" expression)
      "in"
      expression)
     letrec-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("proc" "(" (arbno identifier) ")" expression)
     proc-exp)

    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))


  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define cps-out-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define cps-out-grammar

  '((cps-out-program (tfexp) cps-a-program)

    (simple-expression (number) cps-const-exp)

    (simple-expression (identifier) cps-var-exp)

    (simple-expression
     ("-" "(" simple-expression "," simple-expression ")")
     cps-diff-exp)

    (simple-expression
     ("zero?" "(" simple-expression ")")
     cps-zero?-exp)

    (simple-expression
     ("+" "(" (separated-list simple-expression ",") ")")
     cps-sum-exp)

    (simple-expression
     ("proc" "(" (arbno identifier) ")" tfexp)
     cps-proc-exp)

    (tfexp
     (simple-expression)
     simple-exp->exp)

    (tfexp
     ("let" identifier "=" simple-expression "in" tfexp)
     cps-let-exp)

    (tfexp
     ("letrec"
      (arbno identifier "(" (arbno identifier) ")"
	     "=" tfexp)
      "in"
      tfexp)
     cps-letrec-exp)

    (tfexp
     ("if" simple-expression "then" tfexp "else" tfexp)
     cps-if-exp)

    (tfexp
     ("(" simple-expression (arbno simple-expression) ")")
     cps-call-exp)

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes cps-out-lexical-spec cps-out-grammar)

(define cps-show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes cps-out-lexical-spec cps-out-grammar)))

(define cps-out-scan&parse
  (sllgen:make-string-parser cps-out-lexical-spec cps-out-grammar))


(define value-of-program
  (lambda (pgm)
    (cases cps-out-program pgm
	   (cps-a-program (exp1)
			  (value-of/k exp1 (init-env) (end-cont))))))

(define value-of-simple-exp
  (lambda (exp env)
    (cases simple-expression exp
	   (cps-const-exp (num) (num-val num))
	   (cps-var-exp (var) (apply-env env var))

	   (cps-diff-exp (exp1 exp2)
			 (let ((val1
				(expval->num
				 (value-of-simple-exp exp1 env)))
			       (val2
				(expval->num
				 (value-of-simple-exp exp2 env))))
			   (num-val
			    (- val1 val2))))

	   (cps-zero?-exp (exp1)
			  (bool-val
			   (zero?
			    (expval->num
			     (value-of-simple-exp exp1 env)))))

	   (cps-sum-exp (exps)
			(let ((nums (map
				     (lambda (exp)
				       (expval->num
					(value-of-simple-exp exp env)))
				     exps)))
			  (num-val
			   (let sum-loop ((nums nums))
			     (if (null? nums) 0
				 (+ (car nums) (sum-loop (cdr nums))))))))

	   (cps-proc-exp (vars body)
			 (proc-val
			  (procedure vars body env)))

	   )))

;; value-of/k : TfExp * Env * Cont -> FinalAnswer
;; Page: 209
(define value-of/k
  (lambda (exp env cont)
    (cases tfexp exp
	   (simple-exp->exp (simple)
			    (apply-cont cont
					(value-of-simple-exp simple env)))
	   (cps-let-exp (var rhs body)
			(let ((val (value-of-simple-exp rhs env)))
			  (value-of/k body
				      (extend-env* (list var) (list val) env)
				      cont)))
	   (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
			   (value-of/k letrec-body
				       (extend-env-rec** p-names b-varss p-bodies env)
				       cont))
	   (cps-if-exp (simple1 body1 body2)
		       (if (expval->bool (value-of-simple-exp simple1 env))
			   (value-of/k body1 env cont)
			   (value-of/k body2 env cont)))
	   (cps-call-exp (rator rands)
			 (let ((rator-proc
				(expval->proc
				 (value-of-simple-exp rator env)))
			       (rand-vals
				(map
				 (lambda (simple)
				   (value-of-simple-exp simple env))
				 rands)))
			   (apply-procedure/k rator-proc rand-vals cont))))))

;; apply-cont : Cont * ExpVal -> Final-ExpVal
;; there's only one continuation, and it only gets invoked once, at
;; the end of the computation.
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
	   (end-cont () val))))

;; apply-procedure/k : Proc * ExpVal * Cont -> ExpVal
;; Page: 209
(define apply-procedure/k
  (lambda (proc1 args cont)
    (cases proc proc1
	   (procedure (vars body saved-env)
		      (value-of/k body
				  (extend-env* vars args saved-env)
				  cont)))))

(define instrument-cps (make-parameter #f))

(define run
  (lambda (string)
    (let ((cpsed-pgm
	   (cps-of-program (scan&parse string))))
      (if (instrument-cps) (pretty-print cpsed-pgm))
      (value-of-program cpsed-pgm))))

(run-all)

(run "if zero?(0) then 3 else 4")
