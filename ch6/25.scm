(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/cps.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cps-cases.scm")
(load-relative "./base/cps-lang.scm")
(load-relative "./base/base-iterp.scm")

;; Extend CPS-IN so that a let expression can declare an arbitrary
;; number of variables (exercise 3.16).

;; not a perfect solution, cps-let-exp still keep same
;; multiple args are transformed to multiple cps-let-exp

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

    ;; New stuff
    (expression
     ("let" (arbno identifier "=" expression) "in" expression)
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

    ;; New stuff
    (tfexp
     ("let" identifier "=" simple-expression "in" tfexp)
     cps-let-exp)

    (tfexp
     ("letrec"
      (arbno identifier "(" (arbno identifier) ")" "=" tfexp)
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

;;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
(sllgen:make-define-datatypes cps-out-lexical-spec cps-out-grammar)

(define cps-show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes cps-out-lexical-spec cps-out-grammar)))

(define cps-out-scan&parse
  (sllgen:make-string-parser cps-out-lexical-spec cps-out-grammar))


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


;; cps-of-exp : Exp * SimpleExp -> TfExp
(define cps-of-exp
  (lambda (exp cont)
    (cases expression exp
	   (const-exp (num) (make-send-to-cont cont (cps-const-exp num)))
	   (var-exp (var) (make-send-to-cont cont (cps-var-exp var)))
	   (proc-exp (vars body)
		     (make-send-to-cont cont
			(cps-proc-exp (append vars (list 'k%00))
			      (cps-of-exp body (cps-var-exp 'k%00)))))
	   (zero?-exp (exp1)
		      (cps-of-zero?-exp exp1 cont))
	   (diff-exp (exp1 exp2)
		     (cps-of-diff-exp exp1 exp2 cont))
	   (sum-exp (exps)
		    (cps-of-sum-exp exps cont))
	   (if-exp (exp1 exp2 exp3)
		   (cps-of-if-exp exp1 exp2 exp3 cont))
	   ;; New stuff
	   (let-exp (vars exps body)
		    (cps-of-let-exp vars exps body cont))
	   (letrec-exp (ids bidss proc-bodies body)
		       (cps-of-letrec-exp ids bidss proc-bodies body cont))
	   (call-exp (rator rands)
		     (cps-of-call-exp rator rands cont)))))

;; cps-of-let-exp : Var * InpExp * InpExp * SimpleExp -> TfExp
;; New stuff
(define cps-of-let-exp
  (lambda (vars vals body k-exp)
    (let cps-of-rest ((vars vars) (vals vals))
      (let ((var (car vars))
	    (val (car vals))
	    (left-vars (cdr vars))
	    (left-vals (cdr vals)))
      (if (null? left-vars)
	  (cps-of-exps (list val)
		       (lambda (new-rands)
			 (cps-let-exp var
				      (car new-rands)
				      (cps-of-exp body k-exp))))
	  (cps-of-exps (list val)
		       (lambda (new-rands)
			 (cps-let-exp var
			      (car new-rands)
			      (cps-of-rest left-vars left-vals)))))))))


;; new test
(run "let x = 1 y = 2 in 1")
(run "let x = 30
      in let x = -(x,1)
             y = -(x,2)
         in -(x, y)")

(run "let x = 10
       in let x = 20
       in +(x, 10)")

(run-all)
