(load-relative "../libs/init.scm")
(load-relative "./base/let-structures.scm")
(load-relative "./base/let-cases.scm")
(load-relative "./base/test.scm")
(load-relative "./base/lang.scm")

;; use procedural representation

(define end-cont
  (lambda ()
    (lambda (val)
      val)))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define zero1-cont
  (lambda (cont)
    (lambda (val)
      (apply-cont cont
		  (bool-val (zero? (expval->num val)))))))


(define let-exp-cont
  (lambda (var body env cont)
    (lambda (val)
      (value-of/k body
		  (extend-env var val env) cont))))

(define if-test-cont
  (lambda (exp2 exp3 env cont)
    (lambda (val)
      (if (expval->bool val)
	  (value-of/k exp2 env cont)
	  (value-of/k exp3 env cont)))))

(define diff1-cont
  (lambda (exp env cont)
    (lambda (val)
      (value-of/k exp env
		  (diff2-cont val cont)))))

(define diff2-cont
  (lambda (val2 cont)
    (lambda (val1)
      (let ((num1 (expval->num val1))
	    (num2 (expval->num val2)))
	(apply-cont cont
		  (num-val (- num2 num1)))))))

(define rator-cont
  (lambda (rand env cont)
    (lambda (rator)
      (value-of/k rand env
		  (rand-cont rator cont)))))

(define rand-cont
  (lambda (rator cont)
    (lambda (rand-val)
      (let ((proc (expval->proc rator)))
	(apply-procedure/k proc rand-val cont)))))


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
	   (proc-exp (var body)
		     (apply-cont cont
				 (proc-val (procedure var body env))))
	   (letrec-exp (p-name b-var p-body letrec-body)
		       (value-of/k letrec-body
				   (extend-env-rec p-name b-var p-body env)
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
	   (call-exp (rator rand)
		     (value-of/k rator env
				 (rator-cont rand env cont)))
	   )))

;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 arg cont)
    (cases proc proc1
	   (procedure (var body saved-env)
		      (value-of/k body
				  (extend-env var arg saved-env)
				  cont)))))

(run-all)
