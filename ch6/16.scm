;; Registerize the interpreter of figure 6.6
(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/cps.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cps-cases.scm")
(load-relative "./base/cps-lang.scm")

;; New stuff
(define cont 'uninitialized)
(define cur-exp 'uninitialized)

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
(define value-of/k
  (lambda (env)
    (cases tfexp cur-exp
           (simple-exp->exp (simple)
                            (apply-cont (value-of-simple-exp simple env)))
           (cps-let-exp (var rhs body)
                        (let ((val (value-of-simple-exp rhs env)))
			  (set! cur-exp body)
			  (value-of/k
                           (extend-env* (list var) (list val) env))))
	   
           (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
			   (set! cur-exp letrec-body)
                           (value-of/k 
                            (extend-env-rec** p-names b-varss p-bodies env)))
	   
           (cps-if-exp (simple1 body1 body2)
                       (if (expval->bool (value-of-simple-exp simple1 env))
			   (begin
			     (set! cur-exp body1)
			     (value-of/k env))
			   (begin
			     (set! cur-exp body2)
			     (value-of/k env))))
	   
           (cps-call-exp (rator rands)
                         (let ((rator-proc
                                (expval->proc
                                 (value-of-simple-exp rator env)))
                               (rand-vals
                                (map
                                 (lambda (simple)
                                   (value-of-simple-exp simple env))
                                 rands)))
                           (apply-procedure/k rator-proc rand-vals))))))

;; apply-cont : Cont * ExpVal -> Final-ExpVal
;; there's only one continuation, and it only gets invoked once, at
;; the end of the computation.
(define apply-cont
  (lambda (val)
    (cases continuation cont
           (end-cont () val))))

;; apply-procedure/k : Proc * ExpVal * Cont -> ExpVal
(define apply-procedure/k
  (lambda (proc1 args)
    (cases proc proc1
           (procedure (vars body saved-env)
		      (begin 
			(set! cur-exp body)
			(value-of/k 
			 (extend-env* vars args saved-env)))))))

(define instrument-cps (make-parameter #f))

(define value-of-program
  (lambda (pgm)
    (cases cps-out-program pgm
	   (cps-a-program (exp1)
			  (begin
			    (set! cont (end-cont))
			    (set! cur-exp exp1)
			    (value-of/k (init-env)))))))

(define run
  (lambda (string)
    (let ((cpsed-pgm
           (cps-of-program (scan&parse string))))
      (if (instrument-cps) (pretty-print cpsed-pgm))
      (value-of-program cpsed-pgm))))

(run "1")
(run-all)

