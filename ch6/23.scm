;; Observe that our rule for if makes two copies of the continuation K, so in a nested
;; if the size of the transformed program can grow exponentially. Run an example to
;; confirm this observation. Then show how this may be avoided by changing the
;; transformation to bind a fresh variable to K

(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/cps.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cps-cases.scm")
(load-relative "./base/cps-lang.scm")
(load-relative "./base/base-iterp.scm")

(define debug-cps-if-issue (make-parameter #t))

;; Add a debug flag, and print out the transformed program
;; cps-of-program : InpExp -> TfExp
(define cps-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (cps-a-program
                       (let ((res
                              (cps-of-exps (list exp1)
                                           (lambda (new-args)
                                             (simple-exp->exp (car new-args))))))
                         (begin
                           (if (debug-cps-if-issue)
			       (print "\ntransformed : " res))
                           res)))))))


;; cps-of-if-exp : InpExp * InpExp * InpExp * SimpleExp -> TfExp
(define cps-of-if-exp
  (lambda (exp1 exp2 exp3 k-exp)
    (cps-of-exps (list exp1)
		 (lambda (new-rands)
		   (let ((var (fresh-identifier 'var)))
		     (cps-call-exp
		      (cps-proc-exp (list var)
			   (cps-if-exp (car new-rands)
				 (cps-of-exp exp2 (var-exp var))
				 (cps-of-exp exp3 (var-exp var))))
		      (list k-exp)))))))

;; these cases will grow very quickly
(run "if zero?(0) then 1 else 2")

(run "if zero?(0) then if zero?(0) then 1 else 2 else 3")

(run "if zero?(0) then if zero?(0) then 1 else 2 else if zero?(0) then 1 else 2")

(run "if zero?(1) then if zero?(0) then if zero?(0) then 1 else 2 else if zero?(0)
        then 1 else 2 else if zero?(0) then if zero?(0)
         then 1 else 2 else if zero?(0) then 1 else 2 ")

;; value-of/k : TfExp * Env * Cont -> FinalAnswer
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
