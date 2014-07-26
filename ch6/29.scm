;; Consider this variant of cps-of-exps
(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/cps.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cps-cases.scm")
(load-relative "./base/cps-lang.scm")
(load-relative "./base/base-iterp.scm")

;; This version of cps-of-exps is continuation passing style,
;; like the optimized version fact, acc used as cont.

(define cps-of-exps
  (lambda (exps builder)
    (let cps-of-rest ((exps exps) (acc '()))
      ;; cps-of-rest : Listof(InpExp) × Listof(SimpleExp) → TfExp
      (cond
       ((null? exps) (builder (reverse acc)))
       ((inp-exp-simple? (car exps))
	(cps-of-rest (cdr exps)
		     (cons
		      (cps-of-simple-exp (car exps))
		      acc)))
       (else
	(let ((var (fresh-identifier 'var)))
	  (cps-of-exp (car exps)
		      (cps-proc-exp
		       (list var)
		       (cps-of-rest
			(cdr exps)
			(cons (cps-of-simple-exp (var-exp var)) acc))))))))))

(run-all)
