
(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/cps.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cps-cases.scm")
(load-relative "./base/cps-lang.scm")
(load-relative "./base/base-iterp.scm")

;; cps-of-exp/ctx : InpExp × (SimpleExp → TfExp) → TfExp
(define cps-of-exp/ctx
  (lambda (exp context)
    (if (inp-exp-simple? exp)
	(context (cps-of-simple-exp exp))
	(let ((var (fresh-identifier 'var)))
	  (cps-of-exp exp
		      (cps-proc-exp (list var)
				    (context (cps-var-exp var))))))))


(define cps-of-diff-exp
    (lambda (exp1 exp2 k-exp)
      (cps-of-exp/ctx exp1
        (lambda (simp1)
          (cps-of-exp/ctx exp2
            (lambda (simp2)
              (make-send-to-cont k-exp
                (cps-diff-exp simp1 simp2))))))))

;; cps-of-zero?-exp : InpExp * SimpleExp -> TfExp
(define cps-of-zero?-exp
  (lambda (exp1 k-exp)
    (cps-of-exp/ctx exp1
		    (lambda (simp1)
		      (make-send-to-cont
		       k-exp
		       (cps-zero?-exp simp1))))))

;; cps-of-if-exp : InpExp * InpExp * InpExp * SimpleExp -> TfExp
(define cps-of-if-exp
  (lambda (exp1 exp2 exp3 k-exp)
    (cps-of-exp/ctx exp1
		    (lambda (simp1)
		      (cps-if-exp simp1
				  (cps-of-exp exp2 k-exp)
				  (cps-of-exp exp3 k-exp))))))

;; cps-of-sum-exp : Listof (InpExp) * SimpleExp -> TfExp
;; keep the same
(define cps-of-sum-exp
  (lambda (exps k-exp)
    (cps-of-exps exps
                 (lambda (simp1)
                   (make-send-to-cont
                    k-exp
                    (cps-sum-exp simp1))))))

;; cps-of-call-exp : InpExp * Listof(InpExp) * SimpleExp -> TfExp
(define cps-of-call-exp
  (lambda (rator rands k-exp)
    (cps-of-exps (cons rator rands)
                 (lambda (new-rands)
                   (cps-call-exp
                    (car new-rands)
                    (append (cdr new-rands) (list k-exp)))))))

(run-all)
