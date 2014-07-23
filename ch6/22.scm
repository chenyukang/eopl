
;; Generating better code when K is already proc-exp for (K simp)

(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/cps.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cps-cases.scm")
(load-relative "./base/cps-lang.scm")
(load-relative "./base/base-iterp.scm")

;; make-send-to-cont : SimpleExp * SimpleExp -> TfExp
(define make-send-to-cont
  (lambda (cont bexp)
    (cases simple-expression cont
	     (cps-proc-exp (vars body)
			   (cps-let-exp (car vars) bexp body))
	     (else
	      (cps-call-exp cont (list bexp))))))


(run-all)
