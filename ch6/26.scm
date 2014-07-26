;; Modify make-send-to-cont so that it generates T[simp1/var1],
;; where the notation E1[E2/var] means expression E1 with every free
;; occurrence of the variable var replaced by E2.

(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/cps.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cps-cases.scm")
(load-relative "./base/cps-lang.scm")
(load-relative "./base/base-iterp.scm")

;; TODO ..........

(define debug-print-exp
  (lambda (exp)
    (cases simple-expression exp
           (cps-var-exp (var)
                        (print "now cps-var-exp:" exp))
           (else
            (print "now else:" exp)))))

(define replace-cont-variable
  (lambda (exp var replace-var)
    (cases simple-expression exp
           (cps-const-exp (num) exp)
           (cps-var-exp (var1)
			(if (equal? var1 var)
			replace-var
			exp))
           (cps-diff-exp (exp1 exp2)
                     (cps-diff-exp
	   	      (replace-cont-variable exp1 var replace-var)
	   	      (replace-cont-variable exp2 var replace-var)))
           (cps-zero?-exp (exp1)
	   	      (cps-zero?-exp
	   	       (replace-cont-variable
	   		exp1 var replace-var)))
           (cps-proc-exp (ids exp)
	   	     (cps-proc-exp
	   	      ids
	   	      (replace-cont-variable
	   	       exp
	   	       var
	   	       replace-var)))
           (cps-sum-exp (exps)
                    (cps-sum-exp
	   	     (map (lambda (exp)
	   		    (replace-cont-variable
	   		     exp
	   		     var
	   		     replace-var)) exps)))
           (else
            (error "replace-cont-variable:" exp)))))


;; make-send-to-cont : SimpleExp * SimpleExp -> TfExp
(define make-send-to-cont
  (lambda (cont bexp)
    (cases simple-expression cont
           (cps-proc-exp (vars body)
			 (begin
			   (print "var:" (car vars))
			   (print "now:" bexp)
			   (print "body:" body)
			   (print "cont:" cont)
			   (debug-print-exp (cadr body))
			   (replace-cont-variable
			     (cadr body)
			     (car vars)
			     bexp)))
			   ;;(cps-call-exp cont (list bexp))))
           (else
            (cps-call-exp cont (list bexp))))))


(run "if zero?(0) then 3 else 4")
;;(run-all)
