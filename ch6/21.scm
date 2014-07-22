;; Modify cps-of-call-exp so that the operands are evaluated
;; from left to right, followed by the operator.

(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/cps.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cps-cases.scm")
(load-relative "./base/cps-lang.scm")
(load-relative "./base/base-iterp.scm")


(define last
  (lambda (lst)
    (if (null? (cdr lst))
	(car lst)
	(last (cdr lst)))))

(define remove-last-iter
  (lambda (lst cur)
    (if (null? (cdr lst))
	cur
	(remove-last-iter (cdr lst)
			  (append cur (list (car lst)))))))

(define remove-last
  (lambda (lst)
    (remove-last-iter lst '())))


(define cps-of-call-exp
  (lambda (rator rands k-exp)
    (cps-of-exps (cons rands rator)
                 (lambda (new-rands)
		   (cps-call-exp
		    (last new-rands)
		    (append (remove-last new-rands) (list k-exp)))))))

(run "(proc(x) x 3)")
		   


(run "(proc(x) -(x,1)  30)")

(run-all)
