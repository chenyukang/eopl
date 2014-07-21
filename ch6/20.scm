;; Our procedure cps-of-exps causes subexpressions to be evaluated from
;; left to right. Modify cps-of-exps so that subexpressions are evaluated
;; from right to left.

(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/cps.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cps-cases.scm")
(load-relative "./base/cps-lang.scm")
(load-relative "./base/base-iterp.scm")


;; New stuff, return the last index which pred is succeed
(define last-index
  (lambda (pred lst)
    (let ((res #f))
      (begin 
	(do ((i (- (length lst) 1)
		(- i 1)))
	    ((= i -1))
	  (if (pred (list-ref lst i))
	      (set! res i))))
      res)))


;; cps-of-exps : Listof(InpExp) * (Listof(InpExp) -> TfExp)
;;                -> TfExp
(define cps-of-exps
  (lambda (exps builder)
    (let cps-of-rest ((exps exps))
      (let ((pos (last-index
                  (lambda (exp)
                    (not (inp-exp-simple? exp)))
                  exps)))
        (if (not pos)
            (builder (map cps-of-simple-exp exps))
            (let ((var (fresh-identifier 'var)))
              (cps-of-exp
               (list-ref exps pos)
               (cps-proc-exp (list var)
                             (cps-of-rest
                              (list-set exps pos (var-exp var)))))))))))


(run-all)
