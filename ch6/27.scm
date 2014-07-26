
(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/cps.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cps-cases.scm")
(load-relative "./base/cps-lang.scm")
(load-relative "./base/base-iterp.scm")

;; TODO

;; cps-of-let-exp : Var * InpExp * InpExp * SimpleExp -> TfExp
(define cps-of-let-exp
  (lambda (id rhs body k-exp)
    (cps-of-exps (list rhs)
                 (lambda (new-rands)
                   (cps-let-exp id
                                (car new-rands)
                                (cps-of-exp body k-exp))))))
