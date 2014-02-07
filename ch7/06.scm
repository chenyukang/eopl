(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/checked-cases.scm")
(load-relative "./base/store.scm")

;; use some code from 4.21, add store from chapter 4
;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)

    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("proc" "(" identifier ":" type ")" expression)
     proc-exp)

    (expression
     ("(" expression expression ")")
     call-exp)

    (expression
     ("letrec"
      type identifier "(" identifier ":" type ")" "=" expression
      "in" expression)
     letrec-exp)

    (type
     ("int")
     int-type)

    (type
     ("bool")
     bool-type)

    (type
     ("(" type "->" type ")")
     proc-type)

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; type-to-external-form ;;;;;;;;;;;;;;;;

;; type-to-external-form : Type -> List
(define type-to-external-form
  (lambda (ty)
    (cases type ty
           (int-type () 'int)
           (bool-type () 'bool)
           (proc-type (arg-type result-type)
                      (list
                       (type-to-external-form arg-type)
                       '->
                       (type-to-external-form result-type))))))


;; check-equal-type! : Type * Type * Exp -> Unspecified
(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (if (not (equal? ty1 ty2))
        (report-unequal-types ty1 ty2 exp))))

;; report-unequal-types : Type * Type * Exp -> Unspecified
(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (error 'check-equal-type!
	   "Types didn't match: ~s != ~a in~%~a"
	   (type-to-external-form ty1)
	   (type-to-external-form ty2)
	   exp)))

  ;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;

;; type-of-program : Program -> Type
(define type-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1) (type-of exp1 (init-tenv))))))

;; type-of : Exp * Tenv -> Type
(define type-of
  (lambda (exp tenv)
    (cases expression exp
           (const-exp (num) (int-type))

           (var-exp (var) (apply-tenv tenv var))

           (diff-exp (exp1 exp2)
                     (let ((ty1 (type-of exp1 tenv))
                           (ty2 (type-of exp2 tenv)))
                       (check-equal-type! ty1 (int-type) exp1)
                       (check-equal-type! ty2 (int-type) exp2)
                       (int-type)))

           (zero?-exp (exp1)
                      (let ((ty1 (type-of exp1 tenv)))
                        (check-equal-type! ty1 (int-type) exp1)
                        (bool-type)))

           (if-exp (exp1 exp2 exp3)
                   (let ((ty1 (type-of exp1 tenv))
                         (ty2 (type-of exp2 tenv))
                         (ty3 (type-of exp3 tenv)))
                     (check-equal-type! ty1 (bool-type) exp1)
                     (check-equal-type! ty2 ty3 exp)
                     ty2))

           (let-exp (var exp1 body)
                    (let ((exp1-type (type-of exp1 tenv)))
                      (type-of body
                               (extend-tenv var exp1-type tenv))))

           (proc-exp (var var-type body)
                     (let ((result-type
                            (type-of body
                                     (extend-tenv var var-type tenv))))
                       (proc-type var-type result-type)))

           (call-exp (rator rand)
                     (let ((rator-type (type-of rator tenv))
                           (rand-type  (type-of rand tenv)))
                       (cases type rator-type
                              (proc-type (arg-type result-type)
                                         (begin
                                           (check-equal-type! arg-type rand-type rand)
                                           result-type))
                              (else
                               (report-rator-not-a-proc-type rator-type rator)))))

           (letrec-exp (p-result-type p-name b-var b-var-type p-body
                                      letrec-body)
                       (let ((tenv-for-letrec-body
                              (extend-tenv p-name
                                           (proc-type b-var-type p-result-type)
                                           tenv)))
                         (let ((p-body-type
                                (type-of p-body
                                         (extend-tenv b-var b-var-type
                                                      tenv-for-letrec-body))))
                           (check-equal-type!
                            p-body-type p-result-type p-body)
                           (type-of letrec-body tenv-for-letrec-body)))))))

(define report-rator-not-a-proc-type
  (lambda (rator-type rator)
    (error 'type-of-expression
	   "Rator not a proc type:~%~s~%had rator type ~s"
	   rator
	   (type-to-external-form rator-type))))


(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
   (sym symbol?)
   (type type?)
   (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv
  (lambda (tenv sym)
    (cases type-environment tenv
           (empty-tenv-record ()
                              (error 'apply-tenv "Unbound variable ~s" sym))
           (extended-tenv-record (sym1 val1 old-env)
                                 (if (eqv? sym sym1)
                                     val1
                                     (apply-tenv old-env sym))))))

(define init-tenv
  (lambda ()
    (extend-tenv 'x (int-type)
		 (extend-tenv 'v (int-type)
			      (extend-tenv 'i (int-type)
					   (empty-tenv))))))


;; value-of-program : Program -> Expval
(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
	   (a-program (body)
		      (value-of body (init-env))))))


;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp

	   (const-exp (num) (num-val num))

	   (var-exp (var) (deref (apply-env env var)))

	   (diff-exp (exp1 exp2)
		     (let ((val1
			    (expval->num
			     (value-of exp1 env)))
			   (val2
			    (expval->num
			     (value-of exp2 env))))
		       (num-val
			(- val1 val2))))

	   (zero?-exp (exp1)
		      (let ((val1 (expval->num (value-of exp1 env))))
			(if (zero? val1)
			    (bool-val #t)
			    (bool-val #f))))

	   (if-exp (exp0 exp1 exp2)
		   (if (expval->bool (value-of exp0 env))
		       (value-of exp1 env)
		       (value-of exp2 env)))

	   (let-exp (var exp1 body)
		    (let ((val (value-of exp1 env)))
		      (value-of body
				(extend-env var (newref val) env))))

	   (proc-exp (bvar ty body)
		     (proc-val
		      (procedure bvar body env)))

	   (call-exp (rator rand)
		     (let ((proc (expval->proc (value-of rator env)))
			   (arg  (value-of rand env)))
		       (apply-procedure proc arg)))

	   ;;a hacky for using extend-env-rec*
	   (letrec-exp (ty1 p-name b-var ty2 p-body letrec-body)
		       (value-of letrec-body
				 (extend-env-rec* (list p-name)  (list b-var)  (list p-body) env)))

	   )))


(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
           (procedure (var body saved-env)
                      (let ((r (newref arg)))
                        (let ((new-env (extend-env var r saved-env)))
                          (value-of body new-env)))))))


(define check
  (lambda (string)
    (type-to-external-form
     (type-of-program (scan&parse string)))))


;;(clear-test!)
(run-all)
(check-all)
;;;
;;(check      "proc (x : int) proc (f : (int -> (int -> bool))) (f x)")
