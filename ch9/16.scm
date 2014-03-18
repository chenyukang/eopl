(load-relative "../libs/init.scm")
(load-relative "./base/classes/test.scm")
(load-relative "./base/classes/store.scm")
(load-relative "./base/classes/data-structures.scm")
(load-relative "./base/classes/environments.scm")
(load-relative "./base/classes/lang.scm")
(load-relative "./base/classes/interp.scm")
(load-relative "./base/classes/classes.scm")
(load-relative "./base/classes/class-cases.scm")

;; Extend the interpreter to allow over-loading based on the number
;; of method parameters.


;; won't report error when multi method found
;; for a method name and argument number

;; find-method-with-args-num with name and args-num
(define find-method-with-args-num
  (lambda (m-env name args-num)
    (filter (lambda (m)
	      (cases method (cadr m)
		     (a-method (vars body super-name field-names)
			       (if (and (eq? (car m) name)
					(eq? (length vars) args-num))
				   #t
				   #f))))
	    m-env)))

;; find-method : Sym * Sym -> Method
(define find-method
  (lambda (c-name name args-num)
    (let ((m-env (class->method-env (lookup-class c-name))))
      (let ((find-res (find-method-with-args-num m-env name args-num)))
	(if (not (null? find-res)) (cadar find-res)
	      (report-method-not-found name))))))


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

	   (sum-exp (exp1 exp2)
		    (let ((val1
			   (expval->num
			    (value-of exp1 env)))
			  (val2
			   (expval->num
			    (value-of exp2 env))))
		      (num-val
		       (+ val1 val2))))

	   (zero?-exp (exp1)
		      (let ((val1 (expval->num (value-of exp1 env))))
			(if (zero? val1)
			    (bool-val #t)
			    (bool-val #f))))

	   (if-exp (exp0 exp1 exp2)
		   (if (expval->bool (value-of exp0 env))
		       (value-of exp1 env)
		       (value-of exp2 env)))

	   (let-exp (vars exps body)
		    (if (instrument-let)
			(printf "entering let ~s~%" vars))
		    (let ((new-env
			   (extend-env
			    vars
			    (map newref (values-of-exps exps env))
			    env)))
		      (if (instrument-let)
			  (begin
			    (printf "entering body of let ~s with env =~%" vars)
			    (pretty-print (env->list new-env))
			    (printf "store =~%")
			    (pretty-print (store->readable (get-store-as-list)))
			    (printf "~%")
			    ))
		      (value-of body new-env)))

	   (proc-exp (bvars body)
		     (proc-val
		      (procedure bvars body env)))

	   (call-exp (rator rands)
		     (let ((proc (expval->proc (value-of rator env)))
			   (args (values-of-exps rands env)))
		       (apply-procedure proc args)))

	   (letrec-exp (p-names b-varss p-bodies letrec-body)
		       (value-of letrec-body
				 (extend-env-rec** p-names b-varss p-bodies env)))

	   (begin-exp (exp1 exps)
		      (letrec
			  ((value-of-begins
			    (lambda (e1 es)
			      (let ((v1 (value-of e1 env)))
				(if (null? es)
				    v1
				    (value-of-begins (car es) (cdr es)))))))
			(value-of-begins exp1 exps)))

	   (assign-exp (x e)
		       (begin
			 (setref!
			  (apply-env env x)
			  (value-of e env))
			 (num-val 27)))


	   (list-exp (exps)
		     (list-val
		      (values-of-exps exps env)))

	   ;; new cases for CLASSES language
	   (new-object-exp (class-name rands)
			   (let ((args (values-of-exps rands env))
				 (args-num (length rands))
				 (obj (new-object class-name)))
			     (apply-method
			      (find-method class-name 'initialize args-num)
			      obj
			      args)
			     obj))

	   (self-exp ()
		     (apply-env env '%self))

	   (method-call-exp (obj-exp method-name rands)
			    (let ((args (values-of-exps rands env))
				  (args-num (length rands))
				  (obj (value-of obj-exp env)))
			      (apply-method
			       (find-method (object->class-name obj) method-name args-num)
			       obj
			       args)))

	   (super-call-exp (method-name rands)
			   (let ((args (values-of-exps rands env))
				 (args-num (length rands))
				 (obj (apply-env env '%self)))
			     (apply-method
			      (find-method (apply-env env '%super) method-name args-num)
			      obj
			      args)))
	   )))


(run "class c1 extends object
      field var
method initialize() set var = 0
method initialize(x) set var = x
method getvar() var
let o1 = new c1() in
begin
send o1 initialize(10);
send o1 getvar()
end
 ")

;; => 10

(run-all)
