(load-relative "../libs/init.scm")
(load-relative "./base/classes/test.scm")
(load-relative "./base/classes/store.scm")
(load-relative "./base/classes/data-structures.scm")
(load-relative "./base/classes/environments.scm")
(load-relative "./base/classes/lang.scm")
(load-relative "./base/classes/interp.scm")
(load-relative "./base/classes/classes.scm")
(load-relative "./base/classes/class-cases.scm")

;; see new stuff, store class-inst in object,
;;; will save the time for lookup-class, but cost much more memory.

(define-datatype object object?
  (an-object
   (class-inst class?)
   (fields (list-of reference?))))

;; new-object : ClassName -> Obj
(define new-object
  (lambda (class-name)
    (an-object
     (lookup-class class-name)
     (map
      (lambda (field-name)
        (newref (list 'uninitialized-field field-name)))
      (class->field-names (lookup-class class-name))))))


;; new stuff
(define find-method-from-obj
  (lambda (obj name)
    (cases object obj
	   (an-object (class-inst fields)
		      (let ((m-env (class->method-env class-inst)))
			(let ((maybe-pair (assq name m-env)))
			  (if (pair? maybe-pair) (cadr maybe-pair)
			      (report-method-not-found))))))))

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
				 (obj (new-object class-name)))
			     (apply-method
			      ;; new stuff
			      (find-method-from-obj obj 'initialize)
			      obj
			      args)
			     obj))

	   (self-exp ()
		     (apply-env env '%self))

	   (method-call-exp (obj-exp method-name rands)
			    (let ((args (values-of-exps rands env))
				  (obj (value-of obj-exp env)))
			      (apply-method
			       ;; new stuff
			       (find-method-from-obj obj method-name)
			       obj
			       args)))

	   (super-call-exp (method-name rands)
			   (let ((args (values-of-exps rands env))
				 (obj (apply-env env '%self)))
			     (apply-method
			      (find-method (apply-env env '%super) method-name)
			      obj
			      args)))
	   )))

(run "class aclass extends object
       field i
        method initialize(x) set i = x
        method m(y) -(i,-(0,y))

      let o1 = new aclass(3)
       in send o1 m(2)")

(run-all)
