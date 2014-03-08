(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/store.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/environments.scm")
(load-relative "./base/lang.scm")
(load-relative "./base/interp.scm")
(load-relative "./base/classes.scm")
(load-relative "./base/class-cases.scm")

;;;(define debug? (make-parameter #t))


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
  '((program ((arbno class-decl) expression) a-program)

    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("+" "(" expression "," expression ")")
     sum-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" (arbno identifier "=" expression) "in" expression)
     let-exp)

    (expression
     ("proc" "(" (separated-list identifier ",") ")" expression)
     proc-exp)

    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    (expression
     ("letrec"
      (arbno identifier "(" (separated-list identifier ",") ")"
	     "=" expression)
      "in" expression)
     letrec-exp)

    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)

    (expression
     ("set" identifier "=" expression)
     assign-exp)

    (expression
     ("list" "(" (separated-list expression ",") ")" )
     list-exp)

    ;; new stuff for list
    (expression
     ("empty?" "(" expression ")" )
     empty-exp)

    (expression
     ("car" "(" expression ")" )
     car-exp)

    (expression
     ("cdr" "(" expression ")" )
     cdr-exp)

    (expression
     ("cons" "(" expression "," expression ")")
     cons-exp)

    ;; new productions for oop
    (class-decl
     ("class" identifier
      "extends" identifier
      (arbno "field" identifier)
      (arbno method-decl)
      )
     a-class-decl)

    (method-decl
     ("method" identifier
      "("  (separated-list identifier ",") ")" ; method formals
      expression
      )
     a-method-decl)

    (expression
     ("new" identifier "(" (separated-list expression ",") ")")
     new-object-exp)

    ;; this is special-cased to prevent it from mutation
    (expression
     ("self")
     self-exp)

    (expression
     ("send" expression identifier
      "("  (separated-list expression ",") ")")
     method-call-exp)

    (expression
     ("super" identifier    "("  (separated-list expression ",") ")")
     super-call-exp)

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))


;; value-of : Exp * Env -> ExpVal
;; Page: 336 and 337
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

	   (empty-exp (exp)
		      (let ((val (value-of exp env)))
			(cases expval val
			       (list-val (vals)
					 (if (null? vals)
					     (bool-val #t)
					     (bool-val #f)))
			       (else
				(error "empty: error type ~s" exp)))))

	   (car-exp (exp)
		    (let ((val (value-of exp env)))
		      (cases expval val
			       (list-val (vals)
				       (if (null? vals)
					   (error "car: empty list")
					   (car vals)))
			     (else
			      (error "car: error type ~s" exp)))))

	   (cdr-exp (exp)
		    (let ((val (value-of exp env)))
		      (cases expval val
			     (list-val (vals)
				       (if (null? vals)
					   (error "cdr: empty list")
					   (list-val (cdr vals))))
			     (else
			      (error "cdr: error type ~s" exp)))))

	   (cons-exp (arg1 arg2)
		     (let ((val1 (value-of arg1 env))
			   (val2 (value-of arg2 env)))
		       (begin
			 (printf "cons: ~s ~s\n" val1 val2)
		       (cases expval val2
			      (list-val (vals)
					(list-val (cons val1 vals)))
			      (else
			       (error "cons: error type ~s" arg2))))))

	   ;; new cases for CLASSES language
	   (new-object-exp (class-name rands)
			   (let ((args (values-of-exps rands env))
				 (obj (new-object class-name)))
			     (apply-method
			      (find-method class-name 'initialize)
			      obj
			      args)
			     obj))

	   (self-exp ()
		     (apply-env env '%self))

	   (method-call-exp (obj-exp method-name rands)
			    (let ((args (values-of-exps rands env))
				  (obj (value-of obj-exp env)))
			      (apply-method
			       (find-method (object->class-name obj) method-name)
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


(run "car(list(1, 2))")
(run "cdr(list(1, 2, 3))")
(run "empty?(list(1, 2))")
;;(run "empty?(list())")

(run "car(cons(list(1, 2) , list(3, 4)))")

(run "class counter extends object
  field c_count
   method initialize() set c_count = 0
   method countup() set c_count = +(c_count, 1)
   method getcount() c_count

class queue extends object
   field q_in
   field q_out
   field ans
   field count

   method initialize(the_counter)
    begin
      set count = the_counter;  % must do this first, because reset counts.
      send self reset()
     end

   method reset() begin set q_in = list();
                        set q_out = list();
                        send self countup()
                  end

   method qempty?() if empty?(q_in) then empty?(q_out)
                                   else zero?(1)
   method enq(x) begin
                  send self countup();
                  set q_in = cons(x, q_in)
                 end
   method deq()
     letrec reverse(l) = (reverse_help l list())
                 reverse_help(inp, out) = if empty?(inp) then out
                                         else (reverse_help
                                                 cdr(inp) cons(car(inp), out))
      in if send self qempty?() then 0
                                else begin
                                      send self countup();
                                      if empty?(q_out) then
                                        begin set q_out = (reverse q_in);
                                              set q_in = list()
                                        end
                                        else 0;
                                      set ans = car(q_out);
                                      set q_out = cdr(q_out);
                                      ans
                                     end
      method countup()  send count countup()
      method get_total() send count getcount()

let counter1 = new counter() in
let o1 = new queue (counter1)
    o2 = new queue (counter1)
    t1 = 0 t2 = 0 t3 = 0
    t4 = 0 t5 = 0 t6 = 0
    tot1 = 0 tot2 = 0
in begin
       send o1 enq(11);
       send o2 enq(21);
       send o1 enq(12);
       send o2 enq(22);
       set t1 = send o1 deq();
       set t2 = send o1 deq();
       set t3 = send o2 deq();
       set t4 = send o2 deq();
       set t5 = send o1 get_total();
       set t6 = send o2 get_total();
       list(t1,t2,t3,t4,t5,t6)
  end")

;; => (11 12 21 22 10 10))


;;(run-all)
