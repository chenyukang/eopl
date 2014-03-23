(load-relative "../libs/init.scm")
(load-relative "./base/typed-oo/lang.scm")
(load-relative "./base/typed-oo/test.scm")
(load-relative "./base/typed-oo/store.scm")
(load-relative "./base/typed-oo/interp.scm")
(load-relative "./base/typed-oo/checker.scm")
(load-relative "./base/typed-oo/environments.scm")
(load-relative "./base/typed-oo/classes.scm")
(load-relative "./base/typed-oo/static-classes.scm")
(load-relative "./base/typed-oo/data-structures.scm")
(load-relative "./base/typed-oo/static-data-structures.scm")
(load-relative "./base/typed-oo/tests.scm")

(define debug? (make-parameter #t))


;; new stuff, add types into proc
(define-datatype proc proc?
  (procedure
   (vars (list-of symbol?))
   (types (list-of type?))
   (body expression?)
   (env environment?)))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
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
     ("proc" "(" (separated-list identifier ":" type ",") ")" expression)
     proc-exp)

    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    (expression
     ("letrec"
      (arbno type identifier "(" (separated-list identifier ":" type ",") ")"
             "=" expression)
      "in" expression)
     letrec-exp)

    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)

    (expression
     ("set" identifier "=" expression)
     assign-exp)

    ;; non-empty lists for typechecked version
    (expression
     ("list" "(" expression (arbno "," expression) ")" )
     list-exp)

    ;; new productions for oop
    (class-decl
     ("class" identifier
      "extends" identifier
      (arbno "implements" identifier)
      (arbno "field" type identifier)
      (arbno method-decl)
      )
     a-class-decl)

    (method-decl
     ("method" type identifier
      "("  (separated-list identifier  ":" type ",") ")" ; method formals
      expression
      )
     a-method-decl)

    ;; new stuff
    (method-decl
     ("staticmethod" type identifier
      "(" (separated-list identifier ":" type ",") ")" ;; static method formals
      expression
      )
     a-static-method-decl)

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

    ;; new productions for typed-oo

    (class-decl
     ("interface" identifier (arbno abstract-method-decl))
     an-interface-decl)


    (abstract-method-decl
     ("method" type identifier
      "("  (separated-list identifier ":" type ",") ")" )
     an-abstract-method-decl)

    (expression
     ("cast" expression identifier)
     cast-exp)

    (expression
     ("instanceof" expression identifier)
     instanceof-exp)

    (type ("int") int-type)
    (type ("bool") bool-type)
    (type ("void") void-type)
    (type
     ("(" (separated-list type "*") "->" type ")")
     proc-type)
    (type
     ("listof" type)
     list-type)

    (type (identifier) class-type) ;; new for typed oo

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; syntactic operations on types ;;;;;;;;;;;;;;;;

(define var-exp-type
  (lambda (env a-exp)
    (cases expression a-exp
	   (var-exp (var)
		    (apply-env env var #t))
	   (else
	    (error 'var-exp-type "expect var-exp, got ~s\n" a-exp)))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp

	   (const-exp (num) (num-val num))

	   (var-exp (var) (deref (apply-env env var #f)))

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
			    '() ;; this is useless
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

	   (proc-exp (bvars types body)
		     (begin
		       (printf "now: ~s ~s\n" bvars types)
		     (proc-val
		      (procedure bvars types body env))))

	   (call-exp (rator rands)
		     (let ((proc (expval->proc (value-of rator env)))
			   (args (values-of-exps rands env)))
		       (apply-procedure proc args)))

	   (letrec-exp (result-types p-names b-varss b-vartypess p-bodies letrec-body)
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
			  (apply-env env x #f)
			  (value-of e env))
			 (num-val 27)))

	   ;; args need to be non-empty for type checker
	   (list-exp (exp exps)
		     (list-val
		      (cons (value-of exp env)
			    (values-of-exps exps env))))

	   (new-object-exp (class-name rands)
			   (let ((args (values-of-exps rands env))
				 (obj (new-object class-name)))
			     (apply-method
			      (find-method class-name 'initialize)
			      obj
			      args)
			     obj))

	   (self-exp ()
		     (apply-env env '%self #f))

	   (method-call-exp (obj-exp method-name rands)
			    (let ((args (values-of-exps rands env))
				  (obj (value-of obj-exp env))
				  (obj-type (var-exp-type env obj-exp)))
			      (begin
				(printf "obj-type: ~s \n" obj-type)
			      (apply-method
			       (find-method (object->class-name obj) method-name)
			       obj
			       args))))

	   (super-call-exp (method-name rands)
			   (let ((args (values-of-exps rands env))
				 (obj (apply-env env '%self #f)))
			     (apply-method
			      (find-method (apply-env env '%super #f) method-name)
			      obj
			      args)))

	   ;; new cases for typed-oo

	   (cast-exp (exp c-name)
		     (let ((obj (value-of exp env)))
		       (if (is-subclass? (object->class-name obj) c-name)
			   obj
			   (report-cast-error c-name obj))))

	   (instanceof-exp (exp c-name)
			   (let ((obj (value-of exp env)))
			     (if (is-subclass? (object->class-name obj) c-name)
				 (bool-val #t)
				 (bool-val #f))))

	   )))



;; apply-procedure : Proc * Listof(ExpVal) -> ExpVal
(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
           (procedure (vars types body saved-env)
                      (let ((new-env
                             (extend-env
                              vars
                              (map newref args)
			      types
                              saved-env)))
                        (if (instrument-let)
                            (begin
                              (printf
                               "entering body of proc ~s with env =~%"
                               vars)
                              (pretty-print (env->list new-env))
                              (printf "store =~%")
                              (pretty-print (store->readable (get-store-as-list)))
                              (printf "~%")))
                        (value-of body new-env))))))



;; apply-method : Method * Obj * Listof(ExpVal) -> ExpVal
(define apply-method
    (lambda (m self args)
      (cases method m
        (a-method (vars body super-name field-names)
          (value-of body
            (extend-env vars (map newref args) '()
              (extend-env-with-self-and-super
                self super-name
                (extend-env field-names (object->fields self) '()
                  (empty-env)))))))))


;; method-decls->method-env :
;; Listof(MethodDecl) * ClassName * Listof(FieldName) -> MethodEnv
(define method-decls->method-env
  (lambda (m-decls super-name field-names)
    (map
     (lambda (m-decl)
       (cases method-decl m-decl
              (a-method-decl (result-type method-name vars var-types body)
                             (list method-name
                                   (a-method vars body super-name field-names)))
	      (a-static-method-decl (result-type method-name vars var-types body)
				    (list method-name
					  (a-method vars body super-name field-names)))))
     m-decls)))


(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvars (list-of symbol?))
   (bvals (list-of reference?))
   (btypes (list-of type?))
   (saved-env environment?))
  (extend-env-rec**
   (proc-names (list-of symbol?))
   (b-vars (list-of (list-of symbol?)))
   (proc-bodies (list-of expression?))
   (saved-env environment?))
  (extend-env-with-self-and-super
   (self object?)
   (super-name symbol?)
   (saved-env environment?)))


(define apply-env
    (lambda (env search-sym find-type?)
      (cases environment env
        (empty-env ()
          (error 'apply-env "No binding for ~s" search-sym))
        (extend-env (bvars bvals btypes saved-env)
          (cond
            ((location search-sym bvars)
             => (lambda (n)
		  (if find-type?
		      (list-ref btypes n)
		      (list-ref bvals n))))
            (else
              (apply-env saved-env search-sym find-type?))))
        (extend-env-rec** (p-names b-varss p-bodies saved-env)
          (cond
            ((location search-sym p-names)
             => (lambda (n)
                  (newref
                    (proc-val
                      (procedure
                        (list-ref b-varss n)
                        (list-ref p-bodies n)
                        env)))))
            (else (apply-env saved-env search-sym find-type?))))
        (extend-env-with-self-and-super (self super-name saved-env)
          (case search-sym
            ((%self) self)
            ((%super) super-name)
            (else (apply-env saved-env search-sym find-type?)))))))

(define env->list
  (lambda (env)
    (cases environment env
           (empty-env () '())
           (extend-env (sym types val saved-env)
                       (cons
                        (list sym val)
                        (env->list saved-env)))
           (extend-env-rec** (p-names b-varss p-bodies saved-env)
                             (cons
                              (list 'letrec p-names '...)
                              (env->list saved-env)))
           (extend-env-with-self-and-super (self super-name saved-env)
                                           (cons
                                            (list 'self self 'super super-name)
                                            (env->list saved-env))))))

(define extend-env1
  (lambda (id val env)
    (extend-env (list id) (list val) '() env)))


(run "class c1 extends object
       method int initialize () 1
       method int m1 () 11
       staticmethod int m2 () 21
      class c2 extends c1
       method void m1 () 12
       staticmethod int m2 () 22
      let f = proc(x : c1) send x m1()
          g = proc (x : c1) send x m2()
          o = new c2()
      in list((f o), (g o))")

;;(run-all)
