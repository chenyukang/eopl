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


;; see new stuff, add fieldset-exp and fieldref-exp

(define debug? (make-parameter #f))

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

    ;; new stuff
    (expression
     ("fieldref" expression identifier)
     fieldref-exp)

    (expression
     ("fieldset" expression identifier "=" expression )
     fieldset-exp)

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

	   (proc-exp (bvars types body)
		     (proc-val
		      (procedure bvars body env)))

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
			  (apply-env env x)
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

	   ;; new stuff
	   (fieldref-exp (exp name)
			 (let ((obj (value-of exp env)))
			   (deref (object->field obj name))))

	   (fieldset-exp (exp name val-exp)
			 (let* ((obj (value-of exp env))
				(val (value-of val-exp env))
				(ref (object->field obj name)))
			   (setref! ref val)))


	   )))



;; new stuff
(define object->field
  (lambda (obj field-name)
    (let* ((fields (object->fields obj))
           (class-name (object->class-name obj))
           (field-names (class->field-names (lookup-class class-name))))
      (cond
       ((location field-name field-names)
        => (lambda (n)
             (list-ref fields n)))
       (else
        (error "object->field: can not find ~s" field-name))))))

(define object->fields
  (lambda (obj)
    (cases object obj
           (an-object (class-decl fields)
                      fields))))

;; type-of : Exp -> Tenv
(define type-of
  (lambda (exp tenv)
    (cases expression exp

           (const-exp (num) (int-type))

           (var-exp (var) (apply-tenv tenv var))

           (diff-exp (exp1 exp2)
                     (let ((type1 (type-of exp1 tenv))
                           (type2 (type-of exp2 tenv)))
                       (check-equal-type! type1 (int-type) exp1)
                       (check-equal-type! type2 (int-type) exp2)
                       (int-type)))

           (sum-exp (exp1 exp2)
                    (let ((type1 (type-of exp1 tenv))
                          (type2 (type-of exp2 tenv)))
                      (check-equal-type! type1 (int-type) exp1)
                      (check-equal-type! type2 (int-type) exp2)
                      (int-type)))

           (zero?-exp (exp1)
                      (let ((type1 (type-of exp1 tenv)))
                        (check-equal-type! type1 (int-type) exp1)
                        (bool-type)))

           (if-exp (test-exp true-exp false-exp)
                   (let
                       ((test-type (type-of test-exp tenv))
                        (true-type (type-of true-exp tenv))
                        (false-type (type-of false-exp tenv)))
                     ;; these tests either succeed or raise an error
                     (check-equal-type! test-type (bool-type) test-exp)
                     (check-equal-type! true-type false-type exp)
                     true-type))

           (let-exp (ids rands body)
                    (let ((new-tenv
                           (extend-tenv
                            ids
                            (types-of-exps rands tenv)
                            tenv)))
                      (type-of body new-tenv)))

           (proc-exp (bvars bvar-types body)
                     (let ((result-type
                            (type-of body
                                     (extend-tenv bvars bvar-types tenv))))
                       (proc-type bvar-types result-type)))

           (call-exp (rator rands)
                     (let ((rator-type (type-of rator tenv))
                           (rand-types  (types-of-exps rands tenv)))
                       (type-of-call rator-type rand-types rands exp)))

           (letrec-exp (proc-result-types proc-names
                                          bvarss bvar-typess proc-bodies
                                          letrec-body)
                       (let ((tenv-for-letrec-body
                              (extend-tenv
                               proc-names
                               (map proc-type bvar-typess proc-result-types)
                               tenv)))
                         (for-each
                          (lambda (proc-result-type bvar-types bvars proc-body)
                            (let ((proc-body-type
                                   (type-of proc-body
                                            (extend-tenv
                                             bvars
                                             bvar-types
                                             tenv-for-letrec-body)))) ;; !!
                              (check-equal-type!
                               proc-body-type proc-result-type proc-body)))
                          proc-result-types bvar-typess bvarss proc-bodies)
                         (type-of letrec-body tenv-for-letrec-body)))

           (begin-exp (exp1 exps)
                      (letrec
                          ((type-of-begins
                            (lambda (e1 es)
                              (let ((v1 (type-of e1 tenv)))
                                (if (null? es)
                                    v1
                                    (type-of-begins (car es) (cdr es)))))))
                        (type-of-begins exp1 exps)))

           (assign-exp (id rhs)
                       (check-is-subtype!
                        (type-of rhs tenv)
                        (apply-tenv tenv id)
                        exp)
                       (void-type))

           (list-exp (exp1 exps)
                     (let ((type-of-car (type-of exp1 tenv)))
                       (for-each
                        (lambda (exp)
                          (check-equal-type!
                           (type-of exp tenv)
                           type-of-car
                           exp))
                        exps)
                       (list-type type-of-car)))

           ;; object stuff begins here

           (new-object-exp (class-name rands)
                           (let ((arg-types (types-of-exps rands tenv))
                                 (c (lookup-static-class class-name)))
                             (cases static-class c
                                    (an-interface (method-tenv)
                                                  (report-cant-instantiate-interface class-name))
                                    (a-static-class (super-name i-names
                                                                field-names field-types method-tenv)
                                                    ;; check the call to initialize
                                                    (type-of-call
                                                     (find-method-type
                                                      class-name
                                                      'initialize)
                                                     arg-types
                                                     rands
                                                     exp)
                                                    ;; and return the class name as a type
                                                    (class-type class-name)))))

           (self-exp ()
                     (apply-tenv tenv '%self))

           (method-call-exp (obj-exp method-name rands)
                            (let ((arg-types (types-of-exps rands tenv))
                                  (obj-type (type-of obj-exp tenv)))
                              (type-of-call
                               (find-method-type
                                (type->class-name obj-type)
                                method-name)
                               arg-types
                               rands
                               exp)))

           (super-call-exp (method-name rands)
                           (let ((arg-types (types-of-exps rands tenv))
                                 (obj-type (apply-tenv tenv '%self)))
                             (type-of-call
                              (find-method-type
                               (apply-tenv tenv '%super)
                               method-name)
                              arg-types
                              rands
                              exp)))

           (cast-exp (exp class-name)
                     (let ((obj-type (type-of exp tenv)))
                       (if (class-type? obj-type)
                           (class-type class-name)
                           (report-bad-type-to-cast obj-type exp))))

           (instanceof-exp (exp class-name)
                           (let ((obj-type (type-of exp tenv)))
                             (if (class-type? obj-type)
                                 (bool-type)
                                 (report-bad-type-to-instanceof obj-type exp))))

	   ;; new stuff
           (fieldref-exp (exp name)
                         (let ((exp-type (type-of exp tenv)))
                           (if (class-type? exp-type)
			       (static-class->field-type
				(cadr exp-type) name)
			       (error 'fieldref-exp "expect class type, got ~s\n" exp-type))))

	   ;; new stuff
           (fieldset-exp (exp name val-exp)
			 (let ((exp-type (type-of exp tenv))
			       (val-type (type-of val-exp tenv)))
			   (if (class-type? exp-type)
			       (let ((field-type
				      (static-class->field-type (cadr exp-type) name)))
				 (check-equal-type! field-type val-type
						    exp)
				 (void-type))
			       (error 'field-exp "expect class type, got ~s\n" exp-type))))


           )))


;; new stuff
(define static-class->field-type
  (lambda (class-name field-name)
    (let* ((static-class (lookup-static-class class-name))
	   (fields (static-class->field-names static-class))
	   (field-types (static-class->field-types static-class)))
      (cond
       ((location field-name fields)
	=> (lambda (n)
	     (list-ref field-types n)))
       (else
	(error "static-class->field-type: ~s can not find ~s"
	       class-name field-name))))))


(check "class demo extends object
      field int ivar
      method void initialize() set ivar = 1
      let obj = new demo()
       in begin
          fieldset obj ivar = 1;
          fieldref obj ivar
          end")

(run-all)
