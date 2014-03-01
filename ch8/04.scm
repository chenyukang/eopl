(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/equal-type.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cases.scm")
(load-relative "./base/simplemodule-lang.scm")

(define-datatype proc proc?
  (procedure
   ;;new stuff
   (bvar (list-of symbol?))
   (body expression?)
   (env environment?)))


(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec
   (p-name (list-of symbol?))
   ;;new stuff
   (b-var (list-of (list-of symbol?)))
   (p-body (list-of expression?))
   (saved-env environment?))
  (extend-env-with-module
   (m-name symbol?)
   (m-val typed-module?)
   (saved-env environment?)))

(define-datatype type-environment type-environment?
  (empty-tenv)
  (extend-tenv
   (bvar symbol?)
   (bval type?)
   (saved-tenv type-environment?))
  (extend-tenv-with-module
   (name symbol?)
   (interface interface?)
   (saved-tenv type-environment?))
  )


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
  '(
    (program
     ((arbno module-definition)
      expression)
     a-program)

    (module-definition
     ("module" identifier
      "interface" interface
      "body" module-body)
     a-module-definition)


    (interface
     ("[" (arbno declaration) "]")
     simple-iface)


    (declaration
     (identifier ":" type)
     val-decl)


    (module-body
     ("[" (arbno definition) "]")
     defns-module-body)


    (definition
      (identifier "=" expression)
      val-defn)


    ;; new expression:

    (expression
     ("from" identifier "take" identifier)
     qualified-var-exp)

    ;; new types

    (type
     (identifier)
     named-type)

    (type
     ("from" identifier "take" identifier)
     qualified-type)


      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; no changes in grammar below here
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (expression (number) const-exp)

    (expression
     (identifier)
     var-exp)

    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression
     ("let" (separated-list identifier "=" expression ",") "in" expression)
     let-exp)

    (expression
     ("proc" "(" (separated-list identifier ":" type "," ) ")" expression)
     proc-exp)

    ;; new stuff
    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    (expression
     ("letrec"
      (separated-list type
		      identifier
		      "("
		      (separated-list identifier ":" type ",")
		      ")"
		      "=" expression
		      ",")
		      "in" expression)
     letrec-exp)

    (type
     ("int")
     int-type)

    (type
     ("bool")
     bool-type)

    ;; new stuff
    (type
     ("(" (arbno type) "->" type ")")
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


(define value-of-arg
  (lambda (env)
    (lambda (arg)
      (value-of arg env))))

(define type-of-arg
  (lambda (tenv)
    (lambda (arg)
      (type-of arg tenv))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)

    (cases expression exp

           (const-exp (num) (num-val num))

           (var-exp (var) (apply-env env var))

           (qualified-var-exp (m-name var-name)
                              (lookup-qualified-var-in-env m-name var-name env))

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

           (let-exp (vars exps body)
                    (let ((vals (map (value-of-arg env) exps)))
                      (let ((new-env (extend-env-vars vars vals env)))
                        (value-of body new-env))))

           (proc-exp (bvars ty body)
                     (proc-val
                      (procedure bvars body env)))

           (call-exp (rator rands)
                     (let ((proc (expval->proc (value-of rator env)))
                           (args  (map (value-of-arg env) rands)))
                       (apply-procedure proc args)))

           (letrec-exp (types proc-names bvars args-types proc-body letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec proc-names bvars proc-body env)))

           )))



(define find-sym-in-env-rec
  (lambda (search-sym ids bvars exps)
    (if (null? ids)
	'()
	(if (eqv? search-sym (car ids))
	    (list (car bvars) (car exps))
	    (find-sym-in-env-rec
	     search-sym (cdr ids) (cdr bvars) (cdr exps))))))

(define apply-env
  (lambda (env search-sym)
    (cases environment env
           (empty-env ()
                      (error 'apply-env "No value binding for ~s" search-sym))
           (extend-env (bvar bval saved-env)
                       (if (eqv? search-sym bvar)
                           bval
                           (apply-env saved-env search-sym)))
           (extend-env-rec (ids bvars exps saved-env)
			   (let ((find-res (find-sym-in-env-rec
					    search-sym ids bvars exps)))
			     (if (not (null? find-res))
				 (proc-val (procedure (car find-res)
						      (cadr find-res)
						      env))
				 (apply-env saved-env search-sym))))

	   ;;(if (eqv? search-sym id)
	   ;;(proc-val (procedure bvars body env))
	   ;;(apply-env saved-env search-sym)))
           (extend-env-with-module (m-name m-val saved-env)
                                   (apply-env saved-env search-sym)) )))


(define extend-tenv-procs
  (lambda (names bvars-types result-types tenv)
    (if (null? names)
	tenv
	(extend-tenv
	 (car names)
	 (proc-type (car bvars-types) (car result-types))
	 (extend-tenv-procs
	  (cdr names)
	  (cdr bvars-types)
	  (cdr result-types)
	  tenv)))))


(define extend-env*
  (lambda (vars vals saved-env)
    (if (null? vars)
        saved-env
        (extend-env (car vars) (car vals)
                    (extend-env* (cdr vars) (cdr vals) saved-env)))))

(define extend-tenv*
  (lambda (bvars types tenv)
    (if (null? bvars)
	tenv
	(extend-tenv (car bvars) (car types)
		     (extend-tenv* (cdr bvars) (cdr types) tenv)))))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
           (procedure (vars body saved-env)
                      (value-of body (extend-env* vars args saved-env))))))


(define extend-env-vars
  (lambda (vars vals env)
    (if (null? vars)
	env
	(extend-env (car vars) (car vals)
		    (extend-env-vars (cdr vars) (cdr vals) env)))))


;; lookup-module-name-in-env : Sym * Env -> Typed-Module
(define lookup-module-name-in-env
  (lambda (m-name env)
    (cases environment env
           (empty-env ()
                      (error 'lookup-module-name-in-env
                             "No module binding for ~s" m-name))
           (extend-env (bvar bval saved-env)
                       (lookup-module-name-in-env m-name saved-env))
           (extend-env-rec  (ids bvars exps saved-env)
			    (lookup-module-name-in-env m-name saved-env))
           (extend-env-with-module
            (m-name1 m-val saved-env)
            (if (eqv? m-name1 m-name)
                m-val
                (lookup-module-name-in-env m-name saved-env))))))

;; type-of : Exp * Tenv -> Type
(define type-of
  (lambda (exp tenv)
    (cases expression exp
           (const-exp (num) (int-type))

           (diff-exp (exp1 exp2)
                     (let ((type1 (type-of exp1 tenv))
                           (type2 (type-of exp2 tenv)))
                       (check-equal-type! type1 (int-type) exp1)
                       (check-equal-type! type2 (int-type) exp2)
                       (int-type)))

           (zero?-exp (exp1)
                      (let ((type1 (type-of exp1 tenv)))
                        (check-equal-type! type1 (int-type) exp1)
                        (bool-type)))

           (if-exp (exp1 exp2 exp3)
                   (let ((ty1 (type-of exp1 tenv))
                         (ty2 (type-of exp2 tenv))
                         (ty3 (type-of exp3 tenv)))
                     (check-equal-type! ty1 (bool-type) exp1)
                     (check-equal-type! ty2 ty3 exp)
                     ty2))

           (var-exp (var) (apply-tenv tenv var))

           ;; lookup-qualified-var-in-tenv defined on page 285.
           (qualified-var-exp (m-name var-name)
                              (lookup-qualified-var-in-tenv m-name var-name tenv))

           (let-exp (vars exps body)
                    (let ((rhs-types (map (type-of-arg tenv) exps)))
                      (type-of body (extend-tenv* vars rhs-types tenv))))

           (proc-exp (bvars bvar-types body)
		     (let ((result-type
			      (type-of body
				       (extend-tenv* bvars
						     bvar-types tenv))))
			 (proc-type bvar-types result-type)))

           (call-exp (rator rands)
                     (let ((rator-type (type-of rator tenv))
                           (rand-types  (map (type-of-arg tenv) rands)))
                       (cases type rator-type
                              (proc-type (arg-types result-type)
                                         (begin
                                           (check-equal-type! arg-types rand-types rands)
                                           result-type))
                              (else
                               (error 'type-of
                                      "Rator not a proc type:~%~s~%had rator type ~s"
                                      rator (type-to-external-form rator-type))))))

           (letrec-exp (result-types proc-names
				     bvars bvar-types
				     proc-exps
				     letrec-body)
                       (let ((tenv-for-letrec-body
                              (extend-tenv-procs proc-names
						 bvar-types result-types tenv)))
			 ;; ignore check-type! right now
                         ;; (let ((proc-result-type
                         ;;        (expand-type proc-result-type tenv))
                         ;;       (proc-body-type
                         ;;        (type-of proc-body
                         ;;                 (extend-tenv
                         ;;                  bvar
                         ;;                  (expand-type bvar-type tenv)
                         ;;                  tenv-for-letrec-body))))
                         ;;   (check-equal-type!
                         ;;    proc-body-type proc-result-type proc-body)
                         (type-of letrec-body tenv-for-letrec-body)))
    )))


(define type-to-external-form
  (lambda (ty)
    (cases type ty
           (int-type () 'int)
           (bool-type () 'bool)
           (proc-type (arg-types result-type)
		      (cons
		       (types->form arg-types)
		       (cons '=>
			     (type-to-external-form result-type))))
           (named-type (name) name)
           (qualified-type (modname varname)
                           (list 'from modname 'take varname))
           )))


(define types->form
  (lambda (types)
    (if (null? types)
        '()
        (let ((left (types->form (cdr types))))
          (cons (type-to-external-form (car types))
                (if (null? left)
                    left
                    (cons '* left)))))))

(run "let demo = proc(x : int, y : int) -(x, y) in (demo 1 2)")

(run "let x = 1, y= 2 in -(x ,y)")

(run "letrec int f(x : int) = -(x,1),
             int g(x : int) = -(x, 2)
     in -((f 40), (g 40))")


(run-all)
(check-all)

;;proc int => int now transfer to (int) => int
