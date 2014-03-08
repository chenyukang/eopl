(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/type-structures.scm")
(load-relative "./base/type-module.scm")
(load-relative "./base/grammar.scm")
(load-relative "./base/renaming.scm")
(load-relative "./base/subtyping.scm")
(load-relative "./base/expand-type.scm")
(load-relative "./base/type-cases.scm")


;; add multi-arg procedure

(define debug? (make-parameter #t))

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
  (extend-env-recursively
   (id symbol?)
   (bvar (list-of symbol?))
   (body expression?)
   (saved-env environment?))
  (extend-env-with-module
   (m-name symbol?)
   (m-val typed-module?)
   (saved-env environment?)
   ))

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
     ("opaque" identifier)
     opaque-type-decl)

    (declaration
     ("transparent" identifier "=" type)
     transparent-type-decl)

    (declaration
     (identifier ":" type)
     val-decl)


    (module-body
     ("[" (arbno definition) "]")
     defns-module-body)


    (definition
      (identifier "=" expression)
      val-defn)

    (definition
      ("type" identifier "=" type)
      type-defn)

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
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("proc" "(" (separated-list identifier ":" type ",") ")"
      expression)
     proc-exp)

    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    (expression
     ("letrec"
      type identifier "(" (separated-list identifier ":" type ",")
      ")"
      "=" expression "in" expression)
     letrec-exp)

    (type
     ("int")
     int-type)

    (type
     ("bool")
     bool-type)

    (type
     ("(" (separated-list type ",") "->" type ")")
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

           (let-exp (var exp1 body)
                    (let ((val (value-of exp1 env)))
                      (let ((new-env (extend-env var val env)))
                        (value-of body new-env))))

	   (proc-exp (vars types proc-body)
		     (proc-val
		      (procedure vars proc-body env)))

	   (call-exp (rator rands)
		     (let ((proc (expval->proc (value-of rator env)))
			   (args (map (value-of-arg env) rands)))
		       (apply-procedure proc args)))


           (letrec-exp (ty1 proc-name bvar ty2 proc-body letrec-body)
                       (value-of letrec-body
                                 (extend-env-recursively proc-name bvar proc-body env)))

           )))

(define extend-env*
  (lambda (vars vals saved-env)
    (if (null? vars)
        saved-env
        (extend-env (car vars) (car vals)
                    (extend-env* (cdr vars) (cdr vals) saved-env)))))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
           (procedure (vars body saved-env)
                      (value-of body (extend-env* vars args saved-env))))))


(define expand-arg-type
  (lambda (tenv)
    (lambda (arg)
      (expand-type arg tenv))))

(define type-of-arg
  (lambda (tenv)
    (lambda (arg)
      (type-of arg tenv))))

(define extend-tenv*
  (lambda (vars types tenv)
    (if (null? vars)
	tenv
	(extend-tenv (car vars) (car types)
		     (extend-tenv* (cdr vars) (cdr types)
				   tenv)))))

(define expand-type
  (lambda (ty tenv)
    (cases type ty
           (int-type () (int-type))
           (bool-type () (bool-type))
           (proc-type (arg-types result-type)
                      (proc-type
                       (map (expand-arg-type tenv) arg-types)
                       (expand-type result-type tenv)))
           (named-type (name)
                       (lookup-type-name-in-tenv tenv name))
           (qualified-type (m-name t-name)
                           (lookup-qualified-type-in-tenv m-name t-name tenv))
           )))


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

           (qualified-var-exp (m-name var-name)
                              (lookup-qualified-var-in-tenv m-name var-name tenv))

           (let-exp (var exp1 body)
                    (let ((rhs-type (type-of exp1 tenv)))
                      (type-of body (extend-tenv var rhs-type tenv))))

           (proc-exp (bvars bvar-types body)
                     (let ((expanded-bvar-types
			    (map (expand-arg-type tenv) bvar-types)))
                       (let ((result-type
                              (type-of body
                                       (extend-tenv*
                                        bvars
                                        expanded-bvar-types
                                        tenv))))
                         (proc-type expanded-bvar-types result-type))))

           (call-exp (rator rands)
		     (begin
		       ;;(printf "rator: ~s rands: ~s\n" rator rands)
                     (let ((rator-type (type-of rator tenv))
                           (rand-types (map (type-of-arg tenv) rands)))
                       (cases type rator-type
                              (proc-type (arg-type result-type)
;;                                         (begin
					 ;;(check-equal-type! arg-type rand-type rand)
                                           result-type)
                              (else
                               (eopl:error 'type-of
                                           "Rator not a proc type:~%~s~%had rator type ~s"
                                           rator (type-to-external-form rator-type)))))))

           (letrec-exp (proc-result-type proc-name
                                         bvars bvar-types
                                         proc-body
                                         letrec-body)
		       (begin
			 ;;(printf "letrec-exp : vars -> ~s types -> ~s\n"
			 ;;bvars bvar-types)
                       (let ((tenv-for-letrec-body
                              (extend-tenv
                               proc-name
                               (expand-type
                                (proc-type bvar-types proc-result-type)
                                tenv)
                               tenv)))
                         (let ((proc-result-type
                                (expand-type proc-result-type tenv))
                               (proc-body-type
                                (type-of proc-body
                                         (extend-tenv*
                                          bvars
					  (map (expand-arg-type tenv) bvar-types)
                                          tenv-for-letrec-body))))
                           ;;(check-equal-type!
			   ;;proc-body-type proc-result-type proc-body)
			   (begin
			     ;;(printf "haha now\n")
                           (type-of letrec-body tenv-for-letrec-body))))))

           )))



(run "let func = proc(x : int) -(x, 1) in (func 1)")
(check "letrec int func(x : int) = if zero?(x) then 0 else (func -(x, 1)) in
       (func 3)")

(check "let func = proc(x : int) -(x, 1) in (func 1)")

;; (check      "module m interface [opaque t
;;                                        f : (t -> t)]
;;             body [type t = int
;;                        f = proc (x : t) -(x, 1)]
;;             from m take f")

(run "module tables
interface
[opaque table
        empty : table
        add-to-table : (int -> (int -> (table -> table)))
        lookup-in-table : (int -> (table -> int))]
body
[type table = (int -> int)
      empty = proc (x : int) x
      add-to-table = proc (x : int, y : int)
	                proc (t : table)
                	       proc (v : int)
                 	         if zero?(- (v, x)) then y else (t v)
      lookup-in-table = proc(key : int, t : table)
                              (t key)
      ]
let empty = from tables take empty
in let add-binding = from tables take add-to-table
in let lookup = from tables take lookup-in-table
in let table1 = ((add-binding 3 300)
		 ((add-binding 4 400)
		  ((add-binding 3 600)
		   empty)))
in -((lookup 4 table1),
     (lookup 3 table1))")

(run "letrec int f(x : int, y : int) = -(x, y) in (f 11 33)")

;;(run-all)
(check-all)
