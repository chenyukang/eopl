(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/equal-type.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cases.scm")
(load-relative "./base/simplemodule-lang.scm")

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec
   (p-name (list-of symbol?))
   ;;new stuff
   (b-var (list-of symbol?))
   (p-body (list-of expression?))
   (saved-env environment?))
  (extend-env-with-module
   (m-name symbol?)
   (m-val typed-module?)
   (saved-env environment?)))


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

    ;; new stuff
    (module-body
     ("let" identifier "=" expression "in" module-body)
     let-module-body)

    (module-body
     ("letrec"
      (separated-list type
		      identifier
		      "(" identifier ":" type ")"
		      "=" expression ",")
      "in" module-body)
     letrec-module-body)


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
      type identifier "(" identifier ":" type ")"
      "=" expression "in" expression)
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


;; lookup-module-name-in-env : Sym * Env -> Typed-Module
(define lookup-module-name-in-env
  (lambda (m-name env)
    (cases environment env
           (empty-env ()
                      (error 'lookup-module-name-in-env
                             "No module binding for ~s" m-name))
           (extend-env (bvar bval saved-env)
                       (lookup-module-name-in-env m-name saved-env))
           (extend-env-rec  (id bvar body saved-env)
                                    (lookup-module-name-in-env m-name saved-env))
           (extend-env-with-module
            (m-name1 m-val saved-env)
            (if (eqv? m-name1 m-name)
                m-val
                (lookup-module-name-in-env m-name saved-env))))))


;; value-of-module-body : ModuleBody * Env -> TypedModule
(define value-of-module-body
  (lambda (m-body env)
    (cases module-body m-body
           (defns-module-body (defns)
             (simple-module
              (defns-to-env defns env)))
	   (let-module-body (var exp body)
			    (let  ((val (value-of exp env)))
			      (let ((new-env (extend-env var val env)))
				(value-of-module-body body new-env))))
	   (letrec-module-body (types proc-names bvars args-types proc-body
				      letrec-body)
			       (value-of-module-body letrec-body
					   (extend-env-rec proc-names bvars proc-body env))))))

(define add-module-defns-to-env
  (lambda (defs env)
    (if (null? defs)
        env
	(cases module-definition (car defs)
               (a-module-definition (m-name iface m-body)
                                    (add-module-defns-to-env
                                     (cdr defs)
                                     (extend-env-with-module
                                      m-name
                                      (value-of-module-body m-body env)
                                      env)))))))



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


;; interface-of : ModuleBody * Tenv -> Iface
(define interface-of
  (lambda (m-body tenv)
    (cases module-body m-body
           (defns-module-body (defns)
             (simple-iface
              (defns-to-decls defns tenv)))
	   (let-module-body (var exp body)
			    (let ((var-type (type-of exp tenv)))
			      (interface-of body
				 (extend-tenv var var-type tenv))))
	   (letrec-module-body (results-types proc-names bvars bvars-types proc-body
				      body)
			       (let ((new-tenv
				      (extend-tenv-procs proc-names bvars-types results-types tenv)))
				 (interface-of body new-tenv))))))


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

           (extend-env-with-module (m-name m-val saved-env)
                                   (apply-env saved-env search-sym)) )))



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

           (proc-exp (bvar ty body)
                     (proc-val
                      (procedure bvar body env)))

           (call-exp (rator rand)
                     (let ((proc (expval->proc (value-of rator env)))
                           (arg  (value-of rand env)))
                       (apply-procedure proc arg)))

           (letrec-exp (ty1 proc-name bvar ty2 proc-body letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec (list proc-name) (list bvar)  (list proc-body) env)))

           )))


(run "module even-odd
  interface
     [even : (int -> int)
      odd  : (int -> int) ]
  body
  letrec
    int local-odd (x : int) = if zero?(x) then 1 else (local-even -(x, 1)) ,
    int local-even (x :int) = if zero?(x) then 0 else (local-odd -(x, 1))
  in
 [ even = local-even
   odd = local-odd ]
 from even-odd take odd")


(run "module even-odd
  interface
     [even : int
      odd  : int ]
  body
  let x = 1 in
 [ even = x
   odd =  -(x, 2) ]
 from even-odd take odd")


(check "module even-odd
  interface
     [even : int
      odd  : int ]
  body
  let x = 1 in
 [ even = x
   odd =  -(x, 2) ]
 from even-odd take odd")

(check "module even-odd
  interface
     [even : (int -> int)
      odd  : (int -> int) ]
  body
  letrec
    int local-odd (x : int) = if zero?(x) then 1 else (local-even -(x, 1)) ,
    int local-even (x :int) = if zero?(x) then 0 else (local-odd -(x, 1))
  in
 [ even = local-even
   odd = local-odd ]
 from even-odd take odd")


;;(run "proc(x : int) -(x, 1)")

(run-all)
(check-all)
