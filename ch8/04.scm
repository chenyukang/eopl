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
   (p-name symbol?)
   ;;new stuff
   (b-var (list-of symbol?))
   (p-body expression?)
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

           (letrec-exp (ty1 proc-name bvar ty2 proc-body letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec proc-name bvar proc-body env)))

           )))


(define apply-env
  (lambda (env search-sym)
    (cases environment env
           (empty-env ()
                      (error 'apply-env "No value binding for ~s" search-sym))
           (extend-env (bvar bval saved-env)
                       (if (eqv? search-sym bvar)
                           bval
                           (apply-env saved-env search-sym)))
           (extend-env-rec (id bvars body saved-env)
                           (if (eqv? search-sym id)
                               (proc-val (procedure bvars body env))
                               (apply-env saved-env search-sym)))
           (extend-env-with-module (m-name m-val saved-env)
                                   (apply-env saved-env search-sym)) )))


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


(define extend-env-vars
  (lambda (vars vals env)
    (if (null? vars)
	env
	(extend-env (car vars) (car vals)
		    (extend-env-vars (cdr vars) (cdr vals) env)))))

(run "let demo = proc(x : int, y : int) -(x, y) in (demo 1 2)")

(run "let x = 1, y= 2 in -(x ,y)")

;;(run-all)
