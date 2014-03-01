(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/equal-type.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cases.scm")
(load-relative "./base/simplemodule-lang.scm")

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?))
  (module-val
   (module simple-module?)))


(define debug? (make-parameter #t))
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
      "body"
      (arbno module-definition)
      module-body)
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
     ("from" identifier (arbno "take" identifier))
     qualified-var-exp)

    ;; new types
    (type
     (identifier)
     named-type)

    (type
     (interface)
     iface-type)

    (type
     ("from" identifier "take" identifier)
     qualified-type)

    ;; (type
    ;;  (expression "take" identifier)
    ;;  submodule-var-type)

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

  ;;;;;;;;;;;;;;;; syntactic tests and observers ;;;;;;;;;;;;;;;;

(define module-definition->name
  (lambda (m-defn)
    (cases module-definition m-defn
           (a-module-definition (m-name m-type inner-module m-body)
                                m-name))))


(define module-definition->interface
  (lambda (m-defn)
    (cases module-definition m-defn
           (a-module-definition (m-name m-type inner-module m-body)
                                m-type))))


(define module-definition->body
  (lambda (m-defn)
    (cases module-definition m-defn
           (a-module-definition (m-name m-type inner-module m-body)
                                m-body))))

;; add-module-defns-to-env : Listof(Defn) * Env -> Env
(define add-module-defns-to-env
  (lambda (defs env)
    (if (null? defs)
        env
        (cases module-definition (car defs)
               (a-module-definition (m-name iface inner-modules m-body)
				    (let ((new-module-env
					   (extend-env-with-module* inner-modules env)))
				      (add-module-defns-to-env
				       (cdr defs)
				       (extend-env-with-module
					m-name
					(value-of-module-body m-body new-module-env)
					env))))))))

(define extend-env-with-module*
  (lambda (modules env)
    (if (null? modules) env
	(let ((first (car modules)))
	  (begin
	    (printf "\n fuck now: ~s  ~s\n" first env)
	  (cases module-definition first
		 (a-module-definition (m-name m-type inner-modules m-body)
          	      (extend-env-with-module-name m-name
                                (value-of-module-body m-body
						      (extend-env-with-module* inner-modules env))))))))))

                        ;; (extend-env-with-module*
			;;  (cdr modules) env)))))))))

;; add-module-defns-to-tenv : Listof(ModuleDefn) * Tenv -> Tenv
(define add-module-defns-to-tenv
  (lambda (defns tenv)
    (if (null? defns)
        tenv
        (cases module-definition (car defns)
               (a-module-definition (m-name expected-iface inner-modules m-body)
				    (let ((new-module-tenv (add-module-defns-to-tenv inner-modules tenv)))
				      (let ((actual-iface (interface-of m-body new-module-tenv)))
					(if (<:-iface actual-iface expected-iface tenv)
					    (let ((new-tenv
						   (extend-tenv-with-module
						    m-name
						    expected-iface
						    tenv)))
                                            (add-module-defns-to-tenv
                                             (cdr defns) new-tenv))
                                          (report-module-doesnt-satisfy-iface
                                           m-name expected-iface actual-iface)))))))))



(define type-to-external-form
  (lambda (ty)
    (cases type ty
           (int-type () 'int)
           (bool-type () 'bool)
           (proc-type (arg-type result-type)
                      (list
                       (type-to-external-form arg-type)
                       '->
                       (type-to-external-form result-type)))
           (named-type (name) name)
	   (iface-type (decls) decls) ;; FIXME
           (qualified-type (modname varname)
                           (list 'from modname 'take varname))
           )))


;; lookup-qualified-var-in-env : Sym * Sym * Env -> ExpVal
(define lookup-qualified-var-in-env
  (lambda (m-name var-names env)
    (begin
      (printf "vars: ~s \n" var-names)
    (if (null? (cdr var-names))
	(let ((var-name (car var-names)))
	  (let ((m-val (lookup-module-name-in-env m-name env)))
	    (cases typed-module m-val
		   (simple-module (bindings)
				  (apply-env bindings var-name))
		   (proc-module (bvar body saved-env)
				(error 'lookup-qualified-var
				       "can't retrieve variable from ~s take ~s from proc module"
				       m-name var-name)))))
	(let ((module (lookup-module-name-in-env var-name env)))
	  (lookup module (cdr var-names) env))))))



(define lookup-qualified-var-in-tenv
  (lambda (m-names var-name tenv)
    (begin
      (printf "lookup: ~s ~s\n" var-name tenv)
      (let ((iface (lookup-module-name-in-tenv tenv m-name)))
        (cases interface iface
               (simple-iface (decls)
                             (lookup-variable-name-in-decls var-name decls)) )))))


(define apply-env
  (lambda (env search-sym)
    (cases environment env
           (empty-env ()
                      (error 'apply-env "No value binding for ~s" search-sym))
           (extend-env (bvar bval saved-env)
                       (if (eqv? search-sym bvar)
                           bval
                           (apply-env saved-env search-sym)))
           (extend-env-recursively
            (id bvar body saved-env)
            (if (eqv? search-sym id)
                (proc-val (procedure bvar body env))
                (apply-env saved-env search-sym)))
           (extend-env-with-module (m-name m-val saved-env)
				   (apply-env saved-env search-sym))
	   (extend-env-with-module-name (m-name m-val)
					(if (eqv? search-sym m-name)
					    m-val)))))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-recursively
   (id symbol?)
   (bvar symbol?)
   (body expression?)
   (saved-env environment?))
  (extend-env-with-module
   (m-name symbol?)
   (m-val typed-module?)
   (saved-env environment?))
  (extend-env-with-module-name
   (m-name symbol?)
   (m-val typed-module?))
    )


;; (print (scan&parse "module m1
;; interface
;; [u : int
;;    n : [v : int]]
;; body
;; module m2
;; interface [v : int]
;; body [v = 33]
;; [u = 44 n = m2]
;; from m1 take n take v"))

(run "module m1
interface
[u : int
   n : [v : int]]
body
module m2
interface [v : int]
body [v = 33]
[u = 44 n = m2]
from m1 take n take v")


(run "module m
 interface
  [u : int]
 body
  [u = 3]
from m take u")


;; (check "module m
;;  interface
;;   [u : int]
;;  body
;;   [u = 3]
;; from m take u")

(run-all)
;;(check-all)
