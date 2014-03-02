(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/equal-type.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cases.scm")
(load-relative "./base/simplemodule-lang.scm")

;; type checker haven't finished completely!

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?))
  ;;new stuff
  (simple-module-val
   (module environment?)))

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
					new-module-env))))))))

(define extend-env-with-module*
  (lambda (modules env)
    (if (null? modules) env
	(let ((first (car modules)))
	  (cases module-definition first
		 (a-module-definition (m-name m-type inner-modules m-body)
				      (extend-env m-name
						  (value-of-module m-body
								   (extend-env-with-module* inner-modules env))
						  (extend-env-with-module*
						   (cdr modules) env))))))))

;; add-module-defns-to-tenv : Listof(ModuleDefn) * Tenv -> Tenv
(define add-module-defns-to-tenv
  (lambda (defns tenv)
    (if (null? defns)
        tenv
        (cases module-definition (car defns)
               (a-module-definition (m-name expected-iface inner-modules m-body)
				    (let ((new-module-tenv
					   (add-module-defns-to-tenv inner-modules tenv)))
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



;; lookup-qualified-var-in-env : Sym * Sym * Env -> ExpVal
(define lookup-qualified-var-in-env
  (lambda (m-name var-name env)
    (let ((m-val (lookup-module-name-in-env m-name env)))
	    (cases typed-module m-val
		   (simple-module (bindings)
				  (apply-env* bindings var-name))
		   (proc-module (bvar body saved-env)
				(error 'lookup-qualified-var
				       "can't retrieve variable from ~s take ~s from proc module"
				       m-name var-name))))))


(define lookup-qualified-var-in-tenv
  (lambda (m-name var-name tenv)
    (let ((iface (lookup-module-name-in-tenv tenv m-name)))
        (cases interface iface
               (simple-iface (decls)
			     ;; hacky,
                             (lookup-variable-name-in-decls (car var-name) decls)) ))))


(define lookup-variable-name-in-decls
  (lambda (var-name decls0)
    (let loop ((decls decls0))
      (cond
       ((null? decls)
        (raise-lookup-variable-in-decls-error! var-name decls0))
       ((eqv? var-name (decl->name (car decls)))
        (decl->type (car decls)))
       (else (loop (cdr decls)))))))


(define apply-env*
  (lambda (modules vars)
    (if (null? (cdr vars))
	  (if (environment? modules)
	      (apply-env modules (car vars))
	      (cases expval modules
	       (simple-module-val (bindings)
				  (apply-env bindings (car vars)))
	       (else (error "apply-env*: ~s ~s" modules vars))))
	(let ((next-module (apply-env modules (car vars))))
	  (apply-env* next-module (cdr vars))))))

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
				   (apply-env saved-env search-sym)))))

(define value-of-module
  (lambda (m-body env)
    (cases module-body m-body
           (defns-module-body (defns)
             (simple-module-val
              (defns-to-env defns env))) )))


;; defns-to-env : Listof(Defn) * Env -> Env
(define defns-to-env
  (lambda (defns env)
    (if (null? defns)
        (empty-env)                ; we're making a little environment
        (cases definition (car defns)
               (val-defn (var exp)
			 (let ((val (value-of exp env)))
                           ;; new environment for subsequent definitions
			       (let ((new-env (extend-env var val env)))
				 (extend-env var val
					     (defns-to-env
					       (cdr defns) new-env)))))
			 ))))


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

(run "module m1
interface
[u : int
   n : [v : int]]
body
module m2
interface [v : int]
body [v = 33]
module m3
interface [v: int]
body [v = 44]
[u = 44 n = m3]
from m1 take n take v")


(run "module m
 interface
  [u : int]
 body
  [u = 3]
from m take u")


(check "module m
interface
[u : int]
body
[u = 3]
from m take u")

(run-all)
(check-all)
