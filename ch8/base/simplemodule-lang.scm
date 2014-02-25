;; (load-relative "../libs/init.scm")
;; (load-relative "./base/test.scm")
;; (load-relative "./base/equal-type.scm")
;; (load-relative "./base/data-structures.scm")
;; (load-relative "./base/cases.scm")

;; (define debug? (make-parameter #t))
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

  ;;;; for types
(define atomic-type?
  (lambda (ty)
    (cases type ty
	   (proc-type (ty1 ty2) #f)
	   (else #t))))

(define proc-type?
  (lambda (ty)
    (cases type ty
	   (proc-type (t1 t2) #t)
	   (else #f))))

(define proc-type->arg-type
  (lambda (ty)
    (cases type ty
	   (proc-type (arg-type result-type) arg-type)
	   (else (error 'proc-type->arg-type
			     "Not a proc type: ~s" ty)))))

(define proc-type->result-type
  (lambda (ty)
    (cases type ty
	   (proc-type (arg-type result-type) result-type)
	   (else (error 'proc-type->result-types
			     "Not a proc type: ~s" ty)))))

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
	   (qualified-type (modname varname)
			   (list 'from modname 'take varname))
	   )))


  ;;;; for module definitions

;; maybe-lookup-module-in-list : Sym * Listof(Defn) -> Maybe(Defn)
(define maybe-lookup-module-in-list
  (lambda (name module-defs)
    (if (null? module-defs)
        #f
        (let ((name1 (module-definition->name (car module-defs))))
          (if (eqv? name1 name)
	      (car module-defs)
	      (maybe-lookup-module-in-list name (cdr module-defs)))))))

;; maybe-lookup-module-in-list : Sym * Listof(Defn) -> Defn OR Error
(define lookup-module-in-list
  (lambda (name module-defs)
    (cond
     ((maybe-lookup-module-in-list name module-defs)
      => (lambda (mdef) mdef))
     (else
      (error 'lookup-module-in-list
		  "unknown module ~s"
		  name)))))

(define module-definition->name
  (lambda (m-defn)
    (cases module-definition m-defn
	   (a-module-definition (m-name m-type m-body)
				m-name))))

(define module-definition->interface
  (lambda (m-defn)
    (cases module-definition m-defn
	   (a-module-definition (m-name m-type m-body)
				m-type))))

(define module-definition->body
  (lambda (m-defn)
    (cases module-definition m-defn
	   (a-module-definition (m-name m-type m-body)
				m-body))))

(define val-decl?
  (lambda (decl)
    (cases declaration decl
	   (val-decl (name ty) #t))))

(define decl->name
  (lambda (decl)
    (cases declaration decl
	   (val-decl (name ty) name))))

(define decl->type
  (lambda (decl)
    (cases declaration decl
	   (val-decl (name ty) ty))))


;; value-of-program : Program -> Expval
(define value-of-program
  (lambda (pgm)
    (cases program pgm
	   (a-program (module-defs body)
		      (let ((env
			     (add-module-defns-to-env module-defs (empty-env))))
			(value-of body env))))))

;; add-module-defns-to-env : Listof(Defn) * Env -> Env
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

;; We will have let* scoping inside a module body.
;; We put all the values in the environment, not just the ones
;; that are in the interface.  But the typechecker will prevent
;; anybody from using the extras.

;; value-of-module-body : ModuleBody * Env -> TypedModule
(define value-of-module-body
  (lambda (m-body env)
    (cases module-body m-body
	   (defns-module-body (defns)
	     (simple-module
	      (defns-to-env defns env))) )))


(define raise-cant-apply-non-proc-module!
  (lambda (rator-val)
    (error 'value-of-module-body
		"can't apply non-proc-module-value ~s" rator-val)))

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
				 (extend-env-recursively proc-name bvar proc-body env)))

	   )))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
	   (procedure (var body saved-env)
		      (value-of body (extend-env var arg saved-env))))))


;; <:-iface : Iface * Iface * Tenv -> Bool
(define <:-iface
  (lambda (iface1 iface2 tenv)
    (cases interface iface1
	   (simple-iface (decls1)
			 (cases interface iface2
				(simple-iface (decls2)
					      (<:-decls decls1 decls2 tenv)))))))

;; <:-decls : Listof(Decl) * Listof(Decl) * Tenv -> Bool
;;
;; s1 <: s2 iff s1 has at least as much stuff as s2, and in the same
;; order.  We walk down s1 until we find a declaration that declares
;; the same name as the first component of s2.  If we run off the
;; end of s1, then we fail.  As we walk down s1, we record any type
;; bindings in the tenv
;;
(define <:-decls
  (lambda (decls1 decls2 tenv)
    (cond
     ((null? decls2) #t)
     ((null? decls1) #f)
     (else
      (let ((name1 (decl->name (car decls1)))
	    (name2 (decl->name (car decls2))))
	(if (eqv? name1 name2)
	    (and
	     (equal?
	      (decl->type (car decls1))
	      (decl->type (car decls2)))
	     (<:-decls (cdr decls1) (cdr decls2) tenv))
	    (<:-decls (cdr decls1) decls2 tenv)))))))



;; check-equal-type! : Type * Type * Exp -> Unspecified
(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (if (not (equal? ty1 ty2))
        (report-unequal-types ty1 ty2 exp))))

;; report-unequal-types : Type * Type * Exp -> Unspecified
(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (error 'check-equal-type!
		"Types didn't match: ~s != ~a in~%~a"
		(type-to-external-form ty1)
		(type-to-external-form ty2)
		exp)))

  ;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;

(define type-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (module-defs body)
                      (type-of body
                               (add-module-defns-to-tenv module-defs (empty-tenv)))))))


;; add-module-defns-to-tenv : Listof(ModuleDefn) * Tenv -> Tenv
(define add-module-defns-to-tenv
  (lambda (defns tenv)
    (if (null? defns)
        tenv
        (cases module-definition (car defns)
               (a-module-definition (m-name expected-iface m-body)
                                    (let ((actual-iface (interface-of m-body tenv)))
                                      (if (<:-iface actual-iface expected-iface tenv)
                                          (let ((new-tenv
                                                 (extend-tenv-with-module
                                                  m-name
                                                  expected-iface
                                                  tenv)))
                                            (add-module-defns-to-tenv
                                             (cdr defns) new-tenv))
                                          (report-module-doesnt-satisfy-iface
                                           m-name expected-iface actual-iface))))))))

;; interface-of : ModuleBody * Tenv -> Iface
(define interface-of
  (lambda (m-body tenv)
    (cases module-body m-body
           (defns-module-body (defns)
             (simple-iface
              (defns-to-decls defns tenv))) )))

;; defns-to-decls : Listof(Defn) * Tenv -> Listof(Decl)
;;
;; Convert defns to a set of declarations for just the names defined
;; in defns.  Do this in the context of tenv.  The tenv is extended
;; at every step, so we get the correct let* scoping
;;
(define defns-to-decls
  (lambda (defns tenv)
    (if (null? defns)
        '()
        (cases definition (car defns)
               (val-defn (var-name exp)
                         (let ((ty (type-of exp tenv)))
                           (let ((new-env (extend-tenv var-name ty tenv)))
                             (cons
                              (val-decl var-name ty)
                              (defns-to-decls (cdr defns) new-env)))))))))

(define report-module-doesnt-satisfy-iface
  (lambda (m-name expected-type actual-type)
    (pretty-print
     (list 'error-in-defn-of-module: m-name
           'expected-type: expected-type
           'actual-type: actual-type))
    (error 'type-of-module-defn)))


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

	   (let-exp (var exp1 body)
		    (let ((rhs-type (type-of exp1 tenv)))
		      (type-of body (extend-tenv var rhs-type tenv))))

	   (proc-exp (bvar bvar-type body)
		     (let ((expanded-bvar-type
			    (expand-type bvar-type tenv)))
		       (let ((result-type
			      (type-of body
				       (extend-tenv
					bvar
					expanded-bvar-type
					tenv))))
			 (proc-type expanded-bvar-type result-type))))

	   (call-exp (rator rand)
		     (let ((rator-type (type-of rator tenv))
			   (rand-type  (type-of rand tenv)))
		       (cases type rator-type
			      (proc-type (arg-type result-type)
					 (begin
					   (check-equal-type! arg-type rand-type rand)
					   result-type))
			      (else
			       (error 'type-of
					   "Rator not a proc type:~%~s~%had rator type ~s"
					   rator (type-to-external-form rator-type))))))

	   (letrec-exp (proc-result-type proc-name
					 bvar bvar-type
					 proc-body
					 letrec-body)
		       (let ((tenv-for-letrec-body
			      (extend-tenv
			       proc-name
			       (expand-type
				(proc-type bvar-type proc-result-type)
				tenv)
			       tenv)))
			 (let ((proc-result-type
				(expand-type proc-result-type tenv))
			       (proc-body-type
				(type-of proc-body
					 (extend-tenv
					  bvar
					  (expand-type bvar-type tenv)
					  tenv-for-letrec-body))))
			   (check-equal-type!
			    proc-body-type proc-result-type proc-body)
			   (type-of letrec-body tenv-for-letrec-body))))

	   )))



;;(run-all)
;;(check-all)

;;(check "(proc(x : int) -(x,1)  30)")
