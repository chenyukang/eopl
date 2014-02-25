(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/equal-type.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cases.scm")
(load-relative "./base/simplemodule-lang.scm")

;; The procedure add-module-defn-to-env is not quite right,
;; because it adds all the values defined by the module, not just
;; the ones in the inter- face. Modify add-module-defn-to-env so that it
;; adds to the environment only the values declared in the interface.
;; Does add-module-defn-to-tenv suffer from the same problem?


;; see the new stuff
;; add-module-defn-to-tenv does not suffer this problem.

;; (define debug? (make-parameter #t))
;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

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
				      (value-of-module-body iface m-body env)
				      env)))))))


;; new stuff
(define have-var-in-decls?
  (lambda (var decls)
    (if (null? decls)
	#f
	(cases declaration (car decls)
	       (val-decl (name ty)
			 (if (eq? var name)
			     #t
			     (have-var-in-decls? var (cdr decls))))))))

(define have-var-in-interface?
  (lambda (var iface)
    (cases interface iface
	   (simple-iface (decls)
			 (if (have-var-in-decls? var decls)
			     #t
			     #f)))))
;; new stuff
;; defns-to-env : Listof(Defn) * Env -> Env
(define defns-to-env
  (lambda (iface defns env)
    (if (null? defns)
        (empty-env)                ; we're making a little environment
        (cases definition (car defns)
	       (val-defn (var exp)
			 (if (have-var-in-interface? var iface)
			     (let ((val (value-of exp env)))
			       ;; new environment for subsequent definitions
			       (let ((new-env (extend-env var val env)))
				 (extend-env var val
					     (defns-to-env iface
					       (cdr defns) new-env))))
			     (defns-to-env iface (cdr defns) env))
	       )))))

;; We will have let* scoping inside a module body.
;; We put all the values in the environment, not just the ones
;; that are in the interface.  But the typechecker will prevent
;; anybody from using the extras.
;; value-of-module-body : ModuleBody * Env -> TypedModule
(define value-of-module-body
  (lambda (iface m-body env)
    (cases module-body m-body
      (defns-module-body (defns)
       (simple-module
	(defns-to-env iface defns env))))))


(run "module m
     interface
     [u : int]
     body
     [u = 3] 33")

(run "module m
     interface
     [u : int]
     body
     [u = 3 v = 4]
     from m take u")

;;
(add-test! '(interface-error "module m interface []  body [u = 3]
	      from m take u" error))
;;

(run-all)
(check-all)
