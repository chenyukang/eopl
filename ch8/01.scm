(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/equal-type.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cases.scm")
(load-relative "./base/simplemodule-lang.scm")

(define have-duplicate?
  (lambda (defns)
    (if (null? defns)
	#f
	(let ((first-name (module-definition->name (car defns))))
	  (if (maybe-lookup-module-in-list first-name (cdr defns))
	      #t
	      (have-duplicate? (cdr defns)))))))

(define add-module-defns-to-tenv
  (lambda (defns tenv)
    (if (have-duplicate? defns)
	(error 'add-module-defns-to-tenv "have duplicate module names!")
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
						m-name expected-iface actual-iface)))))))))


(run-all)


(add-check! '(duplicate-module-name "module m1 interface [v : int] body [v = 4]
         module m1
          interface [v : int]
          body [v = 3]
         from m1 take v" error))

(add-check! '(duplicate-module-name-pass "module m1 interface [v : int] body [v = 4]
         module m2
          interface [v : int]
          body [v = 3]
         from m1 take v" int))


(add-check! '(duplicate-module-name-error "module m1 interface [v : int] body [v = 4]
         module m2
          interface [v : int]
          body [v = 3]
         module m1
         interface [v : int]
         body [v = 4]
         from m1 take v" error))

(run "module m1 interface [v : int] body [v = 4]
         module m1
          interface [v : int]
          body [v = 10]
         from m1 take v")

(check-all)
