(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/type-structures.scm")
(load-relative "./base/type-module.scm")
(load-relative "./base/grammar.scm")
(load-relative "./base/renaming.scm")
(load-relative "./base/subtyping.scm")
(load-relative "./base/expand-type.scm")
(load-relative "./base/type-cases.scm")


(check "module m1
       interface [opaque t
			 zero : t]
       body [type t = int
		  zero = 0]
       33")

(run-all)
(check-all)
