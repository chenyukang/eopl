;; compatibility file for larceny

;; save the nicer ambient behavior
(define ambient-pretty-print pretty-print)

(load "r5rs.scm")
 
;; use the nicer ambient behavior for pretty-print
(set! sllgen:pretty-print pretty-print)

;; make error-stop invoke the debugger
(define eopl:error-stop break)

(load "sllgen.scm")
(load "define-datatype.scm")
(load "test-harness.scm")
(load "test-suite.scm")