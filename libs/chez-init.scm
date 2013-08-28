;; compatibility file for chez scheme

;; save the nicer chez behavior
(define chez-printf printf)
(define chez-pretty-print pretty-print)

(load "r5rs.scm")
 
;; use the nicer chez behavior for these
(set! sllgen:pretty-print chez-pretty-print)
(set! eopl:pretty-print chez-pretty-print)
(set! define-datatype:pretty-print chez-pretty-print)

;; make error stop invoke the debugger
(define eopl:error-stop break)

(load "sllgen.scm")
(load "define-datatype.scm")
(load "test-harness.scm")
(load "test-suite.scm")