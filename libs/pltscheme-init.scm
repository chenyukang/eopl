;; compatibility file for drscheme.

; ;; create a new namespace in which keywords are not enforced.  This 
; ;; is necessary because we use "class" as an ordinary variable.  
; ;; Giving a binding for "class" seems to destroy it as a special form. 

; (current-namespace(make-namespace 'no-keywords))
; (define class 13) 
;   
;; a better version, due to Matthew Flatt:

(require-library "pretty.ss")

(let ([p pretty-print])
  (current-namespace (scheme-report-environment 5))
  (global-defined-value 'pretty-print p))
  
;; save the nicer ambient behavior
; (define ambient-printf #%printf)

;; load the defaults
(load "r5rs.scm")

;; load pretty-printer for show-define-datatypes
(set! sllgen:pretty-print pretty-print)
(set! eopl:pretty-print pretty-print)
(set! define-datatype:pretty-print pretty-print)

;; better behavior for error
(define sllgen:error-stop 
  (lambda () (error 'eopl-error-stop)))

(load "sllgen.scm")
(load "define-datatype.scm")
(load "test-harness.scm")
(load "test-suite.scm")
