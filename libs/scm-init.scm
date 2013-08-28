;;; $Id: scm-init.scm,v 1.2 2001/08/21 03:29:33 leavens Exp leavens $
;;; EOPL2e compatibility file for SCM (tested with SCM version 5d3)

;;; save the nicer SCM behavior
(define scm-pretty-print pretty-print)

;;; The following make SCM R5RS compliant
(require 'values)
(require 'macro)
(require 'eval)
;;(set! *R4RS-macro* #t)

(load "/home/yukang/mycode/EOPL/libs/r5rs.scm")
 
;;; use the nicer chez behavior for these
(set! sllgen:pretty-print scm-pretty-print)
(set! eopl:pretty-print scm-pretty-print)
(set! define-datatype:pretty-print scm-pretty-print)

(load "/home/yukang/mycode/EOPL/libs/sllgen.scm")
(load "/home/yukang/mycode/EOPL/libs/define-datatype.scm")
(load "/home/yukang/mycode/EOPL/libs/test-harness.scm")
(load "/home/yukang/mycode/EOPL/libs/test-suite.scm")


;; a very simple macro for inline testing
;; simple-minded magic for tests
(define equal??
  (lambda (x y)
    (if (equal? x y)
	"pass"
	"fail")))

;; (define-syntax equal??
;;   (syntax-rules ()
;;       ((_ x y)
;;        (let ((x^ x) (y^ y))
;;          (if (not (equal? x y))
;; 	     (eopl:error 'equal??
;; 		       "~s is not equal to ~s" 'x 'y)
;; 	     (#t))))))

(define report-unit-tests-completed
    (lambda (fn-name)
      (eopl:printf "unit tests completed: ~s~%" fn-name)))

