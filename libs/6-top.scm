;;; This is 6-top.scm: loader for typed OOP language

(let ((time-stamp "Time-stamp: <2000-12-15 16:07:24 wand>"))
  (eopl:printf "6-top.scm: loader for typed OOP language ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level interface ;;;;;;;;;;;;;;;;

(define type-check
  (lambda (string)
    (type-to-external-form
      (type-of-program
        (scan&parse string)))))

(define run
  (lambda (string)
    (eval-program (scan&parse string))))

(define translate
  (lambda (string)
    (translation-of-program (scan&parse string))))

(define all-groups '(lang4-2 lang6))

(define interpret-all
  (lambda ()
    (run-experiment run use-execution-outcome
      all-groups all-tests)))

(define check-all
  (lambda ()
    (run-experiment type-check use-checker-outcome
      all-groups all-tests)))

(define translate-verbose? #f)

(define toggle-verbose
  (lambda ()
    (set! translate-verbose? (not translate-verbose?))))

(define translate-all
  (lambda ()
    (run-experiment
      (lambda (string)
        (let ((the-translation (translate string)))
          (if translate-verbose? (eopl:pretty-print the-translation))
          (eval-program the-translation)))
      use-translation-outcome
      all-groups all-tests)))

(define check-one
  (lambda (test-name)
    (run-test type-check test-name)))

(define translate-one
  (lambda (test-name)
    (run-test translate test-name)))

(define translate-and-run-one
  (lambda (test-name)
    (run-test
      (lambda (string) (eval-program (translate string)))
      test-name)))

(define equal-external-reps? equal?)

(define reload (lambda () (load "6-top.scm")))

(load "6-grammar.scm")
(load "6-interp.scm")
(load "6-checker.scm")
(load "6-translator.scm")
; the tests are now in test-suite.scm
; (load "6-tests.scm")

