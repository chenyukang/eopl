;; here are some other things that could be provided:

  ;;   (provide (all-defined))
  ;;   (provide (all-from "interp.scm"))
  ;;   (provide (all-from "lang.scm"))

  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;

  ;; run : String -> ExpVal
  ;; Page: 71
  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))

  ;; run-all : () -> unspecified

  ;; runs all the tests in test-list, comparing the results with
  ;; equal-answer?

  (define run-all
    (lambda ()
      (run-tests! run equal-answer? test-list)))

  (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans (sloppy->expval correct-ans))))

  (define sloppy->expval
    (lambda (sloppy-val)
      (cond
        ((number? sloppy-val) (num-val sloppy-val))
        ((boolean? sloppy-val) (bool-val sloppy-val))
        (else
         (eopl:error 'sloppy->expval
                     "Can't convert sloppy value to expval: ~s"
                     sloppy-val)))))

  ;; run-one : symbol -> expval

  ;; (run-one sym) runs the test whose name is sym

  (define run-one
    (lambda (test-name)
      (let ((the-test (assoc test-name test-list)))
        (cond
          ((assoc test-name test-list)
           => (lambda (test)
                (run (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))

  ;; (run-all)
