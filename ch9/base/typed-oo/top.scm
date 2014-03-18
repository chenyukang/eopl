  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;

  ;; run : String -> ExpVal

  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))

  ;; run-all : () -> unspecified
  ;; runs all the tests in test-list, comparing the results with
  ;; equal-answer?

  (define run-all
    (lambda ()
      (run-tests! run equal-answer? tests-for-run)))

  (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans (sloppy->expval correct-ans))))

  (define sloppy->expval
    (lambda (sloppy-val)
      (cond
        ((number? sloppy-val) (num-val sloppy-val))
        ((boolean? sloppy-val) (bool-val sloppy-val))
        ((list? sloppy-val) (list-val (map sloppy->expval sloppy-val)))
        (else
         (eopl:error 'sloppy->expval
                     "Can't convert sloppy value to expval: ~s"
                     sloppy-val)))))

  ;; run-one : Sym -> ExpVal
  ;; (run-one sym) runs the test whose name is sym

  (define run-one
    (lambda (test-name)
      (let ((the-test (assoc test-name tests-for-run)))
        (cond
          ((assoc test-name tests-for-run)
           => (lambda (test)
                (run (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; check : String -> ExternalType

  (define check
    (lambda (string)
      (type-to-external-form
        (type-of-program (scan&parse string)))))

  ;; check-all : () -> Unspecified
  ;; checks all the tests in test-list, comparing the results with
  ;; equal-answer?

  (define check-all
    (lambda ()
      (run-tests! check equal? tests-for-check)))

  ;; check-one : Sym -> ExpVal
  ;; (check-one sym) checks the test whose name is sym

  (define check-one
    (lambda (test-name)
      (let ((the-test (assoc test-name tests-for-check)))
        (cond
          (the-test
           => (lambda (test)
                (check (cadr test))))
          (else (eopl:error 'check-one "no such test: ~s" test-name))))))

  (stop-after-first-error #t)
  ;; (check-all)
  ;; (run-all)
