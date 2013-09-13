(load "../libs/r5rs.scm")
(load "../libs/sllgen.scm")
(load "../libs/define-datatype.scm")
;;(load "../libs/test-harness.scm")
;;(load "../libs/test-suite.scm")
(load "../libs/grammar.scm")

(define equal??
  (lambda (x y)
    (if (equal? x y)
        (display "pass\n")
        (display "fail\n"))))
