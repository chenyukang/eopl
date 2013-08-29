(load "/Users/kang/code/eopl/libs/r5rs.scm")
(load "/Users/kang/code/eopl/libs/sllgen.scm")
(load "/Users/kang/code/eopl/libs/define-datatype.scm")
(load "/Users/kang/code/eopl/libs/test-harness.scm")
(load "/Users/kang/code/eopl/libs/test-suite.scm")
(load "/Users/kang/code/eopl/libs/grammar.scm")

(define equal??
  (lambda (x y)
    (if (equal? x y)
        (display "pass\n")
        (display "fail\n"))))
