(let ((time-stamp "Time-stamp: <2000-12-27 16:43:20 dfried>"))
  (display "EOPL test loader ")
  (display (substring time-stamp 13 29))
  (newline))

; *-init file must be loaded first.

;;; Must be a macro, because the tests aren't defined until the
;;; files are loaded. 

(define-syntax gentest
  (syntax-rules ()
    ((gentest files tests)
     (begin
       (for-each load files)
       (for-each (lambda (t) (t)) tests)))))

;; test suites are already loaded by *-init.scm

(define gen3
  (lambda files
    (gentest files (list run-all))))

(define (test3-5)   (gen3 "3-5.scm"))
(define (test3-6-1) (gen3 "3-6-basis.scm" "3-6-letrec1.scm"))
(define (test3-6-2) (gen3 "3-6-basis.scm" "3-6-letrec2.scm"))
(define (test3-6-3) (gen3 "3-6-basis.scm" "3-6-letrec3.scm"))
(define (test3-7)   (gen3 "3-7.scm"))
(define (test3-8-1) (gen3 "3-8name.scm"))
(define (test3-8-2) (gen3 "3-8need.scm"))
(define (test3-8-3) (gen3 "3-8ref.scm"))
(define (test3-9)   (gen3 "3-9.scm"))

(define gen4
  (lambda files
    (gentest files
      (list
        run-all
        (lambda () (display '*****) (newline))
        check-all
        ))))

(define (test4-2) (gen4 "4-2.scm"))
(define (test4-3) (gen4 "4-3.scm"))
(define (test4-4) (gen4 "4-4.scm"))

(define (test5-1) (gen3 "5-3.scm" "5-4-1.scm"))
(define (test5-2) (gen3 "5-3.scm" "5-4-2.scm"))
(define (test5-3) (gen3 "5-3.scm" "5-4-3.scm"))
(define (test5-4) (gen3 "5-3.scm" "5-4-4.scm"))

(define (test6)
  (gentest
    '("6-top.scm") 
    (list
      interpret-all
      (lambda () (display '*****) (newline))
      check-all
      (lambda () (display '*****) (newline))
      translate-all
      )))

(define (test7-1)   (gen3 "7-1.scm"))
(define (test7-2a)  (gen3 "7-2a.scm"))
(define (test7-2b)  (gen3 "7-2b.scm"))
(define (test7-3)  (gen3 "7-3.scm"))
(define (test7-4)  (gen3 "7-4.scm"))
(define (test7-5)  (gen3 "7-5.scm"))
(define (test7-6)  (gen3 "7-6.scm"))

(define gen8
  (lambda files
    (gentest files (list cps-all))))

(define (test8-4) (gen8 "8-4.scm"))
(define (test8-4danvy) (gen8 "8-4danvy.scm"))
(define (test8-5print) (gen8 "8-5print.scm"))
(define (test8-5set) (gen8 "8-5set.scm"))

(define do-all-tests
  (lambda ()
    (for-each 
      (lambda (p) (p))
      (list
        test3-5 test3-6-1 test3-6-2 test3-6-3
        test3-7 test3-8-1 test3-8-2 test3-8-3
        test3-9
        test4-2 test4-3 test4-4
        test5-1 test5-2 test5-3 test5-4
        test6
        test7-1 test7-2a test7-2b
        test7-3 test7-4 test7-5
        test7-6
        test8-4 test8-4danvy
        test8-5print                    ; includes letcc
        test8-5set
        (lambda () (eopl:printf "no bugs found~%"))))))

(define test-rep
  (lambda ()
    (load "3-5.scm")
    (read-eval-print)))