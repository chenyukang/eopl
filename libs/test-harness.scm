(let ((time-stamp "Time-stamp: <2000-12-21 15:24:57 wand>"))
  (eopl:printf "test-harness.scm: unified test harness ~a~%" (substring time-stamp 13 29)))

;;; unified test harness for all chapters

;;;;;; datatype for test items
;; a test consists of a name, a group, a program, and a sequence of
;; outcomes. 

;; the outcomes can be:
;; 1.  a value  -- the test is passed if the actual value is within
;; equal-external-reps? of this value.
;; 2.  'dontrun  -- don't perform this test (always passes)
;; 3.  'error    -- the test is passed if it calls eopl:error-stop

;; representation:
;; (name group program outcomes)

(define make-test
  (lambda (name group program outcomes)
    (list name group program outcomes)))

(define test->name
  (lambda (l) (list-ref l 0)))

(define test->group
  (lambda (l) (list-ref l 1)))

(define test->program
  (lambda (l) (list-ref l 2)))

(define test->outcomes
  (lambda (l) (list-ref l 3)))

(define assq-test-list assq)            ; interface procedure for
                                        ; searching 

;; the first outcome is always for executing the program.  The second,
;; if present is for the type.  But we don't input them in that order!

(define use-execution-outcome car)

(define use-checker-outcome   cadr)

(define use-translation-outcome
  (lambda (outcomes)
    (let ((checker-outcome (use-checker-outcome outcomes)))
      (if (memv checker-outcome '(dontrun error))
        checker-outcome
        (use-execution-outcome outcomes)))))

;;;;;; interface to the test suite database

(define all-tests '())

(define initialize-test-suite!
  (lambda ()
    (set! all-tests '())))

;; replace test of the same name, otherwise add to end:

(define add-to-all-tests!
  (lambda (the-test)
    (let ((name (test->name the-test)))
      (set! all-tests
        (let loop ((tests all-tests))
          (cond 
            ((null? tests)
             (list the-test)) 
            ((eqv? (test->name (car tests)) name)
             (cons the-test (cdr tests)))
            (else (cons (car tests) (loop (cdr tests))))))))))

;;; main entries for adding test items:

(define add-test!
   (lambda (group name program correct-answer)
     (add-to-all-tests!
       (make-test name group program (list correct-answer)))))

(define add-typed-test!
  (lambda (group name program correct-type correct-answer)
    (add-to-all-tests!
      (make-test name group program (list correct-answer correct-type)))))

;;; ;;; running tests

;; safely run a test:  run (test pgm); if eopl:error-stop is reported,
;; then error-val is returned instead.

(define safely-run-experiment-on-program
  (lambda (experiment pgm error-val)
    (call-with-current-continuation
      (lambda (exit)
        ;; redefine eopl:error-stop for the
        ;; duration of the check, and reset it
        ;; no matter what.
        (let ((alpha (lambda () (exit error-val))))
          (let ((swap (lambda ()
                        (let ((temp eopl:error-stop))
                          (set! eopl:error-stop alpha)
                          (set! alpha temp)))))
            (dynamic-wind
              swap
              (lambda () (experiment pgm))
              swap)))))))

;; run one set of tests:  run the-experiment each element of tests
;; that is a member of one of the groups.   Compare the outcome with
;; outcome number N (usually 0 or 1) 

(define stop-after-first? #t)
(define show-correct-answers? #t)

(define run-experiment
  (lambda (the-experiment outcome-selector groups tests)
    (let ((bugs '()))
      (for-each 
        (lambda (test)
          (let ((name (test->name test))
                (pgm  (test->program test)))
            (if (memq (test->group test) groups)
              (let ((correct-outcome
                      (outcome-selector (test->outcomes test))))
                (begin
                  (eopl:printf "test: ~a~%~a~%" name pgm)
                  (if (eqv? correct-outcome 'dontrun)
                    (eopl:printf "test skipped~%~%")
                    (let ((ans
                            (safely-run-experiment-on-program
                              the-experiment pgm 'error)))
                      (if show-correct-answers?
                        (eopl:printf "correct outcome: ~a~%" correct-outcome))
                        (eopl:printf "actual outcome:  ")
                        (eopl:pretty-print ans)
                      (if (equal-external-reps? ans correct-outcome)
                        (eopl:printf "correct~%~%")
                        (begin
                          (eopl:printf "incorrect~%~%")
                          ;; stop on first error if stop-after-first? is set:
                          (if stop-after-first? (eopl:error-stop)) 
                          (set! bugs
                            (cons name bugs)))))))))))
        tests)
      (if (null? bugs)
        (eopl:printf "no bugs found~%")
        (eopl:printf "incorrect answers on tests: ~a~%" bugs)))))

;; handy to have:
(define run-test
  (lambda (experiment name)
      (experiment
        (test->program
          (assq-test-list name all-tests)))))

;; also handy to have:  checks to see if two list structures differ
;; only up to gensyms.  Builds a table to make sure the renaming is
;; consistent.  table1 and table2 are inverses -- association lists of
;; symbol vs. symbol.

(define equal-up-to-gensyms?
  (lambda (rep1 rep2)
    (let ((table1 '()) (table2 '()))
      (let loop ((rep1 rep1) (rep2 rep2))
;         (eopl:printf "loop: table1 = ~s ~%table2 = ~s~%rep1 = ~s~%rep2 = ~s~%~%"
;           table1 table2 rep1 rep2)
        (cond
          ((and (null? rep1) (null? rep2)) #t)
          ((and (number? rep1) (number? rep2))
           (= rep1 rep2))
          ((and (symbol? rep1) (symbol? rep2))
           (if (analyze-symbols rep1 rep2)
             (let ((assoc1 (assoc rep1 table1))
                   (assoc2 (assoc rep2 table2)))
               (cond
                 ((and (not assoc1) (not assoc2))
                  ;; neither in table, so add them and report OK
                  (set! table1 (cons (cons rep1 rep2) table1))
                  (set! table2 (cons (cons rep2 rep1) table2))
                  #t)
                 ((or (not assoc1) (not assoc2))
                  ;; one is in the table, and one isnt -- bad!
                  #f)
                 (else
                   ;; both in the table -- make sure they match
                   (and
                     (eqv? rep1 (cdr assoc2))
                     (eqv? rep2 (cdr assoc1))))))
             ;; not the same root -- they must be different
             #f))
          ((and (pair? rep1) (pair? rep2))
           (and
             (loop (car rep1) (car rep2))
             (loop (cdr rep1) (cdr rep2))))
          (else #f))))))
                   

;; returns #t iff sym1 and sym2 differ up to a numeric tail.
;; this also allows foo to match foo8 (which is ok, and used in 4-3.scm).

(define analyze-symbols
  (lambda (sym1 sym2)
    (let loop ((lst1 (symbol->list sym1))
               (lst2 (symbol->list sym2)))
      (cond
        ((and (null? lst1) (null? lst2)) #t)
        ((and (list-of-digits? lst1) (list-of-digits? lst2))
         #t)
        ((or (list-of-digits? lst1) (list-of-digits? lst2))
         #f)
        ((eqv? (car lst1) (car lst2))
         (loop (cdr lst1) (cdr lst2)))
        (else #f)))))


(define symbol->list
  (lambda (x) (string->list (symbol->string x))))

(define list-of-digits?
  (lambda (lst)
    (cond
      ((null? lst) #t)
      ((char-numeric? (car lst))
       (list-of-digits? (cdr lst)))
      (else #f))))
