(let ((time-stamp "Time-stamp: <2001-06-03 10:15:24 dfried>"))
  (eopl:printf "7-5.scm: continuation-passing interpreter with threads ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level ;;;;;;;;;;;;;;;;

(define run
  (lambda (string)
    (run-with-quantum 50 string)))

(define run-with-quantum
  (lambda (quantum string)
    (eval-program quantum (scan&parse string))))

(define run-all
  (lambda ()
    (run-experiment run use-execution-outcome
      '(lang3-5 lang3-6 lang3-7 lang7-5) ; but no exceptions, so no 7-4.
      all-tests)))

(define run-one
  (lambda (test-name)
    (run-test run test-name)))

;; needed for testing
(define equal-external-reps? equal?)

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (number (digit (arbno digit)) number)))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)   
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)
    (expression
      ("let" (arbno  identifier "=" expression) "in" expression)
      let-exp)
    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp)
    (expression
      ("(" expression (arbno expression) ")")
      app-exp)
    (expression ("set" identifier "=" expression) varassign-exp)
    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp)
    (expression                         
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression)
      letrec-exp)

    ;; no exceptions in this one, so we don't have to mess with find-handler.

    (expression ("spawn" expression) spawn-exp)    

    (expression ("lock" expression) lock-exp) 
    (expression ("acquire" expression) acquire-exp)
    (expression ("release" expression) release-exp)

    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-test-prim)
    (primitive ("equal?") equal-prim)

    (primitive ("list") list-prim)
    (primitive ("cons") cons-prim)
    (primitive ("nil")  nil-prim)
    (primitive ("car")  car-prim)
    (primitive ("cdr")  cdr-prim)
    (primitive ("null?") null?-prim)
    (primitive ("setcar") setcar-prim)

    (primitive ("print") print-prim)

    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define list-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; values ;;;;;;;;;;;;;;;;

(define expval?                         
  (lambda (x)
    (or (number? x) (procval? x)
      ((list-of expval?) x)
      (lock? x))))

;;;;;;;;;;;;;;;; threads ;;;;;;;;;;;;;;;;

(define make-thread (lambda (proc) proc))

; (define run-thread
;   (lambda (ticks thread)                  
;     (if (runnable? thread)
;       (if (zero? ticks)
;         thread
;         (run-thread (- ticks 1) (thread)))
;       thread)))

; (define run-thread
;   (lambda (ticks runnable-thread)
;     (if (zero? ticks)
;       runnable-thread
;       (let ((next-thread (runnable-thread)))
;         (if (runnable? next-thread)
;           (run-thread (- ticks 1) next-thread)
;           next-thread)))))

(define step-thread
  (lambda (runnable-thread) (runnable-thread)))

(define completed?
  (lambda (thread) (not (procedure? thread))))

;;;;;;;;;;;;;;;; the scheduler ;;;;;;;;;;;;;;;;

; (define schedule
;   (lambda (quantum thread)
;     (if (runnable? thread)
;       (begin
;         (place-on-ready-queue thread)
;         (schedule quantum
;           (run-thread quantum (get-next-from-ready-queue))))
;       thread)))

; (define schedule
;   (lambda (quantum runnable-thread)
;     (begin
;       (place-on-ready-queue runnable-thread)
;       (let
;         ((next-thread (run-thread quantum (get-next-from-ready-queue))))
;         (if (runnable? next-thread)
;           (schedule quantum next-thread)
;           next-thread)))))

(define schedule
  (lambda (quantum runnable-thread)
    (begin
      (place-on-ready-queue runnable-thread)
      (let timer-loop
        ((thread (get-next-from-ready-queue))
         (ticks quantum))
        (cond
          ((completed? thread) thread)
          ((zero? ticks) (schedule quantum thread))
          (else
            (timer-loop (step-thread thread) (- ticks 1))))))))

(define the-final-answer 'uninitialized)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

(define eval-program
  (lambda (quantum pgm)
    (initialize-ready-queue)
    (cases program pgm
      (a-program (exp)
        (schedule quantum
          (make-thread
            (lambda ()
              (eval-expression exp
                (init-env)
                (halt-cont)))))))))

(define eval-expression                 ; exp * env * cont -> expval
  (lambda (exp env cont)
    (cases expression exp
      (lit-exp (datum) (apply-cont cont datum))
      (var-exp (id) (apply-cont cont (apply-env env id)))
      (proc-exp (ids body)
        (apply-cont cont (closure ids body env)))
      (letrec-exp (proc-names idss bodies letrec-body)
        (eval-expression letrec-body
          (extend-env-recursively proc-names idss bodies env)
          cont))
      ;; an easy non-simple guy
      (if-exp (test-exp true-exp false-exp)
        (eval-expression test-exp env
          (test-cont true-exp false-exp env cont)))
      (primapp-exp (prim rands)
        (eval-rands rands env (prim-args-cont prim cont)))
      (let-exp (ids rands body)
        (eval-rands rands env
          (let-exp-cont ids env body cont)))
      (app-exp (rator rands)
        (eval-expression rator env
          (eval-rator-cont rands env cont)))
      (varassign-exp (id rhs-exp)
        (eval-expression rhs-exp env
          (varassign-cont env id cont)))
      (begin-exp (exp1 exps)
        (eval-expression exp1 env
          (begin-cont exps env cont)))

      (spawn-exp (exp)
        (begin 
          (place-on-ready-queue
            (make-thread
              (lambda ()
                (eval-expression exp env (die-cont)))))
          (apply-cont cont 1)))

      (lock-exp (exp)
        (eval-expression exp env
          (lock-cont cont)))
      (acquire-exp (exp)       
        (eval-expression exp env
          (acquire-cont cont)))
      (release-exp (exp)       
        (eval-expression exp env
          (release-cont cont)))

      )))

(define eval-rands
  (lambda (rands env cont)
    (if (null? rands) (apply-cont cont '())
      (eval-expression (car rands) env
        (eval-first-cont rands env cont)))))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim  () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim  () (* (car args) (cadr args)))
      (incr-prim  () (+ (car args) 1))
      (decr-prim  () (- (car args) 1))
      (zero-test-prim () (if (zero? (car args)) 1 0))
      (equal-prim () (if (equal? (car args) (cadr args)) 1 0))
      (list-prim () args)               ;already a list
      (nil-prim () '())
      (car-prim () (car (car args)))
      (cdr-prim () (cdr (car args)))
      (cons-prim () (cons (car args) (cadr args)))
      (null?-prim () (if (null? (car args)) 1 0))
      (setcar-prim () (begin (set-car! (car args) (cadr args)) 1))

      (print-prim () (begin (eopl:printf "~s~%" (car args)) 1))
      )))

(define init-env 
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      (empty-env))))

;;;;;;;;;;;;;;;; booleans ;;;;;;;;;;;;;;;;

(define true-value?
  (lambda (x)
    (not (zero? x))))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype procval procval?
  (closure 
    (ids (list-of symbol?)) 
    (body expression?)
    (env environment?)))

(define apply-procval
  (lambda (proc args cont)
    (cases procval proc
      (closure (ids body env)
        (eval-expression body
          (extend-env ids args env)
          cont)))))
               
;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

(define-datatype continuation continuation?
  (halt-cont)
  (test-cont
    (true-exp expression?)
    (false-exp expression?)
    (env environment?)
    (cont continuation?))
  (prim-args-cont (prim primitive?)
    (cont continuation?))
  (let-exp-cont
    (ids (list-of symbol?))
    (env environment?)
    (body expression?)
    (cont continuation?))
  (eval-rator-cont
    (rands (list-of expression?))
    (env environment?)
    (cont continuation?))
  (eval-rands-cont
    (proc expval?)
    (cont continuation?))
  (eval-first-cont 
    (exps (list-of expression?))
    (env environment?)
    (cont continuation?))
  (eval-rest-cont 
    (first-value expval?)
    (cont continuation?))
  (varassign-cont
    (env environment?)
    (id symbol?)
    (cont continuation?))
  (begin-cont
    (exps (list-of expression?))
    (env environment?)
    (cont continuation?))
  (die-cont)
  (lock-cont
    (cont continuation?))
  (acquire-cont
    (cont continuation?))
  (release-cont
    (cont continuation?))
  )

(define apply-cont
  (lambda (cont val)
    (make-thread
      (lambda ()
        (cases continuation cont
          (halt-cont ()
            (begin
              (eopl:printf "The final answer is: ~s~%" val)
              (set! the-final-answer val)
              (get-next-from-ready-queue)))
          (test-cont (true-exp false-exp env cont)
            (if (true-value? val)
              (eval-expression true-exp env cont)
              (eval-expression false-exp env cont)))
          (prim-args-cont (prim cont)
            (let ((args val))
              (apply-cont cont (apply-primitive prim args))))
          (let-exp-cont (ids env body cont)
            (let ((new-env (extend-env ids val env)))
              (eval-expression body new-env cont)))
          (eval-rands-cont (proc cont)
            (let ((args val))
              (if (procval? proc)
                (apply-procval proc args cont)
                (eopl:error 'eval-expression 
                  "Attempt to apply non-procedure ~s" proc))))
          (eval-rator-cont (rands env cont) 
            (let ((proc val))
              (eval-rands rands env (eval-rands-cont proc cont))))
          (eval-rest-cont (first cont)
            (let ((rest val))
              (apply-cont cont (cons first rest))))
          (eval-first-cont (exps env cont)
            (eval-rands (cdr exps) env
              (eval-rest-cont val cont)))
          (varassign-cont (env id cont)
            (begin
              (setref! (apply-env-ref env id) val)
              (apply-cont cont 1)))
          (begin-cont (exps env cont)
            (if (null? exps)
              (apply-cont cont val)
              (eval-expression (car exps) env
                (begin-cont (cdr exps) env cont))))

          (die-cont ()
            (get-next-from-ready-queue))

          (lock-cont (cont)
            (let ((c (cell 0)))
              (apply-cont cont (a-lock c val))))

          (acquire-cont (cont1)
            (if (lock? val)
              (cases lock val
                (a-lock (occupied value)
                  (if (= (contents occupied) 0)
                    (begin
                      (setcell occupied 1)
                      (apply-cont cont1 value))
                    (begin
                      (place-on-ready-queue
                        (apply-cont cont val)) 
                      (get-next-from-ready-queue)))))
              (eopl:error 'acquire-cont
                "Non-lock to acquire: ~s~%" val)))

          (release-cont (cont)
            (if (lock? val)
              (cases lock val
                (a-lock (occupied value)
                  (if (= (contents occupied) 1)
                    (begin
                      (setcell occupied 0)
                      (apply-cont cont 1))
                    (eopl:error 'release-cont
                      "Must acquire lock before releasing"))))
              (eopl:error 'release-cont
                "Non-lock to release: ~s~%" val)))


          )))))

;;;;;;;;;;;;;;;; locks ;;;;;;;;;;;;;;;;

(define-datatype lock lock?
  (a-lock
    (occupied 
      (lambda (x)
        (and (cell? x) (integer? (contents x)))))
    (value expval?)))

;;;;;;;;;;;;;;;; references ;;;;;;;;;;;;;;;;

(define-datatype reference reference?
  (a-ref
    (position integer?)
    (vec vector?)))

(define primitive-deref 
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
        (vector-ref vec pos)))))

(define primitive-setref!
  (lambda (ref value)
    (cases reference ref
      (a-ref (pos vec)
        (vector-set! vec pos value)))))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define setref!
  (lambda (ref value)
    (primitive-setref! ref value)))

;;;;;;;;;;;;;;;; cells ;;;;;;;;;;;;;;;;

(define cell
  (lambda (v)
    (a-ref 0 (vector v))))

(define cell? reference?)

(define contents deref)

(define setcell setref!)

;;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vec vector?)              ; can use this for anything.
    (env environment?))
  )

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
              (a-ref pos vals)
              (apply-env-ref env sym)))))))

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

;;;;;;;;;;;;;;;; queues ;;;;;;;;;;;;;;;;

;; from chapter 2

(define create-queue
  (lambda ()
    (let ((q-in '())
          (q-out '()))
      (letrec
        ((reset-queue
           (lambda ()
             (set! q-in '())
             (set! q-out '())))
         (empty-queue?
           (lambda ()
             (and
               (null? q-in)
               (null? q-out))))
         (enqueue
           (lambda (x)
             (set! q-in (cons x q-in))))
         (dequeue
           (lambda ()
             (if (empty-queue?)
               (eopl:error 'dequeue "Not on an empty queue")
               (begin
                 (if (null? q-out)
                   (begin
                     (set! q-out (reverse q-in))
                     (set! q-in '())))
                 (let ((ans (car q-out)))
                   (set! q-out (cdr q-out))
                   ans))))))
        (vector reset-queue empty-queue? enqueue dequeue)))))

(define queue-get-reset-operation   (lambda (q) (vector-ref q 0)))
(define queue-get-empty?-operation  (lambda (q) (vector-ref q 1)))
(define queue-get-enqueue-operation (lambda (q) (vector-ref q 2)))
(define queue-get-dequeue-operation (lambda (q) (vector-ref q 3)))

;;;;;;;;;;;;;;;; the ready queue ;;;;;;;;;;;;;;;;

(define the-ready-queue (create-queue))

(define initialize-ready-queue
  (queue-get-reset-operation the-ready-queue))

(define place-on-ready-queue
  (queue-get-enqueue-operation the-ready-queue))

(define get-next-from-ready-queue
  (let ((empty? (queue-get-empty?-operation the-ready-queue))
        (dequeue
          (queue-get-dequeue-operation the-ready-queue)))
    (lambda ()
      (if (empty?) the-final-answer (dequeue)))))


