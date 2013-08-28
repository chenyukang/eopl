(let ((time-stamp "Time-stamp: <2000-12-18 15:09:25 wand>"))
  (eopl:printf "7-3.scm: imperative continuation-passing interpreter ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level ;;;;;;;;;;;;;;;;

(define run
  (lambda (string)
    (eval-program (scan&parse string))))

(define run-all
  (lambda ()
    (run-experiment run use-execution-outcome
      '(lang3-5 lang3-6 lang3-7) all-tests)))

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
;     (expression
;       ("begin" expression (arbno ";" expression) "end")
;       begin-exp)
    (expression                         
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression)
      letrec-exp)

    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-test-prim)
    
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
    (or (number? x) (procval? x))))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

(define exp   'uninitialized)
(define env   'uninitialized)
(define cont  'uninitialized)
(define rands 'uninitialized)
(define val   'uninitialized)
(define proc  'uninitialized)
(define args  'uninitialized)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)                 
        (set! exp exp1)
        (set! env (init-env))
        (set! cont (halt-cont))
        (eval-expression)))))

(define eval-expression
  (lambda ()
    (cases expression exp
      (lit-exp (datum) 
        (set! val datum)
        (apply-cont))
      (var-exp (id)
        (set! val (apply-env env id))
        (apply-cont))
      (proc-exp (ids body)
        (set! val (closure ids body env))
        (apply-cont))
      (letrec-exp (proc-names idss bodies letrec-body)
        (set! exp letrec-body)
        (set! env
          (extend-env-recursively proc-names idss bodies env))
        (eval-expression))
      (if-exp (test-exp true-exp false-exp)
        (set! exp test-exp)
        (set! cont (test-cont true-exp false-exp env cont))
        (eval-expression))
      (varassign-exp (id rhs-exp)       
        (set! exp rhs-exp)
        (set! cont (varassign-cont env id cont))
        (eval-expression))
      (primapp-exp (prim rands1)
        (set! cont (prim-args-cont prim cont))
        (set! rands rands1)
        (eval-rands))
      (let-exp (ids rands1 body)
        (set! rands rands1)
        (set! cont (let-exp-cont ids env body cont))
        (eval-rands))
      (app-exp (rator rands)
        (set! exp rator)
        (set! cont (eval-rator-cont rands env cont))
        (eval-expression))
;^    (begin-exp (exp1 exps)
;^      (eopl:error 'eval-expression
;^        "Begin not implemented in 7-3.scm"))
      )))

(define eval-rands
  (lambda ()
    (if (null? rands)
      (begin
       (set! val '())
       (apply-cont))
      (begin
        (set! exp (car rands))
        (set! cont (eval-first-cont rands env cont))
        (eval-expression)))))



(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim  () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim  () (* (car args) (cadr args)))
      (incr-prim  () (+ (car args) 1))
      (decr-prim  () (- (car args) 1))
      (zero-test-prim () (if (zero? (car args)) 1 0))
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
  (lambda ()
    (cases procval proc
      (closure (ids body old-env)
        (set! exp body)
        (set! env (extend-env ids args old-env))
        (eval-expression)))))
               
;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

(define-datatype continuation continuation?
  (halt-cont)
  (test-cont
    (true-exp expression?)
    (false-exp expression?)
    (env environment?)
    (cont continuation?))
  (prim-args-cont
    (prim primitive?)
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
    (cont continuation?)))

(define apply-cont
  (lambda ()
    (cases continuation cont
      (halt-cont ()
        (begin
          (eopl:printf "The answer is: ~s~%" val)
          val))
      (test-cont (true-exp false-exp old-env old-cont)
        (if (true-value? val)
          (begin
            (set! exp true-exp)
            (set! env old-env)
            (set! cont old-cont)
            (eval-expression))
          (begin
            (set! exp false-exp)
            (set! env old-env)
            (set! cont old-cont)
            (eval-expression))))
      (prim-args-cont (prim old-cont)
        (let ((args val))
          (set! cont old-cont)
          (set! val (apply-primitive prim args))
          (apply-cont)))
      (let-exp-cont (ids old-env body old-cont)
        (let ((new-env (extend-env ids val old-env)))
          (set! exp body)
          (set! env new-env)
          (set! cont old-cont)
          (eval-expression)))
      (eval-rands-cont (old-proc old-cont)
        (let ((new-args val))
          (if (procval? old-proc)
            (begin
              (set! proc old-proc)
              (set! args new-args)
              (set! cont old-cont)
              (apply-procval))
            (eopl:error 'eval-expression 
              "Attempt to apply non-procedure ~s" proc))))
      (eval-rator-cont (rands1 old-env old-cont) 
        (let ((proc val))
          (set! rands rands1)
          (set! env old-env)
          (set! cont (eval-rands-cont proc old-cont))
          (eval-rands)))
      (eval-rest-cont (first-val old-cont)
        (let ((rest val))
          (set! cont old-cont)
          (set! val (cons first-val rest))
          (apply-cont)))
      (eval-first-cont (old-rands old-env old-cont)
        (set! rands (cdr old-rands))
        (set! env old-env)
        (set! cont (eval-rest-cont val old-cont))
        (eval-rands))
      (varassign-cont (old-env id old-cont)
        (begin
          (setref! (apply-env-ref old-env id) val)
          (set! cont old-cont)
          (set! val 1)
          (apply-cont)))
      )))

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

