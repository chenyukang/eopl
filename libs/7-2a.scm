(let ((time-stamp "Time-stamp: <2000-12-18 16:42:39 wand>"))
  (eopl:printf "7-2a.scm: procedural representation of continuations c~a~%"
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

(define eval-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
        (eval-expression body (init-env) (halt-cont))))))

(define eval-expression ; exp * env * cont -> expval
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
      (if-exp (test-exp true-exp false-exp) ;^; an easy non-simple guy
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
      (varassign-exp (id exp)
        (eval-expression exp env (varassign-cont env id cont)))
;^    (begin-exp (exp1 exps)
;^      (eopl:error 'eval-expression
;^        "Begin not implemented in 7-2a.scm"))
      )))

(define eval-rands
  (lambda (rands env cont)
    (if (null? rands)
      (apply-cont cont '())
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

(define apply-cont
  (lambda (cont v)
    (cont v)))

(define halt-cont
  (lambda ()
    (lambda (val)
      (begin
        (eopl:printf "The answer is: ~s~%" val)
        val))))

(define test-cont
  (lambda (true-exp false-exp env cont)
    (lambda (val)
      (if (true-value? val)
        (eval-expression true-exp env cont)
        (eval-expression false-exp env cont)))))

(define prim-args-cont
  (lambda (prim cont)
    (lambda (val)
      (let ((args val))
        (apply-cont cont (apply-primitive prim args))))))

(define let-exp-cont
  (lambda (ids env body cont)
    (lambda (val)
      (let ((new-env (extend-env ids val env)))
        (eval-expression body new-env cont)))))

(define eval-rator-cont
  (lambda (rands env cont)
    (lambda (val)
      (let ((proc val))
        (eval-rands rands env
          (eval-rands-cont proc cont))))))

(define eval-rands-cont
  (lambda (proc cont)
    (lambda (val)
      (let ((args val))
        (if (procval? proc)
          (apply-procval proc args cont)
          (eopl:error 'eval-expression 
            "Attempt to apply non-procedure ~s" proc))))))

(define eval-first-cont
  (lambda (rands env cont)
    (lambda (val) 
      (eval-rands (cdr rands) env
        (eval-rest-cont val cont)))))

(define eval-rest-cont
  (lambda (first-val cont)
    (lambda (val)
      (let ((rest val))
        (apply-cont cont (cons first-val rest))))))


(define varassign-cont
  (lambda (env id cont)
    (lambda (val)
      (begin
        (setref! (apply-env-ref env id) val)
        (apply-cont cont 1)))))

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

