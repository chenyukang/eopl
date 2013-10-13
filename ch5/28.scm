(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/letrec-cases.scm")

;; based on 26.scm
;; Instrument the interpreter, and verify that trace is same with interpreter in 5.12

 ;;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define the-grammar
  '((program (expression) a-program)

    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("letrec"
      identifier "(" (arbno identifier) ")" "=" expression
      "in" expression)
     letrec-exp)

    (expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))


(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?)))

;;; extractors:
(define expval->num
  (lambda (v)
    (cases expval v
           (num-val (num) num)
           (else (expval-extractor-error 'num v)))))

(define expval->bool
  (lambda (v)
    (cases expval v
           (bool-val (bool) bool)
           (else (expval-extractor-error 'bool v)))))

(define expval->proc
  (lambda (v)
    (cases expval v
           (proc-val (proc) proc)
           (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (error 'expval-extractors "Looking for a ~s, found ~s"
           variant value)))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;
(define identifier? symbol?)

(define-datatype continuation continuation?
  (end-cont)
  (zero1-cont
   (saved-cont continuation?))
  (let-exp-cont
   (var identifier?)
   (body expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (diff1-cont
   (exp2 expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (diff2-cont
   (val1 expval?)
   (saved-cont continuation?))
  (rator-cont
   (rands (list-of expression?))
   (vals (list-of expression?))
   (saved-env environment?)
   (saved-cont continuation?))
  (rand-cont
   (val1 expval?)
   (rands (list-of expression?))
   (vals (list-of expval?))
   (saved-cont continuation?)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;
(define-datatype proc proc?
  (procedure
   (bvar (list-of symbol?))
   (body expression?)
   (env environment?)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec
   (p-name symbol?)
   (b-var (list-of symbol?))
   (p-body expression?)
   (saved-env environment?)))

(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;
(define apply-env
  (lambda (env search-sym)
    (cases environment env
           (empty-env ()
                      (error 'apply-env "No binding for ~s" search-sym))
           (extend-env (var val saved-env)
                       (if (eqv? search-sym var)
                           val
                           (apply-env saved-env search-sym)))
           (extend-env-rec (p-name b-var p-body saved-env)
                           (if (eqv? search-sym p-name)
                               (proc-val (procedure b-var p-body env))
                               (apply-env saved-env search-sym))))))

(define exp 'uninitialized)
(define env 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define cur-proc 'uninitialized)

;; new stuff
(define trampoline
  (lambda (pc)
    (if (expval? pc)
	pc
	(trampoline (pc)))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (body)
                      (set! cont (end-cont))
                      (set! exp body)
                      (set! env (init-env))
		      (trampoline (value-of/k))))))

;; value-of : Exp * Env * Cont -> FinalAnswer
;; value-of/k : () -> FinalAnswer
;; usage : relies on registers
;;      exp  : Exp
;;      env  : Env
;;      cont : Cont
;; The code from the corresponding portions of interp.scm is shown
;; as comments.
(define value-of/k
  (lambda ()
    (cases expression exp
           (const-exp (num)
                      (set! val (num-val num))
                      (apply-cont))

           (var-exp (var)
                    (set! val (apply-env env var))
                    (apply-cont))

           (proc-exp (vars body)
                     (set! val (proc-val (procedure vars body env)))
                     (apply-cont))

           (letrec-exp (p-name b-vars p-body letrec-body)
                       (set! exp letrec-body)
                       (set! env
                             (extend-env-rec p-name b-vars p-body env))
                       (value-of/k))

           (zero?-exp (exp1)
                      (set! cont (zero1-cont cont))
                      (set! exp exp1)
                      (value-of/k))

           (let-exp (var exp1 body)
                    (set! cont (let-exp-cont var body env cont))
                    (set! exp exp1)
                    (value-of/k))

           (if-exp (exp1 exp2 exp3)
                   (set! cont (if-test-cont exp2 exp3 env cont))
                   (set! exp exp1)
                   (value-of/k))

           (diff-exp (exp1 exp2)
                     (set! cont (diff1-cont exp2 env cont))
                     (set! exp exp1)
                     (value-of/k))

           (call-exp (rator rands)
                     (set! cont (rator-cont rands '() env cont))
                     (set! exp rator)
                     (value-of/k))
           )))

(define instrument (make-parameter #t))

;; apply-cont : Cont * ExpVal -> FinalAnswer
;; usage : reads registers
;;     cont : Cont
;;     val  : ExpVal
(define apply-cont
  (lambda ()
    (begin
      (if (instrument)
          (printf "apply-cont ~s ~s ~%" (expval->readable val) (cont->readable cont) ))
      (cases continuation cont
	     (end-cont ()
		       (printf "End of computation.~%")
		       val)
	     (zero1-cont (saved-cont)
			 (set! cont saved-cont)
			 (set! val (bool-val (zero? (expval->num val))))
			 (apply-cont))

	     (let-exp-cont (var body saved-env saved-cont)
			   (set! cont saved-cont)
			   (set! exp body)
			   (set! env (extend-env var val saved-env))
			   (value-of/k))

	     (if-test-cont (exp2 exp3 saved-env saved-cont)
			   (set! cont saved-cont)
			   (if (expval->bool val)
			       (set! exp exp2)
			       (set! exp exp3))
			   (set! env saved-env)
			   (value-of/k))

	     (diff1-cont (exp2 saved-env saved-cont)
			 (set! cont (diff2-cont val saved-cont))
			 (set! exp exp2)
			 (set! env saved-env)
			 (value-of/k))

	     (diff2-cont (val1 saved-cont)
			 (let ((num1 (expval->num val1))
			       (num2 (expval->num val)))
			   (set! cont saved-cont)
			   (set! val (num-val (- num1 num2)))
			   (apply-cont)))

	     (rator-cont (rands vals saved-env saved-cont)
			 (set! cont (rand-cont val (cdr rands) vals saved-cont))
			 (set! exp (car rands))
			 (set! env saved-env)
			 (value-of/k))

	     (rand-cont (rator-val rands vals saved-cont)
			(let ((rator-proc (expval->proc rator-val)))
			  (if (null? rands)
			      (begin
				(set! cont saved-cont)
				(set! cur-proc rator-proc)
				(set! val (append vals (list val)))
				(apply-procedure/k))
			      (begin
				(set! cont (rand-cont rator-val (cdr rands)
						      (append vals (list val))
						      saved-cont))
				(set! cur-proc rator-proc)
				(set! exp (car rands))
				(value-of/k)))))
	     ))))

;;new stuff
(define extend-env*
  (lambda (vars vals saved-env)
    (if (null? vars)
	saved-env
	(extend-env* (cdr vars) (cdr vals)
		     (extend-env (car vars) (car vals) saved-env)))))


;; new stuff
(define exp->readable
  (lambda (exp)
    (cond ((eqv? (car exp) 'diff-exp)
           (format "(- ~a ~a)" (exp->readable (cadr exp)) (exp->readable (caddr exp))))
          ((eqv? (car exp) 'const-exp)
           (cadr exp))
          ((eqv? (car exp) 'var-exp)
           (cadr exp))
          ((eqv? (car exp) 'proc-exp) "proc-exp")
          ((eqv? (car exp) 'letrec-exp) "letrec")
          ((eqv? (car exp) 'zero?-exp) "zero?")
          ((eqv? (car exp) 'let-exp) "let")
          ((eqv? (car exp) 'call-exp) "call")
          (else "other"))))

(define cont->readable
  (lambda (cont)
    (cond ((eqv? (car cont) 'end-cont) "end-cont")
          ((eqv? (car cont) 'zero1-cont) "zero1-cont")
          ((eqv? (car cont) 'let-exp-cont) "let-exp-cont")
          ((eqv? (car cont) 'if-test-cont) "if-test-cont")
          ((eqv? (car cont) 'diff1-cont) "diff1-cont")
          ((eqv? (car cont) 'diff2-cont) "diff2-cont")
          ((eqv? (car cont) 'rator-cont) "rator-cont")
          ((eqv? (car cont) 'rand-cont) "rand-cont")
          (else "other-cont"))))

(define expval->readable
  (lambda (val)
    (cond ((eqv? (car val) 'num-val) (cadr val))
          ((eqv? (car val) 'bool-val) (cadr val))
          (else
           "procedure"))))


;; new stuff
;; apply-procedure : Proc * ExpVal -> Bounce
;; apply-procedure/k : () -> FinalAnswer}
;; usage : relies on registers
;;     cur-proc : Proc
;;       val : ExpVal
;;      cont : Cont
(define apply-procedure/k
  (lambda ()
    (lambda ()
      (cases proc cur-proc
	     (procedure (vars body saved-env)
			(set! exp body)
			(set! env (extend-env* vars val saved-env))
			(value-of/k))))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(run "-(-(44, 11), 3)")

(add-test! '(multi-arg  "(proc(x,y) -(x, y) 10 20)" -10))
(add-test! '(multi-arg-letrec "letrec f(x y) = -(x, y) in (f 11 33)" -22))

(run-all)
