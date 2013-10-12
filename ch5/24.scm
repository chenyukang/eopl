(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/letrec-cases.scm")

;; almost doesn't change code based on 23.scm,
;; have a try on the testcase of ./base/letrec-cases.scm
;; remove any "goto", means remove one call of value-of/k
;; then the result of a program will be "#<unspecified>"

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
     ("proc" "(" identifier ")" expression)
     proc-exp)

    (expression
     ("(" expression expression ")")
     call-exp)

    (expression
     ("letrec"
      identifier "(" identifier ")" "=" expression
      "in" expression)
     letrec-exp)

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
   (rand expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (rand-cont
   (val1 expval?)
   (saved-cont continuation?)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (bvar symbol?)
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
   (b-var symbol?)
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

;; value-of-program : Program -> FinalAnswer
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (body)
                      (set! cont (end-cont))
                      (set! exp body)
                      (set! env (init-env))
                      (value-of/k)))))

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
                      ;; (apply-cont cont (num-val num)))
                      (set! val (num-val num))
                      ;; cont is unchanged
                      (apply-cont))
           (var-exp (var)
                    ;; (apply-cont cont (apply-env env id)))
                    (set! val (apply-env env var))
                    ;; cont is unchanged
                    (apply-cont))
           (proc-exp (var body)
                     ;; (apply-cont cont (proc-val (procedure bvar body env))
                     (set! val (proc-val (procedure var body env)))
                     (apply-cont))
           (letrec-exp (p-name b-var p-body letrec-body)
                       ;; (value-of/k letrec-body
                       ;;   (extend-env-rec proc-name bvar proc-body env)
                       ;;   cont)
                       (set! exp letrec-body)
                       (set! env
                             (extend-env-rec p-name b-var p-body env))
                       (value-of/k))
           (zero?-exp (exp1)
                      ;; (value-of/k exp1 env (zero1-cont cont))
                      (set! cont (zero1-cont cont))
                      (set! exp exp1)
                      (value-of/k))
           (let-exp (var exp1 body)
                    ;; (value-of/k rhs env (let-exp-cont id body env cont))
                    (set! cont (let-exp-cont var body env cont))
                    (set! exp exp1)
                    (value-of/k))
           (if-exp (exp1 exp2 exp3)
                   ;; (value-of/k exp0 env (if-test-cont exp2 exp3 env cont))
                   (set! cont (if-test-cont exp2 exp3 env cont))
                   (set! exp exp1)
                   (value-of/k))
           (diff-exp (exp1 exp2)
                     ;; (value-of/k exp1 env (diff1-cont exp2 env cont))
                     (set! cont (diff1-cont exp2 env cont))
                     (set! exp exp1)
                     ;; env is unchanged
                     (value-of/k))
           (call-exp (rator rand)
                     ;; (value-of/k rator env (rator-cont rand env cont))
                     (set! cont (rator-cont rand env cont))
                     (set! exp rator)
                     (value-of/k))
           )))

;; apply-cont : Cont * ExpVal -> FinalAnswer
;; usage : reads registers
;;     cont : Cont
;;     val  : ExpVal
(define apply-cont
  (lambda ()
    (cases continuation cont

           (end-cont ()
                     (printf "End of computation.~%")
                     val)
           (zero1-cont (saved-cont)
                       ;; (apply-cont cont
                       ;;   (bool-val
                       ;;     (zero? (expval->num val))))
                       (set! cont saved-cont)
                       (set! val (bool-val (zero? (expval->num val))))
                       (apply-cont))
           (let-exp-cont (var body saved-env saved-cont)
                         ;; (value-of/k body (extend-env id val env) cont)
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
                       ;; (value-of/k exp2 env (diff2-cont val cont)))
                       (set! cont (diff2-cont val saved-cont))
                       (set! exp exp2)
                       (set! env saved-env)
                       (value-of/k))
           (diff2-cont (val1 saved-cont)
                       ;; (apply-cont cont (num-val (- num1 num2)))))
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val)))
                         (set! cont saved-cont)
                         (set! val (num-val (- num1 num2)))
                         (apply-cont)))
           (rator-cont (rand saved-env saved-cont)
                       ;; (value-of/k rand env (rand-cont val cont))
                       (set! cont (rand-cont val saved-cont))
                       (set! exp rand)
                       (set! env saved-env)
                       (value-of/k))
           (rand-cont (rator-val saved-cont)
                      (let ((rator-proc (expval->proc rator-val)))
                        ;; (apply-procedure rator-proc rator-val cont)
                        (set! cont saved-cont)
                        (set! cur-proc rator-proc)
                        (set! val val)
                        (apply-procedure/k)))
           )))

;; apply-procedure : Proc * ExpVal -> ExpVal
;; apply-procedure/k : () -> FinalAnswer}
;; usage : relies on registers
;;     cur-proc : Proc
;;       val : ExpVal
;;      cont : Cont
(define apply-procedure/k
  (lambda ()
    (cases proc cur-proc
           (procedure (var body saved-env)
                      (set! exp body)
                      (set! env (extend-env var val saved-env))
                      (value-of/k)))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(run-all)
