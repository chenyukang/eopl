(load-relative "../libs/init.scm")
(load-relative "./translator.scm")

;; Exercise3.38 Extend the lexical address translator and interpreter to handle cond from exercise 3.12.

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
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
     ("less?" "(" expression "," expression ")")
     less?-exp)

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

    ;;new stuff
    (expression
     ("cond" (arbno expression "==>" expression) "end")
     cond-exp)

    (expression
     ("%nameless-var" number) nameless-var-exp)
    (expression
     ("%let" expression "in" expression)
     nameless-let-exp)
    (expression
     ("%lexproc" expression)
     nameless-proc-exp)

    ))


(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?)))

;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (v)
    (cases expval v
           (num-val (num) num)
           (else (expval-extractor-error 'num v)))))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (v)
    (cases expval v
           (bool-val (bool) bool)
           (else (expval-extractor-error 'bool v)))))

;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (v)
    (cases expval v
           (proc-val (proc) proc)
           (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (error 'expval-extractors "Looking for a ~s, found ~s"
           variant value)))


;; procedure : Exp * Nameless-env -> Proc
(define-datatype proc proc?
  (procedure
   ;; in LEXADDR, bound variables are replaced by %nameless-vars, so
   ;; there is no need to declare bound variables.
   ;; (bvar symbol?)
   (body expression?)
   ;; and the closure contains a nameless environment
   (env nameless-environment?)))



;; nameless-environment? : SchemeVal -> Bool
(define nameless-environment?
  (lambda (x)
    ((list-of expval?) x)))

;; empty-nameless-env : () -> Nameless-env
(define empty-nameless-env
  (lambda ()
    '()))

;; empty-nameless-env? : Nameless-env -> Bool
(define empty-nameless-env?
  (lambda (x)
    (null? x)))

;; extend-nameless-env : ExpVal * Nameless-env -> Nameless-env
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))

;; apply-nameless-env : Nameless-env * Lexaddr -> ExpVal
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))


(define init-nameless-env
  (lambda ()
    (extend-nameless-env
     (num-val 1)                        ; was i
     (extend-nameless-env
      (num-val 5)                       ; was v
      (extend-nameless-env
       (num-val 10)                     ; was x
       (empty-nameless-env))))))

;;new stuff
(define cond-val
  (lambda (conds acts nameless-env)
    (cond ((null? conds)
	   (error 'cond-val "No conditions got into #t"))
	  ((expval->bool (value-of (car conds) nameless-env))
	   (value-of (car acts) nameless-env))
	  (else
	   (cond-val (cdr conds) (cdr acts) nameless-env)))))

(define value-of-translation
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-nameless-env))))))

;; value-of-translation : Nameless-program -> ExpVal
;; Page: 100
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-nameless-env))))))

(define value-of
  (lambda (exp nameless-env)
    (cases expression exp
           (const-exp (num) (num-val num))

           (diff-exp (exp1 exp2)
                     (let ((val1
                            (expval->num
                             (value-of exp1 nameless-env)))
                           (val2
                            (expval->num
                             (value-of exp2 nameless-env))))
                       (num-val
                        (- val1 val2))))

           (zero?-exp (exp1)
                      (let ((val1 (expval->num (value-of exp1 nameless-env))))
                        (if (zero? val1)
                            (bool-val #t)
                            (bool-val #f))))
	   ;;new stuff
           (less?-exp (exp1 exp2)
                      (let ((val1 (value-of exp1 nameless-env))
                            (val2 (value-of exp2 nameless-env)))
                        (let ((num1 (expval->num val1))
                              (num2 (expval->num val2)))
                          (bool-val
                           (< num1 num2)))))

           (if-exp (exp0 exp1 exp2)
                   (if (expval->bool (value-of exp0 nameless-env))
                       (value-of exp1 nameless-env)
                       (value-of exp2 nameless-env)))

           (call-exp (rator rand)
                     (let ((proc (expval->proc (value-of rator nameless-env)))
                           (arg (value-of rand nameless-env)))
                       (apply-procedure proc arg)))

	   ;;new stuff
           (cond-exp (conds acts)
                     (cond-val conds acts nameless-env))

           (nameless-var-exp (n)
                             (apply-nameless-env nameless-env n))

           (nameless-let-exp (exp1 body)
                             (let ((val (value-of exp1 nameless-env)))
                               (value-of body
                                         (extend-nameless-env val nameless-env))))

           (nameless-proc-exp (body)
                              (proc-val
                               (procedure body nameless-env)))

           (else
            (error 'value-of
                   "Illegal expression in translated code: ~s" exp))

           )))


;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
           (procedure (body saved-env)
                      (value-of body (extend-nameless-env arg saved-env))))))

(define run
  (lambda (string)
    (value-of-translation
     (translation-of-program
      (scan&parse string)))))


(run "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)")

(run "let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
            in let
            t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
             in let times4 = (fix t4m)
         in (times4 3)")


(run "less?(2, 4)")
(run "cond less?(1, 2) ==> 2 end")
;;(run "cond less?(2, 1) ==> 1 greater?(2, 2) ==> 2  greater?(3, 2) ==> 3 end")
