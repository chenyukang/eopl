(load-relative "../libs/init.scm")
(load-relative "./base/environments.scm")

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
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("list" "(" (separated-list expression ",") ")" ) list-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    ;;(expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)
    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;;; datatype ;;;
(define-datatype expression expression?
  (var-exp
   (id symbol?))
  (const-exp
   (num number?))
  (zero?-exp
   (expr expression?))
  (if-exp
   (predicate-exp expression?)
   (true-exp expression?)
   (false-exp expression?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (emptylist-exp)
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (body expression?))
  (cdr-exp
   (body expression?))
  (null?-exp
   (body expression?))
  (list-exp
   (args (list-of expression?)))
  (let-exp
   (vars (list-of symbols?))
   (vals (list-of expression?))
   (body expression?))
  (proc-exp
   (var (list-of symbol?))
   (body expression?))
  (call-exp
   (rator expression?)
   (rand (list-of expression?))))


;;; an expressed value is either a number, a boolean or a procval.
(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?))
  (pair-val
   (car expval?)
   (cdr expval?))
  (emptylist-val))


;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure
   (var (list-of symbol?))
   (body expression?)
   (env environment?)))

(define (extend-env* vars vals env)
  (if (null? vars)
      env
      (extend-env* (cdr vars)
                   (cdr vals)
                   (extend-env (car vars) (car vals) env))))

(define expval-extractor-error
  (lambda (variant value)
    (error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))


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

(define expval->pair
  (lambda (v)
    (cases expval v
           (pair-val (car cdr)
                     (cons car cdr))
           (else (expval-extractor-error 'pair v)))))

(define expval-car
  (lambda (v)
    (cases expval v
           (pair-val (car cdr) car)
           (else (expval-extractor-error 'car v)))))

(define expval-cdr
  (lambda (v)
    (cases expval v
           (pair-val (car cdr) cdr)
           (else (expval-extractor-error 'cdr v)))))

(define expval-null?
  (lambda (v)
    (cases expval v
           (emptylist-val () (bool-val #t))
           (else (bool-val #f)))))


(define list-val
  (lambda (args)
    (if (null? args)
        (emptylist-val)
        (pair-val (car args)
                  (list-val (cdr args))))))


;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (v)
    (cases expval v
           (proc-val (proc) proc)
           (else (expval-extractor-error 'proc v)))))


;; apply-procedure : Proc * ExpVal -> ExpVal
;; Page: 79
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
           (procedure (var body saved-env)
                      (value-of body (extend-env* var val saved-env))))))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;
;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp (num) (num-val num))
           (var-exp (var) (apply-env env var))
           (diff-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (val2 (value-of exp2 env)))
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val2)))
                         (num-val
                          (- num1 num2)))))

           (zero?-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
                        (let ((num1 (expval->num val1)))
                          (if (zero? num1)
                              (bool-val #t)
                              (bool-val #f)))))

           (if-exp (exp1 exp2 exp3)
                   (let ((val1 (value-of exp1 env)))
                     (if (expval->bool val1)
                         (value-of exp2 env)
                         (value-of exp3 env))))

           (emptylist-exp ()
                          (emptylist-val))
           (cons-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (val2 (value-of exp2 env)))
                       (pair-val val1 val2)))
           (car-exp (body)
                    (expval-car (value-of body env)))
           (cdr-exp (body)
                    (expval-cdr (value-of body env)))
           (null?-exp (exp)
                      (expval-null? (value-of exp env)))
           (list-exp (args)
                     (list-val (map (apply-elm env) args)))

           (let-exp (var exp1 body)
                    (let ((val1 (value-of exp1 env)))
                      (value-of body
                                (extend-env var val1 env))))

           (proc-exp (vars body)
                     (proc-val (procedure vars body env)))

           (call-exp (rator rands)
                     (let ((proc (expval->proc (value-of rator env)))
                           (arg (map (lambda(x) (value-of x env))
				     rands)))
                       (apply-procedure proc arg)))
           )))


;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(run "-(1, 2)")
(run "proc(x) -(x, 1)")
(run "(proc(x) -(x,1)  30)")
(run "(proc(f)(f 30)  proc(x)-(x,1))")
(run "let x = 3 in -(x,1)")
(run "let f = proc (x) proc (y) -(x, -(0, y)) in ((f 10) 20)")

;; new testcase
(run "(proc(x, y) -(x, y) 10 20)")
