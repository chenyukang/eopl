(load-relative "../libs/init.scm")

;; extend letrec-lang to have mutually recursive procedures,
;; also with multi argments
;; we have a procedure list in every letrec

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
    (expression (identifier) var-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)
    (expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)
    (expression ("letrec" (arbno identifier "(" (separated-list identifier ",") ")" "=" expression )
                 "in" expression) letrec-exp)
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

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;
;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure
   (bvar (list-of symbol?))
   (body expression?)
   (env environment?)))

;; Page: 86
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec
   (id (list-of symbol?))
   (bvar (list-of (list-of symbol?))) ;;new stuff
   (body (list-of expression?))
   (saved-env environment?)))

(define init-env
  (lambda ()
    (empty-env)))


(define (extend-env* vars vals env)
  (if (null? vars)
      env
      (extend-env* (cdr vars)
                   (cdr vals)
                   (extend-env (car vars) (car vals) env))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

(define has-proc?
  (lambda (symbol names vars bodys)
    (if (null? names)
        (list #f)
        (if (eqv? symbol (car names))
            (list #t (car vars) (car bodys))
            (has-proc? symbol (cdr names)
                       (cdr vars)
                       (cdr bodys))))))

(define apply-env
  (lambda (env search-sym)
    (cases environment env
           (empty-env ()
                      (error 'apply-env "No binding for ~s" search-sym))
           (extend-env (var val saved-env)
                       (if (eqv? search-sym var)
                           val
                           (apply-env saved-env search-sym)))
           (extend-env-rec (p-names b-vars b-bodys saved-env)
                           (if (null? p-names)
                               (apply-env saved-env search-sym)
                               (let ((res (has-proc? search-sym p-names b-vars b-bodys)))
                                 (if (car res)
                                     (proc-val (procedure (cadr res) (caddr res) env))
                                     (apply-env saved-env search-sym))))))))

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

           (let-exp (var exp1 body)
                    (let ((val1 (value-of exp1 env)))
                      (value-of body
                                (extend-env var val1 env))))

           (proc-exp (var body)
                     (proc-val (procedure var body env)))

           (call-exp (rator rands)
                     (let ((proc (expval->proc (value-of rator env)))
                           (args (map (lambda(x) (value-of x env))
                                      rands)))
                       (apply-procedure proc args)))

	   (letrec-exp (p-names b-vars p-bodys letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec p-names b-vars p-bodys env)))

           )))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
           (procedure (var body saved-env)
                      (value-of body (extend-env* var arg saved-env))))))


(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(scan&parse "-(1, 2)")
(run "-(1, 2)")

(run "letrec double(x)
            = if zero?(x) then 0 else -((double -(x,1)), -2)
       in (double 6)")


(run "letrec
        even(x) = if zero?(x) then 1 else (odd -(x,1))
        odd(x) = if zero?(x) then 0 else (even -(x,1))
      in (odd 12)")

;; new testcase
(run "letrec
      one(x, y) = if zero?(x) then 1 else (two -(x, 1) y)
      two(x, y) = if zero?(y) then 0 else (one x -(y, 1))
       in (two 5 4)")

(run "letrec
      func(x) =
       if zero?(x) then
          1
      else
         -((func -(x, 1)), -(0, x))
      in (func 10)")
