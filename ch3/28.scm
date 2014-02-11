(load-relative "../libs/init.scm")
(load-relative "../libs/environments.scm")

;; dynamic binding, use data-structure representaion
;; need let with multi vars and multi vals
;;

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
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression expression ")") call-exp)
    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

;;; datetype ;;;
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
  (let-exp
   (vars (list-of symbols?))
   (vals (list-of expression?))
   (body expression?))
  (proc-exp
   (var identifier?)
   (body expression?))
  (call-exp
   (rator expression?)
   (rand expression?)))


;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure
   (var symbol?)
   (body expression?)))

;;; an expressed value is either a number, a boolean or a procval.
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

(define value-of-vals
  (lambda (vals env)
    (if (null? vals)
        '()
        (cons (value-of (car vals) env)
              (value-of-vals (cdr vals) env)))))

(define extend-env-list
  (lambda (vars vals env)
    (if (null? vars)
        env
        (let ((var1 (car vars))
              (val1 (car vals)))
          (extend-env-list (cdr vars) (cdr vals) (extend-env var1 val1 env))))))

;; apply-procedure : Proc * ExpVal -> ExpVal
;; Page: 79
(define apply-procedure
  (lambda (proc1 val env)
    (cases proc proc1
           (procedure (var body)
                      (value-of body (extend-env var val env))))))

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

           (let-exp (vars vals body)
                    (let ((_vals (value-of-vals vals env)))
                      (value-of body (extend-env-list vars _vals env))))

           (proc-exp (var body)
                     (proc-val (procedure var body)))

           (call-exp (rator rand)
                     (let ((proc (expval->proc (value-of rator env)))
                           (arg (value-of rand env)))
                       (apply-procedure proc arg env)))
           )))


;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(run "proc(x) -(x, 1)")
(run "(proc(x) -(x,1)  30)")
(run "let f = proc (x) -(x,1) in (f 30)")
(run "(proc(f)(f 30)  proc(x)-(x,1))")
(run "let x = 3 in -(x,1)")

(run "let a = 3
      in let p = proc (x) -(x,a)
             a = 5
      in -(a,(p 2))")

;; (num-val 8)
