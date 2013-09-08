(load-relative "../libs/init.scm")
(load-relative "../libs/environments.scm")

;; a free-variables utility to get all free variables in exp
;; optimize-env to remove all the bounded variables
;; I doubt this optimizion's performance, since it need to iterator the env
;; in every proc definition

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
   (body expression?)
   (env environment?)))

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
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

;; apply-procedure : Proc * ExpVal -> ExpVal
;; Page: 79
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
           (procedure (var body saved-env)
                      (value-of body (extend-env var val saved-env))))))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;
;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-env))))))

(define free-variables
  (lambda (expr bound)
    (cases expression expr
           (const-exp (num) '())
           (var-exp (var)
                    (if (memq var bound)
                        '()
                        (list var)))
           (diff-exp (exp1 exp2)
                     (append (free-variables exp1 bound)
                             (free-variables exp2 bound)))
           (zero?-exp (arg)
                      (free-variables arg bound))
           (if-exp (pred consq alte)
                   (append (free-variables pred bound)
                           (free-variables consq bound)
                           (free-variables alte bound)))
           (let-exp (var value body)
                    (append (free-variables value bound)
                            (free-variables body (cons var bound))))
           (proc-exp (var body)
                     (append (free-variables body (cons var bound))))
           (call-exp (var body)
                     (append (free-variables var bound)
                             (free-variables body bound))))))


(define optimize-env
  (lambda (env frees)
    (let ((pred (lambda(var)
		  (if (or (empty-env-record? var)
			  (memq (car var) frees))
		      #t
		      #f))))
      (filter pred env))))

;;(define a (cons (list 'a '1) (empty-env-record)))
;;(define a (cons (list 'b '2) a))
;;(define a (cons (list 'c '3) a))
;;(optimize-env a '(a b))


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
	   ;;new stuff
           (proc-exp (var body)
		     (let ((frees (free-variables body '())))
		       (proc-val (procedure var body
					    (optimize-env env frees)))))

           (call-exp (rator rand)
                     (let ((proc (expval->proc (value-of rator env)))
                           (arg (value-of rand env)))
                       (apply-procedure proc arg)))
           )))

;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;; new stuff : to testing free-variables
(define run-free
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (free-variables exp1 '())))))

(define run-free-v
  (lambda (prog)
    (run-free (scan&parse prog))))

(cadr (scan&parse "proc(x) -(x, 1)"))
(run-free-v "proc(x) -(x, y)")
;;-> (y)

(run-free-v "proc(x) -(x, 1)")
(run-free-v "(proc(x) -(x,1)  30)")
(run-free-v "let f = proc (x) -(x,1) in (f 30)")
(run-free-v "(proc(f)(f 30)  proc(x)-(x,1))")
(run-free-v "let makerec = proc (f)
        let d = proc (x)
          proc (z) ((f (x x)) z)
        in proc (n) ((f (d d)) n)
     in let maketimes4 = proc (f) proc (x)
          if zero?(x)
             then 0
          else -((f -(x,1)), z)
      in let times4 = (makerec maketimes4) in (times4 z)")
;; ==> (z z)

(run "proc(x) -(x, 1)")
(run "(proc(x) -(x,1)  30)")
(run "let f = proc (x) -(x,1) in (f 30)")
(run "(proc(f)(f 30)  proc(x)-(x,1))")
