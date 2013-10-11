(load-relative "../libs/init.scm")
(load-relative "./base/store.scm")
(load-relative "./base/test.scm")
(load-relative "./base/implicit-cases.scm")

;; Add do-while statement for the Lang
;; based on 23.scm

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
  '((program (statement) a-program)

    (expression (number) const-exp)
    (expression (identifier) var-exp)

    (statement
     ("{" (separated-list statement ";") "}")
     block-stat)

    (expression
     ("-" "(" expression "," expression ")") diff-exp)
    (expression
     ("+" "(" expression "," expression ")") add-exp)
    (expression
     ("*" "(" expression "," expression ")") multi-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("not" "(" expression ")")
     not-exp)

    (statement
     ("if" expression "then" statement "else" statement)
     if-stat)

    (statement
     ("while" expression statement)
     while-stat)

    ;;new stuff
    (statement
     ("do" statement "while" expression)
     do-while-stat)

    (expression
     ("proc" "(" (separated-list identifier ",") ")" expression)
     proc-exp)

    (statement
     ("print" expression)
     print-stat)

    (statement
     ("read" identifier)
     read-stat)

    (statement
     ("var" (separated-list identifier ",") ";" statement)
     declear-stat)

    (statement
     (identifier "=" expression)
     set-stat)

    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    ))

;;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define-datatype proc proc?
  (procedure
   (bvar (list-of symbol?))
   (body expression?)
   (env environment?)))

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (ref-val
   (ref reference?))
  (proc-val
   (proc proc?))
  )

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

(define expval->ref
  (lambda (v)
    (cases expval v
           (ref-val (ref) ref)
           (else (expval-extractor-error 'reference v)))))

(define expval-extractor-error
  (lambda (variant value)
    (error 'expval-extractors "Looking for a ~s, found ~s"
           variant value)))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval reference?)                 ; new for implicit-refs
   (saved-env environment?)))

(define init-env
  (lambda ()
    (extend-env
     'i (newref (num-val 1))
     (extend-env
      'v (newref (num-val 5))
      (extend-env
       'x (newref (num-val 10))
       (empty-env))))))

(define location
  (lambda (sym syms)
    (cond
     ((null? syms) #f)
     ((eqv? sym (car syms)) 0)
     ((location sym (cdr syms))
      => (lambda (n)
           (+ n 1)))
     (else #f))))


;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;
(define apply-env
  (lambda (env search-var)
    (cases environment env
           (empty-env ()
                      (error 'apply-env "No binding for ~s" search-var))
           (extend-env (bvar bval saved-env)
                       (if (eqv? search-var bvar)
                           bval
                           (apply-env saved-env search-var))))))

(define extend-unset-env*
  (lambda (vars env)
    (if (null? vars)
	env
	(extend-unset-env* (cdr vars)
			   (extend-env (car vars)
				       (newref (num-val 1))
				       env)))))
(define print-exp
  (lambda (exp)
    (cases expval exp
	  (bool-val (v)
		    (printf "~a\n" v))
	  (num-val (v)
		   (printf "~a\n" v))
	  (proc-val (v)
		    (printf "func: ~a\n" v))
	  (ref-val (v)
		   (error "print-exp trying to print ref\n")))))

(define execute-stat
  (lambda (stat env)
    (cases statement stat
	   (print-stat (exp)
		       (print-exp (value-of exp env)))
	   (declear-stat (vars inner-stat)
			 (execute-stat inner-stat (extend-unset-env* vars env)))

	   (if-stat (test-exp true-stat false-stat)
		    ((let ((val (value-of test-exp env)))
		       (if (expval->bool val)
			   (execute-stat true-stat)
			   (execute-stat false-stat)))))

	   (set-stat (var exp)
		     (setref!
		      (apply-env env var)
		      (value-of exp env)))

	   (block-stat (stats)
		       (execute-stats stats env))

	   (while-stat (exp stat)
		       (execute-while-stat exp stat env))

	   (do-while-stat (stat exp)
			  (begin
			    (execute-stat stat env)
			    (execute-stat (while-stat exp stat) env)))
	   (read-stat (var)
		      (setref!
		       (apply-env env var)
		       (num-val (string->number (read-line))))))))

(define execute-stats
  (lambda (stats env)
    (if (not (null? stats))
	(begin
	  (execute-stat (car stats) env)
	  (execute-stats (cdr stats) env)))))

(define execute-while-stat
  (lambda (exp stat env)
    (let ((val (value-of exp env)))
      (if (expval->bool val)
	  (begin
	    (execute-stat stat env)
	    (execute-while-stat exp stat env))))))

(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
        env
        (extend-env (car vars) (newref (car vals))
                    (extend-env* (cdr vars) (cdr vals) env)))))

(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
           (procedure (vars body saved-env)
                      (let ((new-env (extend-env* vars args saved-env)))
                        (value-of body new-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp (num) (num-val num))
           (var-exp (var) (deref (apply-env env var)))
           (diff-exp (exp1 exp2)
                     (let ((val1 (value-of exp1 env))
                           (val2 (value-of exp2 env)))
                       (let ((num1 (expval->num val1))
                             (num2 (expval->num val2)))
                         (num-val
                          (- num1 num2)))))
	   (add-exp (exp1 exp2)
		    (let ((val1 (value-of exp1 env))
			  (val2 (value-of exp2 env)))
		      (let ((num1 (expval->num val1))
			    (num2 (expval->num val2)))
			(num-val
			 (+ num1 num2)))))

           (multi-exp (exp1 exp2)
                    (let ((val1 (value-of exp1 env))
                          (val2 (value-of exp2 env)))
                      (let ((num1 (expval->num val1))
                            (num2 (expval->num val2)))
                        (num-val
                         (* num1 num2)))))

           (zero?-exp (exp1)
                      (let ((val1 (value-of exp1 env)))
                        (let ((num1 (expval->num val1)))
                          (if (zero? num1)
                              (bool-val #t)
                              (bool-val #f)))))
	   (not-exp (exp)
		    (let ((val (value-of exp env)))
		      (let ((v (expval->bool val)))
			(if v
			    (bool-val #f)
			    (bool-val #t)))))

           (proc-exp (var body)
                     (proc-val (procedure var body env)))

           (call-exp (rator rands)
                     (let ((proc (expval->proc (value-of rator env)))
                           (args (map (lambda(x)
                                        (value-of x env)) rands)))
                       (apply-procedure proc args)))

           )))


(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
           (a-program (stat)
                      (execute-stat stat (init-env))))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define parser
  (lambda (string)
    (scan&parse string)))

(run "var x,y; {x = 3; y = 4; print +(x,y)}")
;; ==> 7

(run "var x,y,z; {x = 3;
                    y = 4;
                    z = 0;
                    while not(zero?(x))
                      {z = +(z,y); x = -(x,1)};
       print z}")
;; ==> 12

(run " var x; {x = 3;
                 print x;
                 var x; {x = 4; print x};
                 print x}")

;; ==> 3 4 3

(run "var f, x; {f = proc(x, y) *(x ,y);
                  x = 3;
                  print (f 4 x)}")

(run "var x; { x = 100;
               do { print x; x = -(x,1) }
                  while not(zero?(x)) }")
;; ==> 12
;;(run "var x; { read x; print x }")
