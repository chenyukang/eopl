(load-relative "../libs/init.scm")
(load-relative "./base/exception-test.scm")
(load-relative "./base/exception-cases.scm")

;; add multi-arg into interpreter.
;; and add raising error when procedure number is not required number

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
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    ;;new stuff
    (expression ("proc" "(" (separated-list identifier ",") ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)

    (expression
     ("letrec"
      identifier "(" (arbno identifier) ")" "=" expression
      "in" expression)
     letrec-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    ;; Lists.  We will have lists of literal numbers only.

    (expression
     ("list" "(" (separated-list number ",") ")")
     const-list-exp)

    (expression
     (unary-op "(" expression ")")
     unop-exp)

    (expression
     ("try" expression "catch" "(" identifier ")" expression)
     try-exp)

    (expression
     ("raise" expression)
     raise-exp)

    (unary-op ("null?") null?-unop)
    (unary-op ("car")   car-unop)
    (unary-op ("cdr" )  cdr-unop)
    (unary-op ("zero?") zero?-unop)

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
   (proc proc?))
  (list-val
   (lst (list-of expval?))))

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

(define expval->list
  (lambda (v)
    (cases expval v
           (list-val (lst) lst)
           (else (expval-extractor-error 'list v)))))

(define expval-extractor-error
  (lambda (variant value)
    (error 'expval-extractors "Looking for a ~s, found ~s"
	   variant value)))

;; ;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;
(define-datatype proc proc?
  (procedure
   (bvar (list-of symbol?))
   (body expression?)
   (env environment?)))

(define-datatype continuation continuation?
  (end-cont)
  (diff1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (diff2-cont
   (val1 expval?)
   (cont continuation?))
  (unop-arg-cont
   (unop unary-op?)
   (cont continuation?))
  (let-exp-cont
   (var identifier?)
   (body expression?)
   (saved-env environment?)
   (saved-cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (env environment?)
   (cont continuation?))
  (rator-cont
   (rands (list-of expression?))
   (saved-vals (list-of expression?))
   (saved-env environment?)
   (saved-cont continuation?))
  (rand-cont
   (val1 expval?)
   (rands (list-of expression?))
   (saved-vals (list-of expval?))
   (saved-env environment?)
   (saved-cont continuation?))
  (try-cont
   (var symbol?)
   (handler-exp expression?)
   (env environment?)
   (cont continuation?))
  (raise1-cont
   (saved-cont continuation?))
  )

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

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;
;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (body)
                      (value-of/k body (init-env) (end-cont))))))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page: 173
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp

           (const-exp (num) (apply-cont cont (num-val num)))

           (const-list-exp (nums)
                           (apply-cont cont
                                       (list-val (map num-val nums))))

           (var-exp (var) (apply-cont cont (apply-env env var)))

           (diff-exp (exp1 exp2)
                     (value-of/k exp1 env
                                 (diff1-cont exp2 env cont)))

           (unop-exp (unop exp1)
                     (value-of/k exp1 env
                                 (unop-arg-cont unop cont)))

           (if-exp (exp1 exp2 exp3)
                   (value-of/k exp1 env
                               (if-test-cont exp2 exp3 env cont)))

           ;;new stuff
           (proc-exp (vars body)
                     (apply-cont cont
                                 (proc-val (procedure vars body env))))
           (letrec-exp (p-name b-vars p-body letrec-body)
                       (value-of/k letrec-body
                                   (extend-env-rec p-name b-vars p-body env)
                                   cont))

           (call-exp (rator rands)
                     (value-of/k rator env
                                 (rator-cont rands '() env cont)))

	   (let-exp (var exp1 body)
		    (value-of/k exp1 env
				(let-exp-cont var body env cont)))

           (try-exp (exp1 var handler-exp)
                    (value-of/k exp1 env
                                (try-cont var handler-exp env cont)))

           (raise-exp (exp1)
                      (value-of/k exp1 env
                                  (raise1-cont cont))))))


;; (define proc-args
;;   (lambda (proc)
;;     (if (equal? (car proc) 'procedure)
;; 	(let (args (c
;;     (cases expval v
;; 	   (proc-val (

;; apply-cont : continuation * expval -> final-expval
(define apply-cont
  (lambda (cont val)
    (cases continuation cont
           (end-cont () val)
           (diff1-cont (exp2 saved-env saved-cont)
                       (value-of/k exp2 saved-env (diff2-cont val saved-cont)))
           (diff2-cont (val1 saved-cont)
                       (let ((n1 (expval->num val1))
                             (n2 (expval->num val)))
                         (apply-cont saved-cont
                                     (num-val (- n1 n2)))))
           (unop-arg-cont (unop cont)
                          (apply-cont cont
                                      (apply-unop unop val)))

           (let-exp-cont (var body saved-env saved-cont)
                         (value-of/k body
                                     (extend-env var val saved-env) saved-cont))

           (if-test-cont (exp2 exp3 env cont)
                         (if (expval->bool val)
                             (value-of/k exp2 env cont)
                             (value-of/k exp3 env cont)))
           ;;new stuff
           (rator-cont (rands saved-vals saved-env saved-cont)
                       (value-of/k (car rands) saved-env
                                   (rand-cont val (cdr rands) saved-vals saved-env saved-cont)))

	   ;;add procedure arguments number checking here!!
           (rand-cont (proc-val rands saved-vals saved-env saved-cont)
                      (if (null? rands)
			  (let ((proc (expval->proc proc-val)))
			    (let ((new-vals (append saved-vals (list val)))
				  (require-vals (cadr proc)))
			      (if (equal? (length require-vals) (length new-vals))
				  (apply-procedure/k proc new-vals saved-cont)
				  (begin
				    (printf "requared args :~a given:~a\n" require-vals new-vals)
				  ;;throw a exception here!!!
				    (apply-cont (raise1-cont saved-cont) (list-val saved-vals))))))
                          (value-of/k (car rands) saved-env
                                      (rand-cont proc-val (cdr rands)
                                                 (append saved-vals (list val)) saved-env saved-cont))))

           ;; the body of the try finished normally-- don't evaluate the handler
           (try-cont (var handler-exp saved-env saved-cont)
                     (apply-cont saved-cont val))

           ;; val is the value of the argument to raise
           (raise1-cont (saved-cont)
                        ;; we put the short argument first to make the trace more readable.
                        (apply-handler val saved-cont))
           )))

;; apply-handler : ExpVal * Cont -> FinalAnswer
(define apply-handler
  (lambda (val cont)
    (cases continuation cont
           ;; interesting cases
           (try-cont (var handler-exp saved-env saved-cont)
                     (value-of/k handler-exp
                                 (extend-env var val saved-env)
                                 saved-cont))

           (end-cont () (error 'apply-handler "uncaught exception!"))

           ;; otherwise, just look for the handler...
           (diff1-cont (exp2 saved-env saved-cont)
                       (apply-handler val saved-cont))

           (diff2-cont (val1 saved-cont)
                       (apply-handler val saved-cont))

           (if-test-cont (exp2 exp3 env saved-cont)
                         (apply-handler val saved-cont))

           (unop-arg-cont (unop saved-cont)
                          (apply-handler val saved-cont))

           (rator-cont (rand vals saved-env saved-cont)
                       (apply-handler val saved-cont))

           (rand-cont (val1 rands vals saved-env saved-cont)
                      (apply-handler val saved-cont))

	   (let-exp-cont (var1 body saved-env saved-cont)
			 (apply-handler var saved-cont))

           (raise1-cont (cont)
                        (apply-handler val cont))
           )))


;;new stuff
(define extend-env*
  (lambda (vars vals saved-env)
    (if (null? vars)
        saved-env
        (extend-env* (cdr vars) (cdr vals)
                     (extend-env (car vars) (car vals) saved-env)))))

;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
(define apply-procedure/k
  (lambda (proc1 args cont)
    (cases proc proc1
           (procedure (vars body saved-env)
		      (value-of/k body
				  (extend-env* vars args saved-env)
				  cont)))))

(define apply-unop
  (lambda (unop val)
    (cases unary-op unop
           (null?-unop ()
                       (bool-val
                        (null? (expval->list val))))
           (car-unop ()
                     (car (expval->list val)))
           (cdr-unop ()
                     (list-val (cdr (expval->list val))))
           (zero?-unop ()
                       (bool-val
                        (zero? (expval->num val)))))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(add-test! '(multi-arg  "(proc(x,y) -(x, y) 10 20)" -10))
(add-test! '(multi-arg-letrec "letrec f(x y) = -(x, y) in (f 11 33)" -22))
(add-test! '(error-args "try (proc(x) -(x,1)  30 40) catch(m) m" (30)))

(run-all)
