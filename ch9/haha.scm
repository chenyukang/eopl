
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
     ("=" "(" expression "," expression ")")
     equal-exp)

    (expression
     ("<" "(" expression "," expression ")")
     less-exp)

    (expression
     (">" "(" expression "," expression ")")
     large-exp)

    (expression
     ("-" "(" (separated-list expression ",") ")")
     diff-exp)

    (expression
     ("+" "(" (separated-list expression ",") ")")
     sum-exp)

    (expression
     ("*" "(" (separated-list expression ",") ")")
     mul-exp)

    (expression
     ("/" "(" (separated-list expression ",") ")")
     div-exp)

    (expression
     ("(set" expression expression ")")
     set-exp)

    (expression
     ("if" expression "then" expression "else" expression "end")
     if-exp)

    (expression
     ("begin" expression ";" (arbno expression ";") "end")
     begin-exp)

    (expression
     ("let" (arbno identifier "=" expression) "in" expression "end")
     let-exp)

    (expression (identifier) var-exp)

    (expression
     ("proc" "(" (arbno identifier) ")" expression "end")
     proc-exp)

    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    (expression
     ("letrec"
      (arbno identifier "=" expression)
      "in" expression "end")
     letrec-exp)


    ;; (obj-expression
    ;;  ("extend" expression "with" (arbno member-decl))
    ;;  extend-exp)

    ;; (member-decl
    ;;  ("public" identifier "=" expression ";")
    ;;  a-member-public-decl)

    ;; (member-decl
    ;;  ("protected" identifier "=" expression ";")
    ;;  a-member-protect-decl)

    ;; (obj-expression
    ;;  ("super")
    ;;  super-exp)

    ;; (obj-expression
    ;;  ("self")
    ;;  self-exp)
    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-specq the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; syntactic operations on types ;;;;;;;;;;;;;;;;
;;; datetype ;;;
(define-datatype expression expression?
  (var-exp
   (var identifier?))
  (const-exp
   (num number?))
  (set-exp
   (arg1 expression?)
   (arg2 expression?))
  (if-exp
   (predicate-exp expression?)
   (test-true-exp expression?)
   (teset-false-exp expression?))
  (diff-exp
   (args (list-of expression?)))
  (sum-exp
   (args (list-of expression?)))
  (mul-exp
   (args (list-of expression?)))
  (div-exp
   (args (list-of expression?)))
  (equal-exp
   (arg1 expression?)
   (arg2 expression?))
  (large-exp
   (arg1 expression?)
   (arg2 expression?))
  (less-exp
   (arg1 expression?)
   (arg2 expression?))
  (begin-exp
   (first expression?)
   (rest (list-of expression?)))
  (proc-exp
   (vars (list-of identifier?))
   (body expression?))
  (let-exp
   (vars (list-of identifier?))
   (vals (list-of expression?))
   (exp expression?))
  (letrec-exp
   (vars (list-of identifier?))
   (vals (list-of expression?))
   (exp expression?))
  (call-exp
   (proc expression?)
   (body (list-of expression?))))

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
(define-datatype proc proc?
  (procedure
   (bvar (list-of symbol?))
   (body expression?)
   (env environment?)))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec*
   (proc-names (list-of symbol?))
   (b-vars (list-of (list-of symbol?)))
   (proc-bodies (list-of expression?))
   (saved-env environment?)))

(define init-env
  (lambda ()
    (extend-env
     'true (bool-val #t)
     (extend-env
      'false (bool-val #f)
      (empty-env)))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;
(define apply-env
  (lambda (env search-sym)
    (cases environment env
           (empty-env ()
                      (error 'apply-env "No binding for ~s" search-sym))
           (extend-env (bvar bval saved-env)
                       (if (eqv? search-sym bvar)
                           bval
                           (apply-env saved-env search-sym)))
           (extend-env-rec* (p-names b-vars p-bodies saved-env)
                            (cond
                             ((location search-sym p-names)
                              => (lambda (n)
                                   (proc-val
                                    (procedure
                                     (list-ref b-vars n)
                                     (list-ref p-bodies n)
                                     env))))
                             (else (apply-env saved-env search-sym)))))))


(define location
  (lambda (sym syms)
    (cond
     ((null? syms) #f)
     ((eqv? sym (car syms)) 0)
     ((location sym (cdr syms))
      => (lambda (n)
           (+ n 1)))
     (else #f))))

;; new stuff
(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
        env
        (extend-env (car vars) (car vals)
                    (extend-env* (cdr vars) (cdr vals) env)))))


;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
           (procedure (vars body saved-env)
                      (let ((new-env (extend-env* vars args saved-env)))
                        (value-of body new-env))))))

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-env))))))

(define args-val
  (lambda (vals env)
    (let ((arg-vals
           (map (lambda(x) (value-of x env)) vals)))
      (map (lambda(x) (expval->num x)) arg-vals))))

(define value-of-ops-iter
  (lambda (op now vals)
    (if (null? vals)
        now
        (value-of-ops-iter op (op now (car vals))
                           (cdr vals)))))

(define value-of-ops
  (lambda (op vals)
    (value-of-ops-iter op (car vals) (cdr vals))))

(define vars-of-exps
  (lambda (exps)
    (map (lambda (x)
           (cases expression x
                  (proc-exp (pvars body)
                            pvars)
                  (else error 'letrec-exp "failed")))
         exps)))

(define bodies-of-exps
  (lambda (exps)
    (map (lambda (x)
           (cases expression x
                  (proc-exp (pvars body)
                            body)
                  (else error 'letrec-exp "failed")))
         exps)))

(define is-set-exp?
  (lambda (exp)
    (cases expression exp
           (set-exp (e1 e2)
                    #t)
           (else #f))))

(define eval-set
  (lambda (set-exp env)
    (cases expression set-exp
           (set-exp (e1 e2)
                    (let ((value (value-of e2 env))
                          (var (value-of e1 env)))
                      (extend-env var value env)))
           (else error 'eval-set "failed"))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (begin
      (printf "now: ~s~%" exp)
      (cases expression exp
             (const-exp (num)
                        (begin
                          (printf "xx: ~s~%" num)
                          (num-val num)))

             (var-exp (var)
                      (let ((res (apply-env env var)))
                        (begin
                          (printf "val: ~s~%" res)
                          res)))

             (equal-exp (exp1 exp2)
                        (let ((val1 (value-of exp1 env))
                              (val2 (value-of exp2 env)))
                          (let ((num1 (expval->num val1))
                                (num2 (expval->num val2)))
                            (bool-val
                             (= num1 num2)))))

             (less-exp (exp1 exp2)
                       (let ((val1 (value-of exp1 env))
                             (val2 (value-of exp2 env)))
                         (let ((num1 (expval->num val1))
                               (num2 (expval->num val2)))
                           (bool-val
                            (< num1 num2)))))
             (large-exp (exp1 exp2)
                        (let ((val1 (value-of exp1 env))
                              (val2 (value-of exp2 env)))
                          (let ((num1 (expval->num val1))
                                (num2 (expval->num val2)))
                            (bool-val
                             (> num1 num2)))))

             (set-exp (arg1 arg2)
                      (bool-val #t))

             ;; (obj-call-exp (args)
             ;;               (bool-val #t))

             (diff-exp (args)
                       (let ((arg-vals (args-val args env)))
                         (num-val (value-of-ops - arg-vals))))

             (sum-exp (args)
                      (let ((arg-vals (args-val args env)))
                        (num-val (value-of-ops + arg-vals))))

             (mul-exp (args)
                      (let ((arg-vals (args-val args env)))
                        (num-val (value-of-ops * arg-vals))))

             (div-exp (args)
                      (let ((arg-vals (args-val args env)))
                        (num-val (value-of-ops / arg-vals))))

             (if-exp (exp1 exp2 exp3)
                     (let ((val1 (value-of exp1 env)))
                       (if (expval->bool val1)
                           (value-of exp2 env)
                           (value-of exp3 env))))

             (begin-exp (first rest)
                        (if (is-set-exp? first)
                            (value-of (begin-exp (car rest) (cdr rest))
                                      (eval-set first env))
                            (let ((first-val (value-of first env)))
                              (if (null? rest)
                                  first-val
                                  (value-of (begin-exp (car rest) (cdr rest))
                                            env)))))
             (proc-exp (vars body)
                       (proc-val (procedure vars body env)))

             (call-exp (rator rands)
                       (let ((proc (expval->proc (value-of rator env)))
                             (args (map (lambda(x)
                                          (value-of x env)) rands)))
                         (apply-procedure proc args)))

             (let-exp (vars exps body)
                      (value-of body
                                (extend-env* vars (map (lambda(x)
                                                         (value-of x env))
                                                       exps) env)))
             (letrec-exp (vars bodies letrec-body)
                         (let ((lvars (vars-of-exps bodies))
                               (lbodies (bodies-of-exps bodies)))
                           (let ((new-env
                                  (extend-env-rec*
                                   vars
                                   lvars
                                   lbodies env)))
                             (value-of letrec-body new-env))))

             ))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))


(run "1")
