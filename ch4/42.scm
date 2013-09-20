(load-relative "../libs/init.scm")
(load-relative "./base/store.scm")
(load-relative "./base/test.scm")
(load-relative "./base/pair-v1.scm")
(load-relative "./base/callneed-cases.scm")

;; still some problem for two cases
;; see new stuff

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
      (arbno identifier "(" identifier ")" "=" expression)
      "in" expression)
     letrec-exp)

    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)

    (expression
     ("set" identifier "=" expression)
     assign-exp)

    (expression
     ("newpair" "(" expression "," expression ")")
     newpair-exp)

    (expression
     ("left" "(" expression ")")
     left-exp)

    (expression
     ("setleft" expression "=" expression)
     setleft-exp)

    (expression
     ("right" "(" expression ")")
     right-exp)

    (expression
     ("setright" expression "=" expression)
     setright-exp)

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
  (ref-val
   (ref reference?))
  (mutpair-val
   (p mutpair?))
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

(define expval->mutpair
  (lambda (v)
    (cases expval v
           (mutpair-val (ref) ref)
           (else (expval-extractor-error 'mutable-pair v)))))

(define expval-extractor-error
  (lambda (variant value)
    (error 'expval-extractors "Looking for a ~s, found ~s"
           variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;
(define-datatype proc proc?
  (procedure
   (bvar symbol?)
   (body expression?)
   (env environment?)))

;;;;;;;;;;;;;;;; environment data structures ;;;;;;;;;;;;;;;;
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval reference?)
   (saved-env environment?))
  (extend-env-rec*
   (proc-names (list-of symbol?))
   (b-vars (list-of symbol?))
   (proc-bodies (list-of expression?))
   (saved-env environment?)))


;;;;;;;;;;;;;;;; thunks ;;;;;;;;;;;;;;;;
(define-datatype thunk thunk?
  (a-thunk
   (exp1 expression?)
   (env environment?)))

(define init-env
  (lambda ()
    (empty-env)))

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
                                   (newref
                                    (proc-val
                                     (procedure
                                      (list-ref b-vars n)
                                      (list-ref p-bodies n)
                                      env)))))
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


;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
           (a-program (body)
                      (value-of body (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 137 and 138
(define value-of
  (lambda (exp env)
    (cases expression exp
           (const-exp (num) (num-val num))

           (var-exp (var)
                    (let ((ref1 (apply-env env var)))
                      (let ((w (deref ref1)))
			(cond ((expval? w) w)
			      (else
			       (let ((v1 (value-of-thunk w)))
				 (begin (setref! ref1 v1)
					v1)))))))

           (diff-exp (exp1 exp2)
                     (let ((val1
                            (expval->num
                             (value-of exp1 env)))
                           (val2
                            (expval->num
                             (value-of exp2 env))))
                       (num-val
                        (- val1 val2))))

           (zero?-exp (exp1)
                      (let ((val1 (expval->num (value-of exp1 env))))
                        (if (zero? val1)
                            (bool-val #t)
                            (bool-val #f))))

           (if-exp (exp0 exp1 exp2)
                   (if (expval->bool (value-of exp0 env))
                       (value-of exp1 env)
                       (value-of exp2 env)))

	   ;;new stuff
	   (let-exp (var exp body)
                    (let ((val (newref (a-thunk exp env))))
                      (value-of body
                                (extend-env var val env))))

           (proc-exp (var body)
                     (proc-val
                      (procedure var body env)))

           (call-exp (rator rand)
                     (let ((proc (expval->proc (value-of rator env)))
                           (arg  (value-of-operand rand env)))
                       (apply-procedure proc arg)))

           (letrec-exp (p-names b-vars p-bodies letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec* p-names b-vars p-bodies env)))

           (begin-exp (exp1 exps)
                      (letrec
                          ((value-of-begins
                            (lambda (e1 es)
                              (let ((v1 (value-of e1 env)))
                                (if (null? es)
                                    v1
                                    (value-of-begins (car es) (cdr es)))))))
                        (value-of-begins exp1 exps)))

           (assign-exp (x e)
                       (begin
                         (setref!
                          (apply-env env x)
                          (value-of e env))
                         (num-val 27)))

           (newpair-exp (exp1 exp2)
                        (let ((v1 (value-of exp1 env))
                              (v2 (value-of exp2 env)))
                          (mutpair-val (make-pair v1 v2))))

           (left-exp (exp1)
                     (let ((v1 (value-of exp1 env)))
                       (let ((p1 (expval->mutpair v1)))
                         (left p1))))

           (setleft-exp (exp1 exp2)
                        (let ((v1 (value-of exp1 env))
                              (v2 (value-of exp2 env)))
                          (let ((p (expval->mutpair v1)))
                            (begin
                              (setleft p v2)
                              (num-val 82)))))

           (right-exp (exp1)
                      (let ((v1 (value-of exp1 env)))
                        (let ((p1 (expval->mutpair v1)))
                          (right p1))))

           (setright-exp (exp1 exp2)
                         (let ((v1 (value-of exp1 env))
                               (v2 (value-of exp2 env)))
                           (let ((p (expval->mutpair v1)))
                             (begin
                               (setright p v2)
                               (num-val 83)))))

           )))

;; apply-procedure : Proc * Ref -> ExpVal
(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
           (procedure (var body saved-env)
                      (value-of body
                                (extend-env var arg saved-env))))))

(define value-of-operand
  (lambda (exp env)
    (cases expression exp
           (var-exp (var) (apply-env env var)) ; no deref!
           (const-exp (num) (newref (value-of exp env)))
           (proc-exp (var body) (newref (value-of exp env)))
           (else
            (newref (a-thunk exp env))))))

(define value-of-thunk
  (lambda (th)
    (cases thunk th
           (a-thunk (exp1 saved-env)
                    (value-of exp1 saved-env)))))

(define value-of-thunk-let
  (lambda (th env)
    (cases thunk th
           (a-thunk (exp1 saved-env)
                    (value-of exp1 env)))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))


(run "let count = 0 in
          let g = proc(d)
                        let d = set count = -(count,-1)
                        in count
        in begin (g 11);
              (g 22) end")

(run-all)
