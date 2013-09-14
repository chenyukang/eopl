(load-relative "../libs/init.scm")
(load-relative "./09.scm")
(load-relative "./base/test.scm")
(load-relative "./base/cases.scm")

;; store passing interpreter, this maybe wrong,
;; I haven't got what the apply-store means in the book.

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;
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
     ("newref" "(" expression ")")
     newref-exp)

    (expression
     ("deref" "(" expression ")")
     deref-exp)

    (expression
     ("setref" "(" expression "," expression ")")
     setref-exp)

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))


;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?))
  (ref-val
   (ref reference?))
  )

(define-datatype answer answer?
  (an-answer
   (val expval?)
   (store store?)))

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

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (bvar symbol?)
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
   (b-vars (list-of symbol?))
   (proc-bodies (list-of expression?))
   (saved-env environment?)))

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


;; env->list : Env -> List
;; used for pretty-printing and debugging
(define env->list
  (lambda (env)
    (cases environment env
           (empty-env () '())
           (extend-env (sym val saved-env)
                       (cons
                        (list sym (expval->printable val))
                        (env->list saved-env)))
           (extend-env-rec* (p-names b-vars p-bodies saved-env)
                            (cons
                             (list 'letrec p-names '...)
                             (env->list saved-env))))))

;; expval->printable : ExpVal -> List
;; up with env->list
(define expval->printable
  (lambda (val)
    (cases expval val
           (proc-val (p)
                     (cases proc p
                            (procedure (var body saved-env)
                                       (list 'procedure var '... (env->list saved-env)))))
           (else val))))



(define instrument-let (make-parameter #f))

;; value-of-program : Program -> ExpVal
;; Page: 110
(define value-of-program
  (lambda (pgm)
    (initialize-store!)               ; new for explicit refs.
    (cases program pgm
           (a-program (exp1)
                      (cases answer (value-of exp1 (init-env) the-store)
			     (an-answer (val new-store)
				     val))))))

(define list-val
  (lambda (args)
    (if (null? args)
        (emptylist-val)
        (pair-val (car args)
                  (list-val (cdr args))))))

;; used as map for the list
(define apply-elm
  (lambda (env)
    (lambda (elem)
      (value-of elem env))))

;; value-of : Exp * Env -> ExpVal
;; Page: 113
(define value-of
  (lambda (exp env store)
    (cases expression exp
           (const-exp (num)
		      (an-answer (num-val num) store))
           (var-exp (var)
		    (an-answer (apply-env env var)
			    store))

	   (diff-exp (exp1 exp2)
		     (cases answer (value-of exp1 env store)
			    (an-answer (val1 new-store)
				       (let ((val2 (value-of exp2 env store)))
					 (cases answer val2
						(an-answer (val2 new-store)
							   (let ((v1 (expval->num val1))
								 (v2 (expval->num val2)))
							     (an-answer
							      (num-val (- v1 v2))
							      new-store))))))))

           (zero?-exp (exp1)
		      (cases answer (value-of exp1 env store)
			     (an-answer (val new-store)
					(if (zero? (expval->num val))
					    (an-answer (bool-val #t) new-store)
					    (an-answer (bool-val #f) new-store)))))
           (if-exp (exp1 exp2 exp3)
		   (cases answer (value-of exp1 env store)
			  (an-answer (val new-store)
				     (if (expval->bool val)
					 (value-of exp2 env new-store)
					 (value-of exp3 env new-store)))))

           (let-exp (var exp1 body)
		    (cases answer (value-of exp1 env store)
			   (an-answer (val new-store)
				      (value-of body
						(extend-env var val env)
						new-store))))
           (proc-exp (var body)
                     (an-answer (proc-val (procedure var body env))
				store))

           (call-exp (rator rand)
		     (cases answer (value-of rator env store)
			    (an-answer (proc-exp new-store)
				       (cases answer (value-of rand env store)
					      (an-answer (rands new-store)
							 (let ((proc (expval->proc proc-exp))
							       (args rands))
							   (apply-procedure proc args store)))))))

           (letrec-exp (p-names b-vars p-bodies letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec* p-names b-vars p-bodies env) store))

           (begin-exp (exp1 exps)
                      (letrec
                          ((value-of-begins
                            (lambda (e1 es store)
                              (let ((v1 (value-of e1 env store)))
                                (if (null? es)
                                    v1
                                    (value-of-begins (car es) (cdr es) store))))))
                        (value-of-begins exp1 exps store)))

           (newref-exp (exp1)
		       (cases answer (value-of exp1 env store)
			      (an-answer (val new-store)
					 (an-answer (ref-val (newref val))
						    new-store))))

           (deref-exp (exp1)
		      (cases answer (value-of exp1 env store)
			     (an-answer (v1 new-store)
					(let ((ref1 (expval->ref v1)))
					  (an-answer (deref ref1) new-store)))))

	   (setref-exp (exp1 exp2)
		       (cases answer (value-of exp1 env store)
			      (an-answer (v1 new-store)
					 (cases answer (value-of exp2 env store)
						(an-answer (v2 new-store)
							   (let ((ref1 (expval->ref v1)))
							     (begin
							       (setref! ref1 v2)
							       (an-answer (num-val 1) new-store))))))))
           )))

;;
;; instrumented version
(define apply-procedure
  (lambda (proc1 arg store)
    (cases proc proc1
           (procedure (var body saved-env)
                      (let ((r arg))
                        (let ((new-env (extend-env var r saved-env)))
                          (if (instrument-let)
                              (begin
                                (printf
                                 "entering body of proc ~s with env =~%"
                                 var)
                                (pretty-print (env->list new-env))
                                (printf "store =~%")
                                (pretty-print (store->readable (get-store-as-list)))
                                (printf "~%")))
                          (value-of body new-env store)))))))


;; store->readable : Listof(List(Ref,Expval))
(define store->readable
  (lambda (l)
    (map
     (lambda (p)
       (cons
        (car p)
        (expval->printable (cadr p))))
     l)))


(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(run "-(1, 2)")
(run "begin 1; 2; 3 end")
(run-all)
