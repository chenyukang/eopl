(load-relative "../libs/init.scm")
(load-relative "./base/store.scm")
(load-relative "./base/test.scm")
(load-relative "./base/pair-v1.scm")
(load-relative "./base/pair-cases.scm")

;; add arrays to language,
;; based on 29, add length for array

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

    ;; new for mutable-pairs

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

    ;;new stuff
    (expression
     ("newarray"  "("  expression "," expression ")" )
     newarray-exp)
    (expression
     ("arrayref" "(" expression "," expression ")" )
     arrayref-exp)
    (expression
     ("arrayset" "(" expression "," expression "," expression ")" )
     arrayset-exp)
    (expression
     ("arraylength" "(" expression ")" )
     arraylength-exp)
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
  (array-val
   (p array?))
  )

;;new stuff
(define-datatype array array?
  (array-v
   (header reference?)
   (length integer?)))


(define make-array
  (lambda (count value)
    (letrec ((do-alloc
	      (lambda (count)
		(if (> count 0)
		    (let ((new (newref value)))
		      (do-alloc (- count 1)))))))
      (let ((first (newref value)))
	(do-alloc (- count 1))
	(array-v first count)))))

;; index check for array
(define array-chk
  (lambda (arr pos)
    (cases array arr
	   (array-v (header count)
		    (if (>= pos count)
			(error "index out of range for array")
			#t)))))
(define array-at
  (lambda (arr pos)
    (if (array-chk arr pos)
	(cases array arr
	       (array-v (header count)
			(deref (+ header pos)))))))

(define array-set!
  (lambda (arr pos val)
    (if (array-chk arr pos)
	(cases array arr
	       (array-v (header count)
			(setref! (+ header pos) val))))))

(define array-length
  (lambda (arr)
    (cases array arr
	   (array-v (header count)
		    count))))

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

;;new stuff
(define expval->array
  (lambda (v)
    (cases expval v
	   (array-val (ref) ref)
	   (else (expval-extractor-error 'array v)))))

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
   (bval reference?)                 ; new for implicit-refs
   (saved-env environment?))
  (extend-env-rec*
   (proc-names (list-of symbol?))
   (b-vars (list-of symbol?))
   (proc-bodies (list-of expression?))
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

;; env->list : Env -> List
;; used for pretty-printing and debugging
(define env->list
  (lambda (env)
    (cases environment env
           (empty-env () '())
           (extend-env (sym val saved-env)
		       (cons
			(list sym val)              ; val is a denoted value-- a
                                        ; reference.
			(env->list saved-env)))
	   (extend-env-rec* (p-names b-vars p-bodies saved-env)
			    (cons
			     (list 'letrec p-names '...)
			     (env->list saved-env))))))

;; expval->printable : ExpVal -> List
;; returns a value like its argument, except procedures get cleaned
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

;; say (instrument-let #t) to turn instrumentation on.
;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
	   (a-program (body)
		      (value-of body (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 126
(define value-of
  (lambda (exp env)
    (cases expression exp

	   (const-exp (num) (num-val num))

	   (var-exp (var) (deref (apply-env env var)))

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

	   (let-exp (var exp1 body)
		    (if (instrument-let)
			(printf "entering let ~s~%" var))
		    (let ((val (value-of exp1 env)))
		      (let ((new-env (extend-env var (newref val) env)))
			(if (instrument-let)
			    (begin
			      (printf "entering body of let ~s with env =~%" var)
			      (pretty-print (env->list new-env))
			      (printf "store =~%")
			      (pretty-print (store->readable (get-store-as-list)))
			      (printf "~%")
			      ))
			(value-of body new-env))))

	   (proc-exp (var body)
		     (proc-val
		      (procedure var body env)))

	   (call-exp (rator rand)
		     (let ((proc (expval->proc (value-of rator env)))
			   (arg  (value-of rand env)))
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

	   (right-exp (exp1)
		      (let ((v1 (value-of exp1 env)))
			(let ((p1 (expval->mutpair v1)))
			  (right p1))))

	   (setleft-exp (exp1 exp2)
			(let ((v1 (value-of exp1 env))
			      (v2 (value-of exp2 env)))
			  (let ((p (expval->mutpair v1)))
			    (begin
			      (setleft p v2)
			      (num-val 82)))))

	   (setright-exp (exp1 exp2)
			 (let ((v1 (value-of exp1 env))
			       (v2 (value-of exp2 env)))
			   (let ((p (expval->mutpair v1)))
			     (begin
			       (setright p v2)
			       (num-val 83)))))

	   ;;new stuff
	   (newarray-exp (count-exp val-exp)
			 (let ((count (expval->num (value-of count-exp env)))
			       (val (value-of val-exp env)))
			   (array-val (make-array count val))))

	   (arrayref-exp (exp1 exp2)
			 (let ((v1 (value-of exp1 env))
			       (v2 (value-of exp2 env)))
			   (let ((p (expval->array v1))
				 (pos (expval->num v2)))
			     (array-at p pos))))

	   (arrayset-exp (exp1 exp2 exp3)
			 (let ((v1 (value-of exp1 env))
			       (v2 (value-of exp2 env))
			       (v3 (value-of exp3 env)))
			   (let ((p (expval->array v1))
				 (pos (expval->num v2)))
			     (array-set! p pos v3))))
	   (arraylength-exp (exp)
			    (let ((arr (expval->array
					(value-of exp env))))
			      (num-val (array-length arr))))

	   )))


(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
	   (procedure (var body saved-env)
		      (let ((r (newref arg)))
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
			  (value-of body new-env)))))))


;; store->readable : Listof(List(Ref,Expval))
;;                    -> Listof(List(Ref,Something-Readable))
(define store->readable
  (lambda (l)
    (map
     (lambda (p)
       (list
	(car p)
	(expval->printable (cadr p))))
     l)))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;;(run-all)

(run "let a = newarray(2, -100)
                in arrayref(a, 1)")

(add-test! '(array
	     "let a = newarray(2, -100)
                in arrayref(a, 1)" -100))


(add-test! '(array-set
	     "let a = newarray(2, 1)
               in let v = arrayref(a, 0)
                  in begin arrayset(a, 0, 10); -(v, arrayref(a, 0)) end"
	     -9))

(add-test! '(array-test
	     "let a = newarray(2,-99)
               in let p = proc (x)
               let v = arrayref(x,1)
             in arrayset(x,1,-(v,-1))
           in begin arrayset(a,1,0); (p a); (p a); arrayref(a,1) end"
	     2))

;; new stuff
(add-test! '(array-length
	     "let a = newarray(10, 1)
                    in arraylength(a)" 10))

;;expected error
(add-test! '(array-index
	     "let a = newarray(10, 1)
               in arrayset(a, 10, 2)" error))
(run-all)
