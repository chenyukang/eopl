(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/subst.scm")
(load-relative "./base/inferred-cases.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/equal-type.scm")

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
     ("proc" "("  identifier ":" optional-type ")" expression)
     proc-exp)

    (expression
     ("(" expression expression ")")
     call-exp)

    (expression
     ("letrec"
      optional-type identifier "(" identifier ":" optional-type ")"
      "=" expression "in" expression)
     letrec-exp)

    (optional-type
     ("?")
     no-type)

    (optional-type
     (type)
     a-type)

    (type
     ("int")
     int-type)

    (type
     ("bool")
     bool-type)

    (type
     ("(" type "->" type ")")
     proc-type)

    (type
     ("%tvar-type" number)
     tvar-type)

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))



  ;;;;;;;;;;;;;;;; syntactic tests and observers ;;;;;;;;;;;;;;;;

(define atomic-type?
  (lambda (ty)
    (cases type ty
           (proc-type (ty1 ty2) #f)
           (tvar-type (sn) #f)
           (else #t))))

(define proc-type?
  (lambda (ty)
    (cases type ty
           (proc-type (t1 t2) #t)
           (else #f))))

(define tvar-type?
  (lambda (ty)
    (cases type ty
           (tvar-type (serial-number) #t)
           (else #f))))


(define proc-type->arg-type
  (lambda (ty)
    (cases type ty
           (proc-type (arg-type result-type) arg-type)
           (else (error 'proc-type->arg-type
                             "Not a proc type: ~s" ty)))))

(define proc-type->result-type
  (lambda (ty)
    (cases type ty
           (proc-type (arg-type result-type) result-type)
           (else (error 'proc-type->result-types
                             "Not a proc type: ~s" ty)))))

;; type-to-external-form : Type -> List
;; Page: 266
(define type-to-external-form
  (lambda (ty)
    (cases type ty
           (int-type () 'int)
           (bool-type () 'bool)
           (proc-type (arg-type result-type)
                      (list
                       (type-to-external-form arg-type)
                       '->
                       (type-to-external-form result-type)))
           (tvar-type (serial-number)
                      (string->symbol
                       (string-append
                        "tvar"
                        (number->string serial-number)))))))


;; unifier : Type * Type * Subst * Exp -> Subst OR Fails
;; Page: 264
(define unifier
  (lambda (ty1 ty2 subst exp)
    (let ((ty1 (apply-subst-to-type ty1 subst))
          (ty2 (apply-subst-to-type ty2 subst)))
      (cond
       ((equal? ty1 ty2) subst)
       ((tvar-type? ty1)
        (if (no-occurrence? ty1 ty2)
            (extend-subst subst ty1 ty2)
	    (report-no-occurrence-violation ty1 ty2 exp)))
       ((tvar-type? ty2)
	(if (no-occurrence? ty2 ty1)
	    (extend-subst subst ty2 ty1)
	    (report-no-occurrence-violation ty2 ty1 exp)))
       ((and (proc-type? ty1) (proc-type? ty2))
	(let ((subst (unifier
		      (proc-type->arg-type ty1)
		      (proc-type->arg-type ty2)
		      subst exp)))
	  (let ((subst (unifier
			(proc-type->result-type ty1)
			(proc-type->result-type ty2)
			subst exp)))
	    subst)))
       (else (report-unification-failure ty1 ty2 exp))))))

(define report-unification-failure
  (lambda (ty1 ty2 exp)
    (error 'unification-failure
		"Type mismatch: ~s doesn't match ~s in ~s~%"
		(type-to-external-form ty1)
		(type-to-external-form ty2)
		exp)))

(define report-no-occurrence-violation
  (lambda (ty1 ty2 exp)
    (error 'check-no-occurence!
		"Can't unify: type variable ~s occurs in type ~s in expression ~s~%"
		(type-to-external-form ty1)
		(type-to-external-form ty2)
		exp)))

;; no-occurrence? : Tvar * Type -> Bool
;; usage: Is there an occurrence of tvar in ty?
;; Page: 265
(define no-occurrence?
  (lambda (tvar ty)
    (cases type ty
	   (int-type () #t)
	   (bool-type () #t)
	   (proc-type (arg-type result-type)
		      (and
		       (no-occurrence? tvar arg-type)
		       (no-occurrence? tvar result-type)))
	   (tvar-type (serial-number) (not (equal? tvar ty))))))



  ;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;

;; we'll be thinking of the type of an expression as pair consisting
;; of a type (possibly with some type variables in it) and a
;; substitution that tells us how to interpret those type variables.

;; Answer = Type * Subst
;; type-of: Exp * Tenv * Subst  -> Answer

(define-datatype answer answer?
  (an-answer
   (type type?)
   (subst substitution?)))

;; type-of-program : Program -> Type
;; Page: 267
(define type-of-program
  (lambda (pgm)
    (cases program pgm
	   (a-program (exp1)
		      (cases answer (type-of exp1 (init-tenv) (empty-subst))
			     (an-answer (ty subst)
					(apply-subst-to-type ty subst)))))))

;; type-of : Exp * Tenv * Subst -> Type
;; Page: 267--270
(define type-of
  (lambda (exp tenv subst)
    (cases expression exp

	   (const-exp (num) (an-answer (int-type) subst))

	   (zero?-exp (exp1)
		      (cases answer (type-of exp1 tenv subst)
			     (an-answer (type1 subst1)
					(let ((subst2 (unifier type1 (int-type) subst1 exp)))
					  (an-answer (bool-type) subst2)))))

	   (diff-exp (exp1 exp2)
		     (cases answer (type-of exp1 tenv subst)
			    (an-answer (type1 subst1)
				       (let ((subst1 (unifier type1 (int-type) subst1 exp1)))
					 (cases answer (type-of exp2 tenv subst1)
						(an-answer (type2 subst2)
							   (let ((subst2
								  (unifier type2 (int-type) subst2 exp2)))
							     (an-answer (int-type) subst2))))))))

	   (if-exp (exp1 exp2 exp3)
		   (cases answer (type-of exp1 tenv subst)
			  (an-answer (ty1 subst)
				     (let ((subst (unifier ty1 (bool-type) subst
							   exp1)))
				       (cases answer (type-of exp2 tenv subst)
					      (an-answer (ty2 subst)
							 (cases answer (type-of exp3 tenv subst)
								(an-answer (ty3 subst)
									   (let ((subst (unifier ty2 ty3 subst exp)))
									     (an-answer ty2 subst))))))))))

	   (var-exp (var) (an-answer (apply-tenv tenv var) subst))

	   (let-exp (var exp1 body)
		    (cases answer (type-of exp1 tenv subst)
			   (an-answer (rhs-type subst)
				      (type-of body
					       (extend-tenv var rhs-type tenv)
					       subst))))

	   (proc-exp (var otype body)
		     (let ((arg-type (otype->type otype)))
		       (cases answer (type-of body
					      (extend-tenv var arg-type tenv)
					      subst)
			      (an-answer (result-type subst)
					 (an-answer
					  (proc-type arg-type result-type)
					  subst)))))

	   (call-exp (rator rand)
		     (let ((result-type (fresh-tvar-type)))
		       (cases answer (type-of rator tenv subst)
			      (an-answer (rator-type subst)
					 (cases answer (type-of rand tenv subst)
						(an-answer (rand-type subst)
							   (let ((subst
								  (unifier rator-type
									   (proc-type rand-type result-type)
									   subst
									   exp)))
							     (an-answer result-type subst))))))))

	   (letrec-exp (proc-result-otype proc-name
					  bvar proc-arg-otype
					  proc-body
					  letrec-body)
		       (let ((proc-result-type
			      (otype->type proc-result-otype))
			     (proc-arg-type
			      (otype->type proc-arg-otype)))
			 (let ((tenv-for-letrec-body
				(extend-tenv
				 proc-name
				 (proc-type proc-arg-type proc-result-type)
				 tenv)))
			   (cases answer (type-of proc-body
						  (extend-tenv
						   bvar proc-arg-type tenv-for-letrec-body)
						  subst)
				  (an-answer (proc-body-type subst)
					     (let ((subst
						    (unifier proc-body-type proc-result-type subst
							     proc-body)))
					       (type-of letrec-body
							tenv-for-letrec-body
							subst)))))))

	   )))

    ;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;

;; why are these separated?

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
   (sym symbol?)
   (type type?)
   (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv
  (lambda (tenv sym)
    (cases type-environment tenv
	   (empty-tenv-record ()
			      (error 'apply-tenv "Unbound variable ~s" sym))
	   (extended-tenv-record (sym1 val1 old-env)
				 (if (eqv? sym sym1)
				     val1
				     (apply-tenv old-env sym))))))

(define init-tenv
  (lambda ()
    (extend-tenv 'x (int-type)
		 (extend-tenv 'v (int-type)
			      (extend-tenv 'i (int-type)
					   (empty-tenv))))))

;; fresh-tvar-type : () -> Type
;; Page: 265
(define fresh-tvar-type
  (let ((sn 0))
    (lambda ()
      (set! sn (+ sn 1))
      (tvar-type sn))))

;; otype->type : OptionalType -> Type
;; Page: 265
(define otype->type
  (lambda (otype)
    (cases optional-type otype
	   (no-type () (fresh-tvar-type))
	   (a-type (ty) ty))))


;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
	   (a-program (body)
		      (value-of body (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp

	   (const-exp (num) (num-val num))

	   (var-exp (var) (apply-env env var))

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
		    (let ((val (value-of exp1 env)))
		      (value-of body
				(extend-env var val env))))

	   (proc-exp (bvar ty body)
		     (proc-val
		      (procedure bvar body env)))

	   (call-exp (rator rand)
		     (let ((proc (expval->proc (value-of rator env)))
			   (arg  (value-of rand env)))
		       (apply-procedure proc arg)))

	   (letrec-exp (ty1 p-name b-var ty2 p-body letrec-body)
		       (value-of letrec-body
				 (extend-env-rec p-name b-var p-body env)))

	   )))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
	   (procedure (var body saved-env)
		      (value-of body (extend-env var arg saved-env))))))


(run-all)


(check "let demo = proc(x : ?) x in -((demo 1), 1)")
(check "letrec ? f (x : ?) = (f x) in f")
(check "letrec ? f (x : ?) = (f x) in proc (n : ?) (f -(n,1))")
(check  "letrec ? even (odd : ?) = proc (x : ?) if zero?(x) then 1 else (odd -(x,1))
         in letrec  ? odd(x : ?) = if zero?(x) then 0 else ((even odd) -(x,1))
         in (odd 13)")


(run "let demo = proc(x : ?) x in -((demo 1), 1)")
(run "letrec ? f (x : ?) = (f x) in f")
(run "letrec ? f (x : ?) = (f x) in proc (n : ?) (f -(n,1))")
(run  "letrec ? even (odd : ?) = proc (x : ?) if zero?(x) then 1 else (odd -(x,1))
         in letrec  ? odd(x : ?) = if zero?(x) then 0 else ((even odd) -(x,1))
         in (odd 13)")

(check-all-inferred)
