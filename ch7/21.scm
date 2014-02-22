(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/subst.scm")
(load-relative "./base/inferred-cases.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/equal-type.scm")


;; new stuff
(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?))
  ;;new stuff
  (pair-val
   (car expval?)
   (cdr expval?)))


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


;; new stuff
;; new implementation of subst
;; empty-subst is same

(define the-subst (empty-subst))

(define extend-subst
  (lambda (tvar ty)
    (set! the-subst (cons (cons tvar ty) the-subst))))

(define apply-subst-to-type
  (lambda (ty)
    (cases type ty
           (int-type () (int-type))
           (bool-type () (bool-type))
           (proc-type (t1 t2)
                      (proc-type
                       (apply-subst-to-type t1)
                       (apply-subst-to-type t2)))
           (tvar-type (sn)
                      (let ((tmp (assoc ty the-subst)))
                        (if tmp
                            (apply-subst-to-type (cdr tmp)) ;;if type is a var-type, re-appply it.
                            ty))))))


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

(define substitution?
  (list-of (pair-of tvar-type? type?)))


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
(define unifier
  (lambda (ty1 ty2 exp)
    (let ((ty1 (apply-subst-to-type ty1))
          (ty2 (apply-subst-to-type ty2)))
      (cond
       ((equal? ty1 ty2) the-subst)
       ((tvar-type? ty1)
        (if (no-occurrence? ty1 ty2)
            (extend-subst ty1 ty2)
	    (report-no-occurrence-violation ty1 ty2 exp)))
       ((tvar-type? ty2)
	(if (no-occurrence? ty2 ty1)
	    (extend-subst ty2 ty1)
	    (report-no-occurrence-violation ty2 ty1 exp)))
       ((and (proc-type? ty1) (proc-type? ty2))
	(begin
	  (unifier
	   (proc-type->arg-type ty1)
	   (proc-type->arg-type ty2)
	   exp)
	  (unifier
	   (proc-type->result-type ty1)
	   (proc-type->result-type ty2)
	   exp)
	  the-subst))
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
(define type-of-program
  (lambda (pgm)
    (cases program pgm
	   (a-program (exp1)
		      (cases answer (type-of exp1 (init-tenv))
			     (an-answer (ty subst)
					(apply-subst-to-type ty)))))))

;; type-of : Exp * Tenv * Subst -> Type
(define type-of
  (lambda (exp tenv)
    (cases expression exp

	   (const-exp (num) (an-answer (int-type) the-subst))

	   (zero?-exp (exp1)
		      (cases answer (type-of exp1 tenv)
			     (an-answer (type1 subst1)
					(let ((subst2 (unifier type1 (int-type) exp)))
					  (an-answer (bool-type) the-subst)))))

	   (diff-exp (exp1 exp2)
		     (cases answer (type-of exp1 tenv)
			    (an-answer (type1 subst1)
				       (let ((subst1 (unifier type1 (int-type) exp1)))
					 (cases answer (type-of exp2 tenv)
						(an-answer (type2 subst2)
							   (let ((subst2
								  (unifier type2 (int-type) exp2)))
							     (an-answer (int-type) the-subst))))))))

	   (if-exp (exp1 exp2 exp3)
		   (cases answer (type-of exp1 tenv)
			  (an-answer (ty1 subst)
				     (let ((subst (unifier ty1 (bool-type)
							   exp1)))
				       (cases answer (type-of exp2 tenv)
					      (an-answer (ty2 subst)
							 (cases answer (type-of exp3 tenv)
								(an-answer (ty3 subst)
									   (let ((subst (unifier ty2 ty3 exp)))
									     (an-answer ty2 the-subst))))))))))

	   (var-exp (var) (an-answer (apply-tenv tenv var) the-subst))

	   (let-exp (var exp1 body)
		    (cases answer (type-of exp1 tenv)
			   (an-answer (rhs-type subst)
				      (type-of body
					       (extend-tenv var rhs-type tenv)))))

	   (proc-exp (var otype body)
		     (let ((arg-type (otype->type otype)))
		       (cases answer (type-of body (extend-tenv var arg-type tenv))
			      (an-answer (result-type subst)
					 (an-answer
					  (proc-type arg-type result-type)
					  the-subst)))))

	   (call-exp (rator rand)
		     (let ((result-type (fresh-tvar-type)))
		       (cases answer (type-of rator tenv)
			      (an-answer (rator-type subst)
					 (cases answer (type-of rand tenv)
						(an-answer (rand-type subst)
							   (begin
							     (unifier rator-type
								      (proc-type rand-type result-type)
									   exp)
							     (an-answer result-type the-subst))))))))

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
						   bvar proc-arg-type tenv-for-letrec-body))
				  (an-answer (proc-body-type subst)
					     (let ((subst
						    (unifier proc-body-type proc-result-type
							     proc-body)))
					       (type-of letrec-body
							tenv-for-letrec-body
							)))))))

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
(define fresh-tvar-type
  (let ((sn 0))
    (lambda ()
      (set! sn (+ sn 1))
      (tvar-type sn))))

;; otype->type : OptionalType -> Type
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
