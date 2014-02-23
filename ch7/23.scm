(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/subst.scm")
(load-relative "./base/inferred-cases.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/equal-type.scm")


;; based on 7.22
;; Extend the inferencer to handle pair types, as in exercise 7.8.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?))
  ;; new stuff
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

    ;; new stuff
    (expression
     ("pair" "(" expression "," expression ")")
     pair-exp)


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

    ;; new stuff
    (type
     ("pairof" type type)
     pairof-type)

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


;; new stuff, use hashtable as substitution.
;; new implementation of subst
(define the-subst (make-hash-table))

(define extend-subst
  (lambda (tvar ty)
  (set! (hash-table-ref the-subst tvar) ty)))

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
		      (if (hash-table-exists? the-subst ty)
			  (apply-subst-to-type (hash-table-ref the-subst ty))
			  ty))
	   ;;new stuff
	   (pairof-type (type1 type2)
			(pairof-type
			 (apply-subst-to-type type1)
			 (apply-subst-to-type type2))))))

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
                        (number->string serial-number))))
	   (pairof-type (type1 type2)
			(list
			 (type-to-external-form type1)
			 '*
			 (type-to-external-form type2))))))


;; unifier : Type * Type * Subst * Exp -> void OR Fails
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
	   exp)))
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
	   (tvar-type (serial-number) (not (equal? tvar ty)))
	   (pairof-type (type1 type2)
			(and (no-occurrence? tvar type1)
			     (no-occurrence? tvar type2))))))



;;;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;
;; type-of-program : Program -> Type
(define type-of-program
  (lambda (pgm)
    (cases program pgm
	   (a-program (exp1)
		      (let ((exp-type (type-of exp1 (init-tenv))))
			(apply-subst-to-type exp-type))))))

;; type-of : Exp * Tenv * Subst -> Type
(define type-of
  (lambda (exp tenv)
    (cases expression exp

	   (const-exp (num) (int-type))

	   (zero?-exp (exp1)
		      (let ((type1 (type-of exp1 tenv)))
			(begin
			  (unifier type1 (int-type) exp)
			  (bool-type))))

	   (diff-exp (exp1 exp2)
		     (let ((type1 (type-of exp1 tenv))
			   (type2 (type-of exp2 tenv)))
		       (begin
			 (unifier type1 (int-type) exp1)
			 (unifier type2 (int-type) exp2)
			 (int-type))))

	   (if-exp (exp1 exp2 exp3)
		   (let ((type1 (type-of exp1 tenv))
			 (type2 (type-of exp2 tenv))
			 (type3 (type-of exp3 tenv)))
		     (begin
		       (unifier type1 (bool-type) exp1)
		       (unifier type2 type3 exp2)
		       type2)))

	   ;; new stuff
	   (pair-exp (exp1 exp2)
		     (let ((type1 (type-of exp1 tenv))
			   (type2 (type-of exp2 tenv)))
		       (pairof-type type1 type2)))

	   (var-exp (var) (apply-tenv tenv var))

	   (let-exp (var exp1 body)
		    (let ((type1 (type-of exp1 tenv)))
		      (type-of body (extend-tenv var type1 tenv))))

	   (proc-exp (var otype body)
		     (let ((arg-type (otype->type otype)))
		       (let ((body-type (type-of body (extend-tenv var arg-type tenv))))
			 (proc-type arg-type body-type))))


	   (call-exp (rator rand)
		     (let ((result-type (fresh-tvar-type)))
		       (let ((rator-type (type-of rator tenv))
			     (rand-type  (type-of rand tenv)))
			 (begin
			   (unifier rator-type (proc-type rand-type result-type) exp)
			   result-type))))

	   (letrec-exp (proc-result-otype proc-name
					  bvar proc-arg-otype
					  proc-body letrec-body)
		       (let ((proc-result-type (otype->type proc-result-otype))
			     (proc-arg-type    (otype->type proc-arg-otype)))
			 (let ((tenv-for-letrec-body
				(extend-tenv proc-name
					     (proc-type proc-arg-type proc-result-type)
					     tenv)))
			   (let ((proc-body-type
				  (type-of proc-body
					   (extend-tenv bvar
							proc-arg-type
							tenv-for-letrec-body))))
			     (begin
			       (unifier proc-body-type proc-result-type proc-body)
			       (type-of letrec-body tenv-for-letrec-body))))))
	   )))


;;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;
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

	   ;; new stuff
	   (pair-exp (exp1 exp2)
		     (let ((val1 (value-of exp1 env))
			   (val2 (value-of exp2 env)))
		       (pair-val val1 val2)))

	   )))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 arg)
    (cases proc proc1
	   (procedure (var body saved-env)
		      (value-of body (extend-env var arg saved-env))))))

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

(run "pair (-(3, 0), 2)")
(check "pair (-(3, 0), 2)")
(check "let p = proc(v1 : int) pair(v1, v1) in (p 1)")
(run-all)
(check-all-inferred)
