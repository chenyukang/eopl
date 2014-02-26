(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/subst.scm")
(load-relative "./base/inferred-cases.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/equal-type.scm")


;; based on 7.22, use hashtable as substitution.
;; Extend the inferencer to handle multiple let declarations,
;; multi-argument procedures, and multiple letrec declarations

;; some cases which contain proc will fail for the type-form of
;; proc have changed
;; for example:  int -> int will converted to (int) -> int

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?))
  (pair-val
   (car expval?)
   (cdr expval?)))

(define-datatype proc proc?
  (procedure
   ;;new stuff
   (bvar (list-of symbol?))
   (body expression?)
   (env environment?)))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvar symbol?)
   (bval expval?)
   (saved-env environment?))
  (extend-env-rec
   (p-name symbol?)
   ;;new stuff
   (b-var (list-of symbol?))
   (p-body expression?)
   (saved-env environment?)))



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

    ;; new stuff
    (expression
     ("proc" "("  (separated-list identifier ":" optional-type ",") ")" expression)
     proc-exp)

    ;; new stuff
    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    ;; new stuff
    (expression
     ("letrec"
      optional-type identifier "("
      (separated-list identifier ":" optional-type ",")
      ")"
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

    ;; new stuff
    (type
     ("(" (arbno type) "->" type ")")
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
                       (map apply-subst-to-type t1)
                       (apply-subst-to-type t2)))
           (tvar-type (sn)
		      (if (hash-table-exists? the-subst ty)
			  (apply-subst-to-type (hash-table-ref the-subst ty))
			  ty)))))

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
           (proc-type (arg-types result-types) arg-types)
           (else (error 'proc-type->arg-type
                             "Not a proc type: ~s" ty)))))

(define proc-type->result-type
  (lambda (ty)
    (cases type ty
           (proc-type (arg-types result-type) result-type)
           (else (error 'proc-type->result-types
                             "Not a proc type: ~s" ty)))))

(define types->form
  (lambda (types)
    (if (null? types)
	'()
	(let ((left (types->form (cdr types))))
	  (cons (type-to-external-form (car types))
		(if (null? left)
		    left
		    (cons '* left)))))))

;; type-to-external-form : Type -> List
(define type-to-external-form
  (lambda (ty)
    (cases type ty
           (int-type () 'int)
           (bool-type () 'bool)
           (proc-type (args-type result-type)
		      (cons
                       (types->form args-type)
		       (cons
			'->
			(type-to-external-form result-type))))
           (tvar-type (serial-number)
                      (string->symbol
                       (string-append
                        "tvar"
                        (number->string serial-number)))))))

(define unifier-list
  (lambda (args1 args2 exp)
    (if (not (null? args1))
	(begin
	  (unifier (car args1) (car args2) exp)
	  (unifier-list (cdr args1) (cdr args2) exp)))))

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
	  (unifier-list
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
;; new stuff
(define no-occurrence-args?
  (lambda (tvar args)
    (if (null? args)
	#t
	(and (no-occurrence? tvar (car args))
	     (no-occurrence-args? tvar (cdr args))))))

(define no-occurrence?
  (lambda (tvar ty)
    (cases type ty
	   (int-type () #t)
	   (bool-type () #t)
	   (proc-type (args-type result-type)
		      (and
		       (no-occurrence-args? tvar args-type)
		       (no-occurrence? tvar result-type)))
	   (tvar-type (serial-number) (not (equal? tvar ty))))))


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

	   (var-exp (var) (apply-tenv tenv var))

	   (let-exp (var exp1 body)
		    (let ((type1 (type-of exp1 tenv)))
		      (type-of body (extend-tenv var type1 tenv))))

	   ;; new stuff
	   (proc-exp (vars otypes body)
		     (let ((arg-types (map otype->type otypes)))
			 (let ((body-type (type-of body (extend-tenv* vars arg-types tenv))))
			   (proc-type arg-types body-type))))


	   (call-exp (rator rands)
		     (let ((result-type (fresh-tvar-type)))
		       (let ((rator-type (type-of rator tenv))
			     (rands-type  (map (arg-type tenv) rands)))
			 (begin
			   (unifier rator-type (proc-type rands-type result-type) exp)
			   result-type))))

	   (letrec-exp (proc-result-otype proc-name bvar proc-args-otype
					  proc-body letrec-body)
		       (let ((proc-result-type (otype->type proc-result-otype))
			     (proc-args-type    (map otype->type proc-args-otype)))
			 (let ((tenv-for-letrec-body
				(extend-tenv proc-name
					     (proc-type proc-args-type proc-result-type)
					     tenv)))
			   (let ((proc-body-type
				  (type-of proc-body (extend-tenv* bvar
								  proc-args-type
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

(define extend-tenv*
  (lambda (vars types tenv)
    (if (null? vars)
	tenv
	(extend-tenv (car vars) (car types)
		     (extend-tenv* (cdr vars) (cdr types) tenv)))))

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

(define value-of-arg
  (lambda (env)
    (lambda (arg)
      (value-of arg env))))

(define arg-type
  (lambda (tenv)
    (lambda (arg)
      (type-of arg tenv))))

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

	   (proc-exp (bvars ty body)
		     (proc-val
		      (procedure bvars body env)))

	   (call-exp (rator rands)
		     (let ((proc (expval->proc (value-of rator env)))
			   (args  (map (value-of-arg env) rands)))
		       (apply-procedure proc args)))

	   (letrec-exp (ty1 p-name b-vars ty2 p-body letrec-body)
		       (value-of letrec-body
				 (extend-env-rec p-name b-vars p-body env)))

	   )))



(define extend-env*
  (lambda (vars vals saved-env)
    (if (null? vars)
	  saved-env
	  (extend-env (car vars) (car vals)
		      (extend-env* (cdr vars) (cdr vals) saved-env)))))

;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 args)
    (cases proc proc1
	   (procedure (vars body saved-env)
		      (value-of body (extend-env* vars args saved-env))))))

(run "let demo = proc(x : ?) x in 1")

(run "let func = proc(x : ?, y: ?) -(x, y) in (func 1 2)")
(run "let demo = proc(x : ?) x in -((demo 1), 1)")
(run "letrec ? f (x : ?) = (f x) in f")
(run "letrec ? f (x : ?) = (f x) in proc (n : ?) (f -(n,1))")
(run  "letrec ? even (odd : ?) = proc (x : ?) if zero?(x) then 1 else (odd -(x,1))
         in letrec  ? odd(x : ?) = if zero?(x) then 0 else ((even odd) -(x,1))
         in (odd 13)")

(run-all)

(check "let demo = proc(x : ?) x in demo")
(check "let demo = proc(x : ?, y : int) -(x, y) in demo")

(check "letrec ? f (x : ?) = (f x) in f")
(check "letrec ? f (x : ?) = (f x) in proc (n : ?) (f -(n,1))")
(check  "letrec ? even (odd : ?) = proc (x : ?) if zero?(x) then 1 else (odd -(x,1))
         in letrec  ? odd(x : ?) = if zero?(x) then 0 else ((even odd) -(x,1))
         in (odd 13)")


(check-all-inferred)
;; some cases which contain proc will fail for the type-form of
;; proc have changed
