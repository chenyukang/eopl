(load-relative "../libs/init.scm")
(load-relative "./base/exception-lang.scm")
(load-relative "./base/exception-data-structures.scm")
(load-relative "./base/exception-cases.scm")
(load-relative "./base/test.scm")


;; Rewrite the interpreter of section 5.4 using a procedural
;; and inlined representation.

;; This is challenging because we effectively have two observers,
;; apply-cont and apply-handler. As a hint, consider modifying
;; the recipe on page 6.1 so that we add to each procedure two
;; extra arguments, one representing the behavior of the
;; continuation under apply-cont and one representing its
;; behavior under apply-handler.

;; list with different represention, so re-define this.
(define sloppy->expval
  (lambda (sloppy-val)
    (cond
     ((number? sloppy-val) (num-val sloppy-val))
     ((boolean? sloppy-val) (bool-val sloppy-val))
     ((list? sloppy-val) (list-val (map sloppy->expval sloppy-val)))
     (else
      (error 'sloppy->expval
             "Can't convert sloppy value to expval: ~s"
             sloppy-val)))))

(define end-cont
  (lambda()
    (lambda (val)
      val)))

(define end-hand-cont
  (lambda ()
    (lambda (val)
      (error 'end-hand-cont
             "uncaught exception!"
	     val))))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define apply-handler
  (lambda (val cont)
    (cont val)))

(define diff1-cont
  (lambda (exp2 env cont hand-cont)
    (lambda (val)
      (value-of/k exp2 env
		  (diff2-cont val cont)
		  hand-cont))))

(define diff2-cont
  (lambda (val2 cont)
    (lambda (val1)
      (let ((num1 (expval->num val1))
	    (num2 (expval->num val2)))
	(apply-cont cont
		    (num-val (- num2 num1)))))))

(define if-test-cont
  (lambda (exp2 exp3 env cont hand-cont)
    (lambda (val)
      (if (expval->bool val)
	  (value-of/k exp2 env cont hand-cont)
	  (value-of/k exp3 env cont hand-cont)))))

(define rator-cont
  (lambda (rand env cont hand-cont)
    (lambda (rator)
      (value-of/k rand env
                  (rand-cont rator cont hand-cont)
		  hand-cont))))

(define rand-cont
  (lambda (rator cont hand-cont)
    (lambda (rand-val)
      (let ((proc (expval->proc rator)))
        (apply-procedure proc rand-val cont hand-cont)))))

(define unop-arg-cont
  (lambda (unop cont)
    (lambda (val)
      (apply-cont cont
		  (apply-unop unop val)))))

(define try-cont
  (lambda (var exp env cont)
    (lambda (val)
      (apply-cont cont val))))

(define raise1-cont
  (lambda (hand-cont)
    (lambda (val)
      (apply-handler val hand-cont))))


(define diff1-hand-cont
  (lambda (exp2 env hand-cont)
    (lambda (val)
      (apply-handler val hand-cont))))

(define diff2-hand-cont
  (lambda (val1 cont hand-cont)
    (lambda (val)
      (apply-handler val hand-cont))))

(define if-test-hand-cont
  (lambda (exp2 exp3 env hand-cont)
    (lambda (val)
      (apply-handler val hand-cont))))

(define unop-arg-hand-cont
  (lambda (unop hand-cont)
    (lambda (val)
      (apply-handler val hand-cont))))

(define rator-hand-cont
  (lambda (rand env hand-cont)
    (lambda (val)
      (apply-handler val hand-cont))))

(define rand-hand-cont
  (lambda (val1 cont)
    (lambda (val)
      (apply-handler val cont))))

(define try-hand-cont
  (lambda (var exp env cont hand-cont)
    (lambda (val)
      (value-of/k exp
		  (extend-env var val env)
		  cont
		  hand-cont))))

(define raise1-hand-cont
  (lambda (cont)
    (lambda (val)
      (apply-handler val cont))))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
	   (a-program (body)
		      (value-of/k
		       body (init-env) (end-cont) (end-hand-cont))))))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont hand-cont)
    (cases expression exp

	   (const-exp (num) (apply-cont cont (num-val num)))

	   (const-list-exp (nums)
			   (apply-cont cont
				       (list-val (map num-val nums))))

	   (var-exp (var) (apply-cont cont (apply-env env var)))

	   (diff-exp (exp1 exp2)
		     (value-of/k exp1 env
				 (diff1-cont exp2 env cont hand-cont)
				 (diff1-hand-cont exp2 env hand-cont)))

	   (unop-exp (unop exp1)
		     (value-of/k exp1 env
				 (unop-arg-cont unop cont)
				 (unop-arg-hand-cont unop hand-cont)))

	   (if-exp (exp1 exp2 exp3)
		   (value-of/k exp1 env
			       (if-test-cont exp2 exp3 env cont hand-cont)
			       (if-test-hand-cont exp2 exp3 env hand-cont)))

	   (proc-exp (var body)
		     (apply-cont cont
				 (proc-val
				  (procedure var body env))))

	   (call-exp (rator rand)
		     (value-of/k rator env
				 (rator-cont rand env cont hand-cont)
				 (rator-hand-cont rand env hand-cont)))

	   (let-exp (var exp1 body)
		    (value-of/k
		     (call-exp (proc-exp var body) exp1)
		     env
		     cont
		     hand-cont))

	   (letrec-exp (p-name b-var p-body letrec-body)
		       (value-of/k
			letrec-body
			(extend-env-rec p-name b-var p-body env)
			cont
			hand-cont))

	   (try-exp (exp1 var handler-exp)
		    (value-of/k exp1 env
				(try-cont var handler-exp env cont)
				(try-hand-cont
				 var handler-exp env cont hand-cont)))

	   (raise-exp (exp1)
		      (value-of/k exp1 env
				  ;; catch it now!
				  (raise1-hand-cont hand-cont)
				  (raise1-hand-cont hand-cont))))))



;; apply-procedure : procedure * expval * cont -> final-expval
(define apply-procedure
  (lambda (proc1 arg cont hand-cont)
    (cases proc proc1
	   (procedure (var body saved-env)
		      (value-of/k body
				  (extend-env var arg saved-env)
				  cont
				  hand-cont)))))

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


(run-all)

