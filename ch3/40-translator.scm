;; translation-of-program : Program -> Nameless-program
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
	   (a-program (exp1)
		      (a-program
		       (translation-of exp1 (init-senv)))))))

;; translation-of : Exp * Senv -> Nameless-exp
;; Page 97
(define translation-of
  (lambda (exp senv)
    (cases expression exp
	   (const-exp (num) (const-exp num))
	   (diff-exp (exp1 exp2)
		     (diff-exp
		      (translation-of exp1 senv)
		      (translation-of exp2 senv)))
	   (zero?-exp (exp1)
		      (zero?-exp
		       (translation-of exp1 senv)))

	   (if-exp (exp1 exp2 exp3)
		   (if-exp
		    (translation-of exp1 senv)
		    (translation-of exp2 senv)
		    (translation-of exp3 senv)))

	   (var-exp (var)
		    (let ((res (apply-senv senv var)))
		      (cond ((eqv? (car res) 'normal)
			     (nameless-var-exp (cadr res)))
			    ((eqv? (car res) 'letrec)
			     (nameless-letrec-var-exp (cadr res)))
			    (else (error "translation-of: error type" res)))))

	   (letrec-exp (p-name p-var p-body letrec-body)
		       (nameless-let-exp
			(translation-of p-body (extend-senv-normal p-var 
						 (extend-senv-letrec p-name senv)))
			(translation-of letrec-body
					(extend-senv-letrec p-name senv))))

	   (let-exp (var exp1 body)
		    (nameless-let-exp
		     (translation-of exp1 senv)
		     (translation-of body
				     (extend-senv-normal var senv))))
	   (proc-exp (var body)
		     (nameless-proc-exp
		      (translation-of body
				      (extend-senv-normal var senv))))
	   (call-exp (rator rand)
		     (call-exp
		      (translation-of rator senv)
		      (translation-of rand senv)))
	   
	   (else (report-invalid-source-expression exp))
	   )))

(define report-invalid-source-expression
  (lambda (exp)
    (error 'value-of
		"Illegal expression in source code: ~s" exp)))

   ;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;

;; empty-senv : () -> Senv
;; Page: 95
(define empty-senv
  (lambda ()
    '()))

;; extend-senv : Var * Senv -> Senv
(define extend-senv
  (lambda (type var senv)
    (cons (list type var) senv)))

(define extend-senv-normal
  (lambda (var senv)
    (extend-senv 'normal var senv)))

(define extend-senv-letrec
  (lambda (var senv)
    (extend-senv 'letrec var senv)))

;; apply-senv : Senv * Var -> Lexaddr
;; Page: 95
(define apply-senv-iter
  (lambda (senv var depth)
    (cond ((null? senv) (report-unbound-var var))
	  ((eqv? var (cadar senv))
	   (list (caar senv) depth))
	  (else
	   (apply-senv-iter (cdr senv) var (+ depth 1))))))
    
(define apply-senv
  (lambda (senv var)
    (apply-senv-iter senv var 0)))

(define report-unbound-var
  (lambda (var)
    (error 'translation-of "unbound variable in code: ~s" var)))

;; init-senv : () -> Senv
;; Page: 96
(define init-senv
  (lambda ()
    (empty-senv)))
