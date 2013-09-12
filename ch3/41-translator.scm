;; translation-of-program : Program -> Nameless-program
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
	   (a-program (exp1)
		      (a-program
		       (translation-of exp1 (init-senv)))))))

(define translation-one
  (lambda (senv)
    (lambda (elem)
      (translation-of elem senv))))

;; translation-of : Exp * Senv -> Nameless-exp
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
		      (nameless-var-exp
		       (car res)
		       (cadr res))))

	   (let-exp (vars exps body)
		    (nameless-let-exp
		     (map (translation-one senv) exps)
		     (translation-of body
				     (extend-senv vars senv))))
	   (proc-exp (vars body)
		     (nameless-proc-exp
		      (translation-of body
				      (extend-senv vars senv))))
	   (call-exp (rator rands)
		     (call-exp
		      (translation-of rator senv)
		      (map (translation-one senv) rands)))

	   (else (report-invalid-source-expression exp))
	   )))

(define report-invalid-source-expression
  (lambda (exp)
    (error 'value-of
		"Illegal expression in source code: ~s" exp)))

   ;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;

;; empty-senv : () -> Senv
(define empty-senv
  (lambda ()
    '()))

;; extend-senv : Var * Senv -> Senv
(define extend-senv
  (lambda (var senv)
    (cons var senv)))

(define apply-current
  (lambda (sub-senv var pos)
    (cond ((null? sub-senv) #f)
	  ((eqv? var (car sub-senv))
	   pos)
	  (else
	   (apply-current (cdr sub-senv)
			  var
			  (+ pos 1))))))

(define apply-senv-iter
  (lambda (senv var depth)
    (if (null? senv) (report-unbound-var)
	(let ((pos (apply-current (car senv) var 0)))
	      (if pos
		  (list depth pos)
		  (apply-senv-iter (cdr senv)
				   var
				   (+ depth 1)))))))

;; apply-senv : Senv * Var -> Lexaddr
(define apply-senv
  (lambda (senv var)
    (apply-senv-iter senv var 0)))

(define report-unbound-var
  (lambda (var)
    (error 'translation-of "unbound variable in code: ~s" var)))

;; init-senv : () -> Senv
(define init-senv
  (lambda ()
    (empty-senv)))
