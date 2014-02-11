(load-relative "../libs/init.scm")
(load-relative "./base/scheduler.scm")
(load-relative "./base/semaphores.scm")
(load-relative "./base/store.scm")
(load-relative "./base/queues.scm")
(load-relative "./base/thread-cases.scm")
(load-relative "./base/environments.scm")

;; add yield for the lang, return 99 when yield returning
;; see the new stuff

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

    ;; like list(n1,...,nk) in exceptions language. Sorry about that.
    (expression
     ("[" (separated-list number ",") "]")
     const-list-exp)

    (expression (identifier) var-exp)

    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression
     ("proc" "(" identifier ")" expression)
     proc-exp)

    (expression
     ("(" expression expression ")")
     call-exp)

    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ;; arbitrary number of unary procedures
     ("letrec"
      (arbno identifier "(" identifier ")" "=" expression)
      "in" expression)
     letrec-exp)

    (expression
     ("set" identifier "=" expression)
     set-exp)

    (expression
     ("spawn" "(" expression ")")
     spawn-exp)

    ;;new stuff
    (expression
     ("yield" "(" ")")
     yield-exp)

    (expression
     ("mutex" "(" ")")
     mutex-exp)

    (expression
     ("wait" "(" expression ")")
     wait-exp)

    (expression
     ("signal" "(" expression ")")
     signal-exp)

    ;; other unary operators

    (expression
     (unop "(" expression ")")
     unop-exp)

    (unop ("car") car-unop)
    (unop ("cdr") cdr-unop)
    (unop ("null?") null?-unop)
    (unop ("zero?") zero?-unop)
    (unop ("print") print-unop)

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))


  ;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;
(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?))
  (list-val
   (lst (list-of expval?)))
  (mutex-val
   (mutex mutex?))
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

(define expval->list
  (lambda (v)
    (cases expval v
	   (list-val (lst) lst)
	   (else (expval-extractor-error 'list v)))))

(define expval->mutex
  (lambda (v)
    (cases expval v
	   (mutex-val (l) l)
	   (else (expval-extractor-error 'mutex v)))))

(define expval-extractor-error
  (lambda (variant value)
    (error 'expval-extractors "Looking for a ~s, found ~s"
	   variant value)))

;;;;;;;;;;;;;;;; mutexes ;;;;;;;;;;;;;;;;

(define-datatype mutex mutex?
  (a-mutex
   (ref-to-closed?    reference?)    ; ref to bool
   (ref-to-wait-queue reference?)))  ; ref to (listof thread)

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (bvar symbol?)
   (body expression?)
   (env environment?)))

;; used by begin-exp
(define fresh-identifier
  (let ((sn 0))
    (lambda (identifier)
      (set! sn (+ sn 1))
      (string->symbol
       (string-append
	(symbol->string identifier)
	"%"             ; this can't appear in an input identifier
	(number->string sn))))))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;


(define-datatype continuation continuation?

  (end-main-thread-cont)
  (end-subthread-cont)

  (diff1-cont
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (diff2-cont
   (val1 expval?)
   (cont continuation?))
  (if-test-cont
   (exp2 expression?)
   (exp3 expression?)
   (env environment?)
   (cont continuation?))
  (rator-cont
   (rand expression?)
   (env environment?)
   (cont continuation?))
  (rand-cont
   (val1 expval?)
   (cont continuation?))
  (set-rhs-cont
   (loc reference?)
   (cont continuation?))

  (spawn-cont
   (saved-cont continuation?))
  (wait-cont
   (saved-cont continuation?))
  (signal-cont
   (saved-cont continuation?))

  (unop-arg-cont
   (unop1 unop?)
   (cont continuation?))
  )


;; value-of-program : Program * Int -> ExpVal
;; Page: 185
(define value-of-program
  (lambda (timeslice pgm)
    (initialize-store!)
    (initialize-scheduler! timeslice)
    (cases program pgm
	   (a-program (exp1)
		      (value-of/k
		       exp1
		       (init-env)
		       (end-main-thread-cont))))))

(define trace-interp (make-parameter #f))

;; value-of/k : Exp * Env * Cont -> FinalAnswer
;; Page 182
(define value-of/k
  (lambda (exp env cont)

    (if (trace-interp)
        (printf "value-of/k: ~s~%" exp))

    (cases expression exp

	   (const-exp (num) (apply-cont cont (num-val num)))

	   (const-list-exp (nums)
			   (apply-cont cont
				       (list-val (map num-val nums))))

	   (var-exp (var) (apply-cont cont (deref (apply-env env var))))

	   (diff-exp (exp1 exp2)
		     (value-of/k exp1 env
				 (diff1-cont exp2 env cont)))

	   (if-exp (exp1 exp2 exp3)
		   (value-of/k exp1 env
			       (if-test-cont exp2 exp3 env cont)))

	   (proc-exp (var body)
		     (apply-cont cont
				 (proc-val
				  (procedure var body env))))

	   (call-exp (rator rand)
		     (value-of/k rator env
				 (rator-cont rand env cont)))

	   (let-exp (var exp1 body)          ; implemented like a macro!
		    (value-of/k
		     (call-exp
		      (proc-exp var body)
		      exp1)
		     env
		     cont))

	   (begin-exp (exp exps)           ; this one, too
		      (if (null? exps)
			  (value-of/k exp env cont)
			  (value-of/k
			   (call-exp
			    (proc-exp
			     (fresh-identifier 'dummy)
			     (begin-exp (car exps) (cdr exps)))
			    exp)
			   env
			   cont)))

	   (letrec-exp (p-names b-vars p-bodies letrec-body)
		       (value-of/k
			letrec-body
			(extend-env-rec* p-names b-vars p-bodies env)
			cont))

	   (set-exp (id exp)
		    (value-of/k exp env
				(set-rhs-cont (apply-env env id) cont)))

	   (spawn-exp (exp)
		      (value-of/k exp env
				  (spawn-cont cont)))

	   ;;new stuff
	   (yield-exp ()
		      (place-on-ready-queue!
		       (lambda () (apply-cont cont (num-val 99))))
		      (run-next-thread))

	   (mutex-exp ()
		      (apply-cont cont (mutex-val (new-mutex))))

	   (wait-exp (exp)
		     (value-of/k exp env
				 (wait-cont cont)))

	   (signal-exp (exp)
		       (value-of/k exp env
				   (signal-cont cont)))

	   (unop-exp (unop1 exp)
		     (value-of/k exp env
				 (unop-arg-cont unop1 cont)))

	   )))

;; apply-cont : Cont * Exp -> FinalAnswer
(define apply-cont
  (lambda (cont val)
    (if (time-expired?)
        (begin
          (place-on-ready-queue!
	   (lambda () (apply-cont cont val)))
          (run-next-thread))
        (begin

          (decrement-timer!)

          (cases continuation cont

		 (end-main-thread-cont ()
				       (set-final-answer! val)
				       (run-next-thread))

		 (end-subthread-cont ()
				     (run-next-thread))

		 (diff1-cont (exp2 saved-env saved-cont)
			     (value-of/k exp2 saved-env (diff2-cont val saved-cont)))
		 (diff2-cont (val1 saved-cont)
			     (let ((n1 (expval->num val1))
				   (n2 (expval->num val)))
			       (apply-cont saved-cont
					   (num-val (- n1 n2)))))
		 (if-test-cont (exp2 exp3 env cont)
			       (if (expval->bool val)
				   (value-of/k exp2 env cont)
				   (value-of/k exp3 env cont)))
		 (rator-cont (rand saved-env saved-cont)
			     (value-of/k rand saved-env
					 (rand-cont val saved-cont)))
		 (rand-cont (val1 saved-cont)
			    (let ((proc (expval->proc val1)))
			      (apply-procedure proc val saved-cont)))
		 (set-rhs-cont (loc cont)
			       (begin
				 (setref! loc val)
				 (apply-cont cont (num-val 26))))

		 (spawn-cont (saved-cont)
			     (let ((proc1 (expval->proc val)))
			       (place-on-ready-queue!
				(lambda ()
				  (apply-procedure proc1
						   (num-val 28)
						   (end-subthread-cont))))
			       (apply-cont saved-cont (num-val 73))))

		 (wait-cont (saved-cont)
			    (wait-for-mutex
			     (expval->mutex val)
			     (lambda () (apply-cont saved-cont (num-val 52)))))

		 (signal-cont (saved-cont)
			      (signal-mutex
			       (expval->mutex val)
			       (lambda () (apply-cont saved-cont (num-val 53)))))

		 (unop-arg-cont (unop1 cont)
				(apply-unop unop1 val cont))

		 )))))

(define apply-procedure
  (lambda (proc1 arg cont)
    (cases proc proc1
	   (procedure (var body saved-env)
		      (value-of/k body
				  (extend-env var (newref arg) saved-env)
				  cont)))))

(define apply-unop
  (lambda (unop1 arg cont)
    (cases unop unop1
	   (zero?-unop ()
		       (apply-cont cont
				   (bool-val
				    (zero? (expval->num arg)))))

	   (car-unop ()
		     (let ((lst (expval->list arg)))
		       (apply-cont cont (car lst))))
	   (cdr-unop ()
		     (let ((lst (expval->list arg)))
		       (apply-cont cont (list-val (cdr lst)))))

	   (null?-unop ()
		       (apply-cont cont
				   (bool-val (null? (expval->list arg)))))

	   (print-unop ()
		       (begin
			 (eopl:printf "~a~%" (expval->num arg))
			 (apply-cont cont (num-val 1))))

	   )))


(define run
  (lambda (timeslice string)
    (value-of-program timeslice (scan&parse string))))

(define run-all
  (lambda (timeslice)
    (run-tests!
     (lambda (string) (run timeslice string))
     equal-answer? test-list)))

(define run-one
  (lambda (timeslice test-name)
    (let ((the-test (assoc test-name test-list)))
      (cond
       ((assoc test-name test-list)
	=> (lambda (test)
	     (run timeslice (cadr test))))
       (else (error 'run-one "no such test: ~s" test-name))))))


;;new stuff
(add-test! '(yield-test "begin 33; 44 ; yield() end" 99))
(run-all 10)
