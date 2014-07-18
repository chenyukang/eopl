;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; list of expvals.

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (pair-val
   (car expval?)
   (cdr expval?))
  (proc-val 
   (proc proc?))
  (emptylist-val)
  (list-val
   (lst (list-of expval?))))

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

(define expval-extractor-error
  (lambda (variant value)
    (error 'expval-extractors "Looking for a ~s, found ~s"
		variant value)))

;; ;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

;; moved to interp.scm

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype proc proc?
  (procedure
   (bvar symbol?)
   (body expression?)
   (env environment?)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

;;; replaced by custom environment structure in environments.scm.
;;; This represents an environment as an alist  ((id rhs) ...)
;;; where rhs is either an expval or a list (bvar body)
;;; expval is for extend-env; the list is for extend-env-rec.

;;; this representation is designed to make the printed representation
;;; of the environment more readable.

;;; The code for this is in environments.scm, but we need environment?
;;; for define-datatype proc, so we write an appoximation:

(define environment?
  (list-of
   (lambda (p)
     (and 
      (pair? p)
      (symbol? (car p))))))


;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> environment

;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.  

(define init-env 
  (lambda ()
    (extend-env 
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

;;; represent environment as an alist  ((id rhs) ...)

;;; rhs is either an expval or a list (bvar body)
;;; expval is for extend-env; the list is for extend-env-rec.

;;; this representation is designed to make the printed representation
;;; of the environment more readable.

(define empty-env
  (lambda ()
    '()))

(define empty-env? 
  (lambda (x) (null? x)))

(define extend-env
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define extend-env-rec
  (lambda (p-name b-var p-body saved-env)
    (cons 
     (list p-name b-var p-body)
     saved-env)))

(define apply-env
  (lambda (env search-sym)
    (if (null? env) 
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let* ((binding (car env))
               (id (list-ref binding 0))
               (expval-or-bvar (list-ref binding 1)))
          (cond
	   ((not (eqv? search-sym id))
	    (apply-env (cdr env) search-sym))
	   ((not (symbol? expval-or-bvar))
            ;; this was built by extend-env
	    expval-or-bvar)
	   (else
            ;; this was built by extend-env-rec
	    (let ((bvar (cadr binding))
		  (body (caddr binding)))
	      (proc-val (procedure bvar body env)))))))))
