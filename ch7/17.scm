(load-relative "../libs/init.scm")

;; A new implementation on the SUBST

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


;; apply-one-subst : Type × Tvar × Type → Type
(define apply-one-subst
  (lambda (ty0 tvar ty1)
    (cases type ty0
           (int-type () (int-type))
           (bool-type () (bool-type))
	   (proc-type (arg-type result-type)
		      (proc-type
		       (apply-one-subst arg-type tvar ty1)
		       (apply-one-subst result-type tvar ty1)))
	   (tvar-type (sn)
		      (if (equal? ty0 tvar) ty1 ty0)))))

(define apply-subst-to-type
  (lambda (ty subst)
    (cases type ty
	   (int-type () (int-type))
	   (bool-type () (bool-type))
	   (proc-type (t1 t2)
		      (proc-type
		       (apply-subst-to-type t1 subst)
		       (apply-subst-to-type t2 subst)))
	   (tvar-type (sn)
		      (let ((tmp (assoc ty subst)))
			(if tmp
			    (cdr tmp)
			    ty))))))

(define empty-subst
  (lambda ()
    '()))


(define extend-subst
  (lambda (subst tvar ty)
    (cons
     (cons tvar ty)
     (map
      (lambda (p)
        (let ((oldlhs (car p))
              (oldrhs (cdr p)))
	  (cons
	   oldlhs
	   (apply-one-subst oldrhs tvar ty))))
      subst))))


;; new implementation
(define extend-subst
  (lambda (subst tvar ty)
    (cons (cons tvar ty) subst)))

(define apply-subst-to-type
  (lambda (ty subst)
    (cases type ty
	   (int-type () (int-type))
	   (bool-type () (bool-type))
	   (proc-type (t1 t2)
		      (proc-type
		       (apply-subst-to-type t1 subst)
		       (apply-subst-to-type t2 subst)))
	   (tvar-type (sn)
		      (let ((tmp (assoc ty subst)))
			(if tmp
			    (apply-subst-to-type (cdr tmp) subst) ;;if type is a var-type, re-appply it.
			    ty))))))


(extend-subst (empty-subst) 'var (int-type))

(extend-subst (extend-subst
	       (extend-subst (empty-subst) 'var (bool-type))
	       'xx (int-type)) 'vv (bool-type))


(assoc (tvar-type 1) (extend-subst
 (empty-subst) (tvar-type 1) (int-type)))


(extend-subst (extend-subst
	       (extend-subst (empty-subst) (tvar-type 1) (bool-type))
	       (tvar-type 2) (int-type)) (tvar-type 3) (tvar-type 1))

(extend-subst
 (extend-subst (extend-subst
		(extend-subst (empty-subst) (tvar-type 0) (bool-type))
		(tvar-type 2) (int-type))
	       (tvar-type 3) (tvar-type 1))

 (tvar-type 1) (int-type))

(apply-subst-to-type
 (tvar-type 3)
 (extend-subst
  (extend-subst (extend-subst
		 (extend-subst (empty-subst) (tvar-type 0) (bool-type))
		 (tvar-type 2) (int-type)) (tvar-type 3) (tvar-type 1))
 (tvar-type 1) (bool-type)))

;; type(v3) ==> (bool-type)


(apply-subst-to-type
 (tvar-type 1) (extend-subst (extend-subst  (extend-subst (empty-subst) (tvar-type 1) (bool-type)) 2 (int-type)) 3 (bool-type)))
