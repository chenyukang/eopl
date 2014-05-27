(load-relative "../../libs/init.scm")
(define value?
  (lambda (v)
    #t))

(define-datatype env env?
  (empty-env-record)
  (extend-env-record
   (_var symbol?)
   (_val value?)
   (_env env?)))
(define empty-env
  (lambda ()
    (empty-env-record)))
(define extend-env
  (lambda (var val E)
    (extend-env-record var val E)))
(define apply-env
  (lambda (var E)
    (cases env E
	   (empty-env-record ()
			     (error 'apply-env "Empty env"))
	   (extend-env-record (_var _val _env)
			      (if (eqv? _var var)
				  _val
				  (apply-env var _env))))))
  
