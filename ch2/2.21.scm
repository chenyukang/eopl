#lang eopl

(define value?
  (lambda (v)
    #t))

(define-datatype env env?
  (empty-env-inter)
  (extend-env-inter
   (_var symbol?)
   (_val value?)
   (_env env?)))

(define empty-env
  (lambda ()
    (empty-env-inter)))

(define extend-env
  (lambda (var val E)
    (extend-env-inter var val E)))

(define apply-env
  (lambda (var E)
    (cases env E
	   (empty-env-inter ()
			    (eopl:error 'apply-env "Empty env"))
	   (extend-env-inter (_var _val _env)
			     (if (eqv? _var var)
				 _val
				 (apply-env var _env))))))

(define has-binding?
  (lambda (var E)
    (cases env E
	   (empty-env-inter () #f)
	   (extend-env-inter (_var _val _env)
			     (if (eqv? _var var)
				 #t
				 (has-binding? var _env))))))


(define e (empty-env))        ;;#(struct:empty-env-inter)
(define f (extend-env 'a 1 e));;#(struct:extend-env-inter a 1 #(struct:empty-env-inter))
(define g (extend-env 'a 2 f))
;;#(struct:extend-env-inter 
;; a
;; 2 
;; #(struct:extend-env-inter a 1 #(struct:empty-env-inter)))
(define h (extend-env 'b 3 g))
;;#(struct:extend-env-inter
;; b
;; 3
;; #(struct:extend-env-inter
;;   a
;;   2
;;   #(struct:extend-env-inter a 1 #(struct:empty-env-inter))))
(apply-env 'a h);;2
(has-binding? 'a h);;t
(has-binding? 'c h);;#f
