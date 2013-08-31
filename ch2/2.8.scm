(load "../libs/init.scm")


;; empty-env : () -> Env
(define empty-env
  (lambda() '()))

(define empty-env?
  (lambda (env)
    (if (null? env)
	#t
	#f)))

(equal?? (empty-env? (empty-env)) #t)
