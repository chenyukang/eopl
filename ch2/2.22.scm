(load "../libs/init.scm")


(define value?
  (lambda (v)
    #t))

(define-datatype stack stack?
  (empty-stack-record)
  (push-record
   (e value?)
   (s stack?))
  (pop-record
   (s stack?)))

(define empty-stack
  (lambda ()
    (empty-stack-record)))

(define push
  (lambda (e s)
    (push-record e s)))

(define pop
  (lambda (st)
    (cases stack st
	   (empty-stack-record ()
				(error 'pop "Empty stack"))
	   (push-record (e s) s)
	   (pop-record (s) s))))


(define top
  (lambda (st)
    (cases stack st
	   (empty-stack-record ()
			       (error 'top "Empty stack"))
	   (push-record (e s) e)
	   (pop-record (s) (top s)))))

(define empty-stack?
  (lambda (st)
    (cases stack st
	   (empty-stack-record () #t)
	   (push-record (e s) #f)
	   (pop-record (s) (empty-stack? s)))))

(define e (empty-stack))
(define e (push 1 e))
(define e (push 2 e))
(define e (push 3 e))

(equal?? (top e) 3)
(define e (pop e))

(equal?? (top e) 2)

(define x (top e))
(equal?? x 2)

(define e (pop e))
(define e (pop e))
(empty-stack? e)
