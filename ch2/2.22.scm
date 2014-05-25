#lang eopl


(define value?
  (lambda (v)
    #t))

(define-datatype stack stack?
  (empty-stack-record)
  (push-record
   (e value?)
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
				(eopl:error 'pop "Empty stack"))
	   (push-record (e s) s) )))


(define top
  (lambda (st)
    (cases stack st
	   (empty-stack-record ()
			       (eopl:error 'top "Empty stack"))
	   (push-record (e s) e))))

(define empty-stack?
  (lambda (st)
    (cases stack st
	   (empty-stack-record () #t)
	   (push-record (e s) #f))))
	   
(define e (empty-stack));;#(struct:empty-stack-record)
(define f (push 1 e));;#(struct:push-record 1 #(struct:empty-stack-record))
(define g (push 2 f));;#(struct:push-record 2 #(struct:push-record 1 #(struct:empty-stack-record)))
(define h (push 3 g));;#(struct:push-record 3 #(struct:push-record 2 #(struct:push-record 1 #(struct:empty-stack-record))))
(define i (pop h))  ;;#(struct:push-record 2 #(struct:push-record 1 #(struct:empty-stack-record)))
(define x (top h))  ;;3
(define j (pop i))  ;;#(struct:push-record 1 #(struct:empty-stack-record))
(define k (pop j))  ;;#(struct:empty-stack-record)
(empty-stack? e)    ;;#t

