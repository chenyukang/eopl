#lang eopl

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

(define make-prefix-exp
  (lambda (lst)
    (if (null? lst)
	'()
    (if (number? (car lst))
	(cons (const-exp (car lst))
	      (cdr lst))
	(when (eqv? (car lst) '-)
	    (if (null? (cdr lst))
		(eopl:error 'make-prefix-exp "need operand")
		(let* ((next (make-prefix-exp (cdr lst)))
		       (op1 (car next))
		       (next (make-prefix-exp (cdr next)))
		       (op2 (car next))
		       (rest (cdr next)))
		  (cons (diff-exp op1 op2)
			rest))))))))

(define make-prefix
  (lambda (prog)
    (car (make-prefix-exp prog))))

(make-prefix '(1));;#(struct:const-exp 1)
(make-prefix '(- - 3 2 - 4 - 12 7))
;;#(struct:diff-exp
;;  #(struct:diff-exp #(struct:const-exp 3) #(struct:const-exp 2))
;;  #(struct:diff-exp #(struct:const-exp 4) #(struct:diff-exp #(struct:const-exp 12) #(struct:const-exp 7))))

;;(make-prefix '(-)) -> error
