(load "/Users/kang/code/eopl/libs/scheme48-init.scm")

(define empty-stack
  (lambda()
    (lambda (cmd)
      (cond
       ((eqv? cmd 'top)
	(error "try top on empty stack"))
       ((eqv? cmd 'pop)
	(error "try pop on empty stack"))
       (else
	(error "unknow cmd on stack"))))))

(define push
  (lambda (saved-stack var)
    (lambda (cmd)
      (cond
       ((eqv? cmd 'top) var)
       ((eqv? cmd 'pop) saved-stack)
       (else
	(error "error cmd"))))))

(define pop
  (lambda (stack)
    (stack 'pop)))

(define top
  (lambda (stack)
    (stack 'top)))


(define e (empty-stack))
(define x1 (push e 1))
(define x2 (push x1 2))
(define x3 (push x2 3))

(equal?? (top (pop x2)) 1)
(equal?? (top x2) 2)
(equal?? (top x3) 3)
(equal?? (top (pop (pop x3))) 1)
