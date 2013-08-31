(load "../libs/init.scm")

;;emtpy-stack? and top is observers

(define empty-stack
  (lambda ()
    '()))

(define empty-stack?
  (lambda (stack)
    (if (equal? stack '())
	#t
	#f)))

(define push
  (lambda (stack val)
    (cons val stack)))

(define pop
  (lambda (stack)
    (cdr stack)))

(define top
  (lambda (stack)
    (car stack)))
