(load "../libs/init.scm")

(define list-index-rec
  (lambda (pred lst idx)
    (if (null? lst)
	#f
	(if (pred (car lst))
	    idx
	    (list-index-rec pred (cdr lst) (+ idx 1))))))


(define list-index
  (lambda (pred lst)
    (list-index-rec pred lst 0)))


(equal?? (list-index number? '(a 2 (1 3) b 7)) 1)
(equal?? (list-index symbol? '(a (b c) 17 foo)) 0)
(equal?? (list-index symbol? '(1 2 (a b) 3)) #f)

