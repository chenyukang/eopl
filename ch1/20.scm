(load "../libs/init.scm")

(define count-occurs-rec
  (lambda (val list cnt)
    (if (null? list)
	cnt
	(if (list? (car list))
	    (count-occurs-rec val (cdr list)
			      (+ (count-occurs-rec val (car list) 0) cnt))
	    (if (equal? (car list) val)
		(count-occurs-rec val (cdr list) (+ cnt 1))
		(count-occurs-rec val (cdr list) cnt))))))


(define count-occurs
  (lambda (val list)
    (count-occurs-rec val list 0)))



(equal?? (count-occurs 'x '((f x) y (((x z) x)))) 3)

(equal?? (count-occurs 'x '((f x) y (((x z) () x)))) 3)
(equal?? (count-occurs 'w '((f x) y (((x z) x)))) 0)
