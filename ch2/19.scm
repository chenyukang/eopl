(load "../libs/init.scm")

(define (number->bintree node)
	`(,node () ()))

(define (current-element lst)
	(car lst))

(define (move-to-left-son bintree)
	(cadr bintree))

(define (move-to-right-son bintree)
	(caddr bintree))

(define at-leaf? null?)

(define (as-left-branch left bintree)
  (list (car bintree) left (move-to-right-son bintree)))

(define (as-right-branch right bintree)
  (list (car bintree) (move-to-left-son bintree) right))

(define (insert-to-left node bintree)
	(cond ((at-leaf? bintree) #f)
		((pair? (move-to-left-son bintree))
			(list (car bintree) (as-left-branch (move-to-left-son bintree) (number->bintree node)) (caddr bintree)))
		(else (list (car bintree) (number->bintree node) (caddr bintree)))))

(define (insert-to-right node bintree)
	(cond ((at-leaf? bintree) #f)
		((pair? (move-to-right-son bintree))
			(list (car bintree) (cadr bintree) (as-right-branch (move-to-right-son bintree) (number->bintree node))))
		(else (list (car bintree) (cadr bintree) (number->bintree node)))))

(equal?? '(13 () ()) (number->bintree 13))
(define t1 (insert-to-right 14
				(insert-to-left 12
					(number->bintree 13))))

(equal?? '(12 () ()) (move-to-left-son t1))
(equal?? 12 (current-element (move-to-left-son t1)))
(equal?? #t (at-leaf? (move-to-right-son (move-to-left-son t1))))
(equal?? (insert-to-left 15 t1) '(13 (15 (12 () ()) ()) (14 () ())))
