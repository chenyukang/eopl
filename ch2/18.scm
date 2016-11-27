(load "../libs/init.scm")

(define (number->sequence node)
	`(,node () ()))

(define (current-element lst)
	(car lst))

(define (at-left-end? lst)
	(null? (cadr lst)))

(define (at-right-end? lst)
	(null? (caddr lst)))

(define (move-to-left lst)
	(if (at-left-end? lst)
		(report-at-the-end-of-left)
		(list (caadr lst) (cdadr lst) (cons (car lst) (caddr lst)))))

(define (move-to-right lst)
	(if (at-right-end? lst)
		(report-at-the-end-of-right)
		(list (caaddr lst) (cons (car lst) (cadr lst)) (cdaddr lst))))

(define (report-at-the-end-of-left)
	(error 'move-to-left "Already at the end of the left."))

(define (report-at-the-end-of-right)
	(error 'move-to-right "Already at the end of the right."))

(define (insert-to-left node lst)
	(list (car lst) (cons node (cadr lst)) (caddr lst)))

(define (insert-to-right node lst)
	(list (car lst) (cadr lst) (cons node(caddr lst))))

(equal?? 6 (current-element '(6 (5 4 3 2 1) (7 8 9))))
(equal?? '(5 (4 3 2 1) (6 7 8 9)) (move-to-left '(6 (5 4 3 2 1) (7 8 9))))
(equal?? '(7 (6 5 4 3 2 1) (8 9)) (move-to-right '(6 (5 4 3 2 1) (7 8 9))))
(equal?? '(6 (13 5 4 3 2 1) (7 8 9)) (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9))))
(equal?? '(6 (5 4 3 2 1) (13 7 8 9)) (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9))))

(equal?? '(6 (5 4 3 2 1) (7 8 9)) (move-to-right (move-to-left '(6 (5 4 3 2 1) (7 8 9)))))