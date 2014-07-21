(load "../libs/init.scm")

(define invert
  (lambda (list)
    (if (null? list)
	'()
	(let ((first (car list)))
	  (cons
	   (cons (cadr first) (car first))
	   (invert (cdr list)))))))


(equal?? (invert '((a 1) (a 2) (b 1) (b 2)))
         '((1 . a) (2 . a) (1 . b) (2 . b)))

(invert '((a 1)))
(equal?? (invert '((a 1))) '((1 . a)))
