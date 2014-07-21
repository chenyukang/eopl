(load "/Users/kang/code/eopl/libs/scheme48-init.scm")
;; Implement for bigint numbers
(define base 16)
(define bzero '())
(define bzero? null?)

(define succ
  (lambda (n)
    (if (bzero? n)
	'(1)
	(let ((t (+ (car n) 1)))
	  (if (= t base)
	      (cons 0 (succ (cdr n)))
	      (cons t (cdr n)))))))

(define pred
  (lambda (n)
    (cond
     ((bzero? n) #f)
     ((>= (car n) base) #f)
     ((equal? n '(1)) '())
     ((zero? (car n))
      (if (null? (cdr n))
	  #f
	  (cons (- base 1) (pred (cdr n)))))
      (else (cons (- (car n) 1) (cdr n))))))

;; use mult for better performance
(define make
  (lambda (n)
    (if (zero? n)
	'()
	(if (odd? n)
	    (succ (make (- n 1)))
	    (mult (make (/ n 2)) '(2))))))

(define plus
  (lambda (a b)
    (if (bzero? b)
	a
	(plus (succ a) (pred b)))))


(define bone?
  (lambda (n)
    (bzero? (pred n))))

(define mult
  (lambda (a b)
    (cond
     ((bzero? b) '())
     ((bone? b) a)
     (else
      (plus a (mult a (pred b)))))))

(define fact
  (lambda (n)
    (cond
     ((bzero? n) '())
     ((bone? n) '(1))
     (else
      (mult n (fact (pred n)))))))

(define fact-number
  (lambda (n)
    (cond
     ((zero? n) 0)
     ((= n 1) 1)
     (else
      (* n (fact-number (- n 1)))))))

(equal?? (make 10) '(10))
(equal?? (make 16) '(0 1))
(make 5040)
(equal?? (fact (make 7)) (make 5040))

;;fact is much slower than fact-number
