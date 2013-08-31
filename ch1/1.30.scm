(load "../libs/init.scm")

;; useing bubble sort here


(define insert
  (lambda (lst elem pred)
    (cond ((null? lst) (list elem))
          ((pred elem (car lst))
           (cons elem lst))
          (else (cons (car lst)
                      (insert (cdr lst) elem pred))))))

(define sort-rec
  (lambda (prev now pred)
    (if (null? now)
        prev
        (sort-rec (insert prev (car now) pred)
                  (cdr now)
		  pred))))

(define sort/predicate
  (lambda (pred lst)
    (sort-rec '() lst pred)))


(equal?? (sort/predicate < '(8 2 5 2 3)) '(2 2 3 5 8))
(equal?? (sort/predicate > '(8 2 5 2 3)) '(8 5 3 2 2))

(equal?? (sort/predicate < '()) '())
(equal?? (sort/predicate < '(1 2 3 4)) '(1 2 3 4))
(equal?? (sort/predicate < '(4 3 2 1)) '(1 2 3 4))
(equal?? (sort/predicate < '(8 2 5 2 3)) '(2 2 3 5 8))
