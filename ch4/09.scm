
;; implement store using vector in Scheme
;; we have constant time for setref!,
;; but we have linear time for newref.

(define instrument-newref (make-parameter #f))

(define the-store 'uninitialized-store)

(define empty-store
  (lambda()
    (make-vector 0)))

(define initialize-store!
  (lambda()
    (set! the-store (empty-store))))

(define get-store
  (lambda()
    (the-store)))

(define store?
  (lambda (s)
    (cond ((eqv? s 'uninitialized-store) #t)
	  ((vector? s) #t)
	  (else
	   #f))))

(define reference?
  (lambda(x)
    (integer? x)))

;;cost linear time
(define vector-enlarge
  (lambda(store)
    (let* ((length (vector-length store))
	   (new-store (make-vector (+ length 1))))
      (do ((i 0 (+ i 1)))
	  ((= i length))
	(vector-set! new-store i
		     (vector-ref store i)))
      new-store)))

(define newref
  (lambda (val)
    (let* ((next-ref (vector-length the-store))
	   (new-store (vector-enlarge the-store)))
      (vector-set! new-store next-ref val)
      (set! the-store new-store)
      (if (instrument-newref)
	  (printf
	   "newref: allocating location ~s with initial contents ~s~%"
	   next-ref val))
      next-ref)))


(define deref
  (lambda (ref)
    (vector-ref the-store ref)))

(define setref!
  (lambda (ref val)
    (vector-set! the-store ref val)))

(define get-store-as-list
  (lambda()
    (vector->list the-store)))
