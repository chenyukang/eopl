
;; How many different evaluation orders are possible for the procedure
;; calls in (lambda (x y) (+ (f (g x)) (h (j y))))? For each evaluation
;; order, write a CPS expression that calls the procedures in that order.

;; 4! ?  for function f, g, h, j.

(lambda (x y)
  (+ (f (g x))
     (h (j y))))


(lambda (x y cont)
  (f (g x) (lambda (v1)
	     (+ v1 (h (j y))))))

(lambda (x y cont)
  (g x (lambda (v2)
	 (f v2 (lambda (v1)
		 (+ v1 (h (j y))))))))


(lambda (x y cont)
  (h (j y) (lambda (v3)
	     (g x (lambda (v2)
		    (f v2 (lambda (v1)
			    (+ v1 v3))))))))


(lambda (x y cont)
  (j y (lambda (v4)
	 (h v4 (lambda (v3)
		 (g x (lambda (v2)
			(f v2 (lambda (v1)
				(+ v1 v3))))))))))
;; the order is j h g f

	
