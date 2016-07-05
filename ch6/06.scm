
;; How many different evaluation orders are possible for the procedure
;; calls in (lambda (x y) (+ (f (g x)) (h (j y))))? For each evaluation
;; order, write a CPS expression that calls the procedures in that order.

;; 4! ?  for function f, g, h, j.

(lambda (x y)
  (+ (f (g x))
     (h (j y))))

;; g -> j -> f -> h -> +
(lambda (x y cont)
  (g x (lambda (val)
	 (j y (lambda (val2)
		(f val (lambda (val3)
			 (h val2 (lambda (val4)
				   (cont (+ val3 val4)))))))))))

;; j -> g -> f -> h
(lambda (x y cont)
  (j y (lambda (val)
	 (g x (lambda (val2)
		(f val2 (lambda (val3)
			  (h val (lambda (val4)
				   (cont (+ val3 val4)))))))))))

;; g -> j -> h -> f
(lambda (x y cont)
  (g x (lambda (val)
	 (j y (lambda (val2)
		(h val2 (lambda (val3)
			  (f val (lambda (val4)
				   (cont (+ val4 val3)))))))))))

;; j -> g -> h -> f
(lambda (x y cont)
  (j y (lambda (val)
	 (g x (lambda (val2)
		(h val (lambda (val3)
			 (f val2 (lambda (val4)
				   (cont (+ val3 val4)))))))))))

;; g -> f -> j -> h
(lambda (x y cont)
  (g x (lambda (val)
	 (f val (lambda (val2)
		  (j y (lambda (val3)
			 (h val3 (lambda (val4)
				   (cont (+ val2 val4)))))))))))

;; j -> h -> g -> f
(lambda (x y cont)
  (j y (lambda (val)
	 (h val (lambda (val2)
		  (g x (lambda (val3)
			 (f val3 (lambda (val4)
				   (cont (+ val4 val2)))))))))))
