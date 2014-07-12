
;; Prove by induction on n that for any g, (fib/k n g) = (g (fib n)).

;; initial step:
;; (fib/k 1 g) = (g 1)  = (g (fib 1)) stands true

;; assume (fib/k n g) = (g fib(n))

;; (fib/k n+1 g) => (g fib(n+1))

;; =>

;; (fib/k n (lambda (v1)
;;   	   (fib/k (n-1)
;;   		  (lambda (v2)
;;   		    (g (v1 + v2))))))

;; (lambda (v1)
;;   (fib/k (n-1)
;; 	 (lambda (v2)
;; 	   (g (v1 + v2))))
;;   (fib/k n))

;; (fib/k (n-1) (lambda (v2)
;; 	       (g ((fib/k n) + v2))))

;; (lambda (v2)
;;   (g ((fib/k n) + v2))
;;   (fib/k (n-1)))

;; (g ((fib/k n) + (fib/k (n - 1))))

;; (g (fibd/k (n + 1)))

  
			   


