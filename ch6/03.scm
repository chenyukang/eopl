;;rewrite each of these in continuation-passing style

;; 1. (lambda (x y) (p ( + 8 x) (q y)))
;; ==>
(lambda (x y cont) (q y (lambda(v)
			  (p v (cont (+ 8 x cont))))))

;;2. (lambda(x y u v) (+ 1 (f (g x y) (+ u v))))
;; ==>
