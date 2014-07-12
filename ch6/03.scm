;;rewrite each of these in continuation-passing style

;; 1. (lambda (x y) (p (+ 8 x) (q y)))
;; ==>
(lambda (x y cont) (q y (lambda(v)
			  (p v (cont (+ 8 x cont))))))

;;2. (lambda (x y u v) (+ 1 (f (g x y) (+ u v))))
;; ==>

(lambda (x y u v cont)
  (f ((lambda (v1, v2)
	(cont (+ 1 v1 v2)))
      (g x y)
      (+ u v))))

(lambda (x y u v cont)
  (g x y (lambda (v1)
	   (f ((lambda (v1, v2)
		 (cont (+ 1 v1 v2)))
	       v1)))
	       (+ u v)))


(lambda (x y u v cont)
  (g x y (f ((lambda (v1, v2)
                 (cont (+ 1 v1 v2)))
	     (+ u v)))))


(lambda (x y u v cont)
  (g x y (f ((lambda (v1)
               (cont (+ 1 v1 (+ u v))))))))

;; 3. (+ 1 (f (g x y) (+ u (h v))))
;; =>
(lambda (x y v cont)
  (lambda (v1 v2)
    (cont (+ 1 v1 v2)))
  (f (g x y))
  (+ u (h v)))

(lambda (x y v cont)
  (f (g x y) ((lambda (v1)
	       (lambda (v2)
		 (cont (+ 1 v1 v2))))
	      (+ u (h v)))))


(lambda (x y v cont)
  (f (g x y) ((lambda (v1)
                (lambda (v2)
                  (cont (+ 1 v1 v2))))
              (+ u (h v)))))

(lambda (x y v cont)
  (g x y (lambda (v3)
	   (f v3 ((lambda (v1)
		   (lambda (v2)
		     (cont (+ 1 v1 v2)))))
	      (+ u (h v))))))

(lambda (x y v cont)
  (h v ((lambda (v4)
	  (+ u v4))
	(g x y (lambda (v3)
		 (f v3 ((lambda (v1)
			  (lambda (v2)
			    (cont (+ 1 v1 v2)))))))))))



		      
;; 4. (zero? (if a (p x) (p y)))

(lambda (a x y cont)
  ((lambda (v)
    (cont (zero? v)))
    (if a (p x) (p y))))

(lambda (a x y cont)
  (if a 
      (p x (lambda (v)
	     (cont (zero? v))))
      (p y (lambda (v)
	     (cont (zero? v))))))

;; 5. (zero? (if (f a) (p x) (p y)))

(lambda (a x y cont)
  ((lambda (v)
     (cont (zero? v)))
   (if (f a) (p x) (p y))))

(lambda (a x y cont)
  (f a (lambda (v2)
	 ((lambda (v)
	    (cont (zero? v)))
	  (if v2 (p x) (p y))))))

(lambda (a x y cont)
  (p x (lambda (v3)
	 (f a (lambda (v2)
		((lambda (v)
		   (cont (zero? v)))
		 (if v2 v3 (p y))))))))

(lambda (a x y cont)
  (p y (lambda (v4)
	 (p x (lambda (v3)
		(f a (lambda (v2)
		       (if v2 v3 v4
			   (lambda (v1)
			     (cont (zero? v1)))))))))))

;; 6. (let ((x (let ((y 8)) (p y)))) x)
(let ((x
       (let ((y 8))
	 (p y))))
  x)

(p 8)


;; 7. (let ((x (if a (p x) (p y)))) x)
(let ((x
       (if a (p x) (p y))))
  x)

;; =>
(if a (p x) (p y))

(lambda (x y cont)
  (p x (lambda (v1)
	 (cont (if a v1 (p y))))))

(lambda (a x y cont)
  (p y (lambda (v2)
	 (p x (lambda (v1)
		(cont (if a v1 v2)))))))


	 
  
    
