
;; ((c3
;;  #(struct:a-class c2 (x%2 y%1 y x z)  %fields shared
;;     ((initialize #(struct:a-method ()
;;        #(struct:begin-exp ...) c2 (x%2 y%1 y x z)))
;;    (m3 #(struct:a-method ()
;;        #(struct:diff-exp ...)) c2 (x%2 y%1 y x z))
;;    (initialize #(struct:a-method ...))
;;        (m1 #(struct:a-method (u v)
;; 			     #(struct:diff-exp ...) c1 (x y%1 y)))
;;        (m3 #(struct:a-method ...))
;;    (initialize #(struct:a-method ...))  %methods shared
;;       (m1 #(struct:a-method ...))
;;          (m2 #(struct:a-method ()
;; 			       #(struct:method-call-exp
;; 				 #(struct:self-exp) m3 ()) object (x y))))))
