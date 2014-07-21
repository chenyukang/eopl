(load "../libs/init.scm")

(define merge
  (lambda (loi1 loi2)
    (cond ((and (null? loi1) (not (null? loi2)))
	   loi2)
	  ((and (null? loi2) (not (null? loi1)))
	   loi1)
	  (else (let ((first1 (car loi1))
		      (first2 (car loi2)))
		  (if (< first1 first2)
		      (cons first1 (merge (cdr loi1) loi2))
		      (cons first2 (merge loi1 (cdr loi2)))))))))


(equal?? (merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
(equal?? (merge '(35 62 81 90 91) '(3 83 85 90))
	 '(3 35 62 81 83 85 90 90 91))
