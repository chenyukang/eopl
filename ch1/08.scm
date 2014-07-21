(load "../libs/init.scm")

;; this will drop the elements before the first key element

(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
	    (cdr los)
	    (remove-first s (cdr los))))))


;;(remove-first 'c '(b c d e))
;;(remove-first 'c '(c d e))

(equal?? (remove-first 'c '(b c d e)) '(d e))

;; when first emlemnt is the key element, it's OK
(equal?? (remove-first 'c '(c d e)) '(d e))
