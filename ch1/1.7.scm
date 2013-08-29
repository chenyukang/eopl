(load "/Users/kang/code/eopl/libs/scheme48-init.scm")

;; we add a wrapper at nth-element-rec

(define report-list-too-short
  (lambda (list nth)
    (eopl:error 'nth-element
	   "List ~s too short by ~s elements .~%"  list nth)))

(define nth-element-rec
  (lambda (lst n)
    (if (null? lst)
	#f
	(if (zero? n)
	    (car lst)
	    (nth-element-rec (cdr lst) (- n 1))))))

(define nth-element
  (lambda (lst n)
    (let ((ans (nth-element-rec lst n)))
      (if (not ans)
	  (report-list-too-short lst n)
	  ans))))
