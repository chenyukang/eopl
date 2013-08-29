(load "../libs/init.scm")

(define duple
  (lambda (time val)
    (if (= time 0)
	'()
	(cons val (duple (- time 1) val)))))

(equal?? (duple 2 3) '(3 3))
(equal?? (duple 2 '(ha ha)) '((ha ha) (ha ha)))
(equal?? (duple 0 '(haha)) '())
