
(define mutpair?
  (lambda (v)
    (reference? v)))

;; make-pair : ExpVal * ExpVal -> MutPair
;; Page: 129
(define make-pair
  (lambda (val1 val2)
    (let ((ref1 (newref val1)))
      (let ((ref2 (newref val2)))
	ref1))))

;; left : MutPair -> ExpVal
;; Page: 129
(define left
  (lambda (p)
    (deref p)))

;; right : MutPair -> ExpVal
;; Page: 129
(define right
  (lambda (p)
    (deref (+ 1 p))))

;; setleft : MutPair * ExpVal -> Unspecified
;; Page: 129
(define setleft
  (lambda (p val)
    (setref! p val)))

;; setright : MutPair * Expval -> Unspecified
;; Page: 129
(define setright
  (lambda (p val)
    (setref! (+ 1 p) val)))
