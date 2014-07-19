;; Determine whether each of the following expressions is simple.

;; -((f -(x, 1)), 1)
;; --> simple-exp

;; (f -(-(x,y), 1))
;; --> simple-exp

;; if zero?(x) then -(x, y) else -(-(x, y), 1)
;; --> simple-exp

;; let x = proc(y) (y x) in -(x, 3)
;; --> not simple-exp

;; let f = proc(x) x in (f 3)
;; --> simple-exp

;; 
