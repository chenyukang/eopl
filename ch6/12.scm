;;Determine whether each of the following expressions is simple.

;; -((f -(x, 1)), 1)  --> simple-exp
;; (f -(-(x,y),1))     --> simple-exp
;; if zero?(x) then -(x,y) else -(-(x,y),1) 4. letx=proc(y)(yx)in-(x,3)  --> not simple-exp
;; letf=proc(x)xin(f3) --> not simple-exp
