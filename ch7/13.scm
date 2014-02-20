
;; let x = exp in let-body
;; bound t_x = t_exp in let-body

;; let x = 4 in (x 3)    error, x int? or a proc?

;; let f = proc(z) z in proc(x) -((f x), 1)
;; proc(x)  type_x -> int
;; type(exp)  type_x -> (type_x -> int)

;; let p = zero?(x) in if p then 88 else 99
;; type(x) = int
;; type(exp) = int

;; let p = proc(z) z in if p then 88 else 99
;; type(z) = type_x
;; type(p) = bool
;; type(exp) = int
