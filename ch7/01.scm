
;; 1. proc (x) -(x,3)
;; (int -> int)

;; 2. proc (f) proc (x) -((f x), 1)
;; ((t -> t) -> (t -> int))

;; 3. proc (x) x
;;    (t -> t)

;; 4. proc (x) proc (y) (x y).
;; ((t -> t) -> (t -> t))

;; 5. proc (x) (x 3)
;; (t -> t)

;; 6. proc (x) (x x)
;; (t -> t)

;; 7. proc (x) if x then 88 else 99
;; (bool -> int)

;; 8. proc (x) proc (y) if x then y else 99
;; have no type

;; 9. (proc (p) if p then 88 else 99 33)
;; int

;; 10. (proc (p) if p then 88 else 99 proc (z) z)
;; have no type

;; 11. proc (f) proc (g)
;;     proc (p)
;;       proc (x) if (p (f x)) then (g 1) else -((f x),1)
;; have no type

;; 12. proc (x) proc(p)
;;     proc (f)
;;     if (p x) then -(x,1) else (f p)
;; have no type

;; 13. proc (f)
;;   let d = proc (x)
;;     proc (z) ((f (x x)) z)
;;        in proc (n) ((f (d d)) n)
;; ((t->t) -> (t->t))
