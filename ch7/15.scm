

;; Write down a rule for doing type inference for a letrec expression.
;; Your rule should handle multiple declarations in a letrec. Using your rule,
;; derive types for each of the following expressions, or determine that no such type
;; exists:
;; 1. letrec ? f (x : ?)
;;        = if zero?(x) then 0 else -((f -(x,1)), -2)
;;     in f

;; type(f) = (int -> int)

;; 2. letrec ? even (x : ?)
;;     = if zero?(x) then 1 else (odd -(x,1))
;;      ? odd (x : ?)
;;       = if zero?(x) then 0 else (even -(x,1))
;;       in (odd 13)

;; type(even) = (int -> int)
;; type(odd)  = (int -> int)
;; type(exp)  = int

;; 3. letrec ? even (odd : ?)
;;      = proc (x) if zero?(x)
;;      then 1
;;      else (odd -(x,1))
;;     in letrec ? odd (x : ?) =
;;       if zero?(x)
;;       then 0
;;      else ((even odd) -(x,1))
;;    in (odd 13)

;; type(even) = (int -> int)
;; type(odd)  = (int -> (int -> int))
;; type(exp)  = int
