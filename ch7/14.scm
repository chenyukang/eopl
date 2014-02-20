
;; What is wrong with this expression?
;; letrec
;;     ? even(odd : ?) =
;;         proc (x : ?)
;;    if zero?(x) then 1 else (odd -(x,1))
;;         in letrec
;;    ? odd(x : bool) =
;;       if zero?(x) then 0 else ((even odd) -(x,1))
;;      in (odd 13)

;; type(x) is bool but used as int in odd(x : bool)
