(load "../libs/init.scm")

;; strategy 1, use pair in list
;; as excersie 2.5
;; ((d . 6) (y . 8) (x . 7) (y . 14))
;; emtpy env is '()


;; strategy 2, use seperate list for key and vals
;; ((d y x y) (6 8 7 14)
;; empty env is '( () () )

;; stratege 3, use pairs in list, but based on key
;; ((d 6) (y 8 14) (x 7))
;; empty env is '()
