(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")

;; WOW, using a case as example?

(let ((x
       (if a (p x) (p y))))
  x)

;; =>
(if a (p x) (p y))

(lambda (x y cont)
  (p x (lambda (v1)
         (cont (if a v1 (p y))))))

(lambda (a x y cont)
  (p y (lambda (v2)
         (p x (lambda (v1)
                (cont (if a v1 v2)))))))
