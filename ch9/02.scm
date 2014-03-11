(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/store.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/environments.scm")
(load-relative "./base/lang.scm")
(load-relative "./base/interp.scm")
(load-relative "./base/classes.scm")
(load-relative "./base/class-cases.scm")

;;;(define debug? (make-parameter #t))


;;a bug oddeven

(run "class oddeven extends object
 method initialize () 1
 method even(n)
       if zero?(n) then 1 else send self odd(-(n, 1))
 method odd(n)
       if zero?(n) then 0 else send self even(-(n, 1))
 let o1 = new oddeven()
 in send o1 odd(13)")
;; => 1

;; extend even with a wrong buggy even
(run "
class oddeven extends object
 method initialize () 1
 method even(n)
       if zero?(n) then 1 else send self odd(-(n, 1))
 method odd(n)
       if zero?(n) then 0 else send self even(-(n, 1))

class bug-oddeven extends oddeven
 method initialize () 1
 method even(n)
       if zero?(n) then 0 else send self odd(-(n, 1))
 let o1 = new bug-oddeven()
 in send o1 odd(13)")
;; => 0

(run-all)
