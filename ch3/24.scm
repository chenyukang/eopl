(load-relative "../libs/init.scm")
(load-relative "../libs/environments.scm")
(load-relative "./proc-lang.scm")
;; I think this is a little difficult, see the new stuff

;; new stuff
(run "let even-iter = proc (o) proc(e) proc(num)
      if zero?(num)
          then 1
      else
          (((o o) e) -(num, 1))
      in let odd-iter = proc (o) proc(e) proc(num)
      if zero?(num)
           then 0
      else
          (((e o) e) -(num, 1))
      in let odd = proc(num) (((odd-iter odd-iter) even-iter) num)
        in let even = proc(num) (((even-iter odd-iter) even-iter) num)
            in (odd 6)")
