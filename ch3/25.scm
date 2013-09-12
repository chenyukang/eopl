(load-relative "../libs/init.scm")
(load-relative "../libs/environments.scm")

(load-relative "./base/proc-lang.scm")

(run "let makerec = proc (f)
        let d = proc (x)
          proc (z) ((f (x x)) z)
        in proc (n) ((f (d d)) n)
     in let maketimes4 = proc (f) proc (x)
          if zero?(x)
             then 0
          else -((f -(x,1)), -4)
      in let times4 = (makerec maketimes4) in (times4 3)")

;; -> (num-val 12)
