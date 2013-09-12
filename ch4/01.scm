(load-relative "../libs/init.scm")
(load-relative "./base/store.scm")
(load-relative "./base/explicit-lang.scm")

(run "      let g = proc (dummy)
               let counter = newref(0)
                 in begin
                    setref(counter, -(deref(counter), -1));
                    deref(counter)
                 end
      in let a = (g 11)
         in let b = (g 11)
            in -(a,b)")

;; => (num-val 0)
