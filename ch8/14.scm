(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/type-structures.scm")
(load-relative "./base/type-module.scm")
(load-relative "./base/grammar.scm")
(load-relative "./base/renaming.scm")
(load-relative "./base/subtyping.scm")
(load-relative "./base/expand-type.scm")
(load-relative "./base/type-cases.scm")


(run "module mybool
interface
[opaque t
        true : t
        false : t
        and : (t -> (t -> t))
        not : (t -> t)
        to-bool : (t -> bool)]
body
[type t = int
      true = 0
      false = 13
      and = proc (x : t)
      proc (y : t)
      if zero?(x) then y else false
      not = proc (x : t)
      if zero?(x) then false else true
      to-bool = proc (x : t) zero?(x)]
let true = from mybool take true
in let false = from mybool take false in let and = from mybool take and
in ((and true) false)")

;; => (num-val 13)

(run "module mybool
interface
[opaque t
        true : t
        false : t
        and : (t -> (t -> t))
        not : (t -> t)
        to-bool : (t -> bool)]
body
[type t = int
      true = 1
      false = 0
      and = proc (x : t)
      proc (y : t)
      if zero?(x) then false else y
      not = proc (x : t)
      if zero?(x) then true else false
      to-bool = proc (x : t)
      if zero?(x) then zero?(1) else zero?(0)]

let true = from mybool take true
in let false = from mybool take false in let and = from mybool take and
in ((and true) false)")

;; => (num-val 0)
