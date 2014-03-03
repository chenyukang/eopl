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


(run "module tables
interface
[opaque table
        empty : table
        add-to-table : (int -> (int -> (table -> table)))
        lookup-in-table : (int -> (table -> int))]
body
[type table = (int -> int)
      ...]
let empty = from tables take empty
in let add-binding = from tables take add-to-table
in let lookup = from tables take lookup-in-table
in let table1 = (((add-binding 3) 300)
                 (((add-binding 4) 400)
                  (((add-binding 3) 600)
                   empty)))
in -(((lookup 4) table1),
     ((lookup 3) table1))")
