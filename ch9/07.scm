(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/store.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/environments.scm")
(load-relative "./base/lang.scm")
(load-relative "./base/interp.scm")
(load-relative "./base/classes.scm")
(load-relative "./base/class-cases.scm")

;; see new stuff

;; remove the append-field-names part for field-names
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
           (a-class-decl (c-name s-name f-names m-decls)
			 (add-to-class-env!
                            c-name
                            (a-class s-name f-names
                                     (merge-method-envs
                                      (class->method-env (lookup-class s-name))
                                      (method-decls->method-env
                                       m-decls s-name f-names))))))))



(run "class c1 extends object
  field ivar1
  method initialize() set ivar1 = 1

class c2 extends c1
  field ivar2
  method initialize()
   begin
    super initialize();
    set ivar2 = 2
   end
  method setiv1(n) set ivar1 = n  %execute error
  method getiv1()  ivar1          %execute error
  method setiv2(n) set ivar2 = n
  method getiv2()  ivar2

let o = new c2 ()
    t1 = 0
in begin
       send o setiv2(33);
       send o getiv2()
   end")

;; => 33

(run-all)
