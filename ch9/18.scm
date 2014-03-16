(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/store.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/environments.scm")
(load-relative "./base/lang.scm")
(load-relative "./base/interp.scm")
(load-relative "./base/classes.scm")
(load-relative "./base/class-cases.scm")


;; The method environments produced by merge-method-envs can be long.
;; Write a new version of merge-method-envs with the property that each
;; method name occurs exactly once, and furthermore, it appears in the
;; same place as its earliest declaration.


;; have-method-with-name: Sym * MethodEnv -> #t/#f
(define have-method-with-name
  (lambda (name m-env)
    (let ((maybe-pair (assq name m-env)))
      (if (pair? maybe-pair) #t #f))))


;; merge-method-envs : MethodEnv * MethodEnv -> MethodEnv
(define merge-method-envs
  (lambda (super-m-env new-m-env)
    (let ((left-method-env (remove
			    (lambda(m)
			      (have-method-with-name (car m) new-m-env))
			      super-m-env)))
      (append new-m-env left-method-env))))


;; find-method : Sym * Sym -> Method
(define find-method
  (lambda (c-name name)
    (let ((m-env (class->method-env (lookup-class c-name))))
      (let ((maybe-pair (assq name m-env)))
        (if (pair? maybe-pair) (cadr maybe-pair)
            (report-method-not-found name))))))

(run "class c1 extends object
       field ivar
       method initialize() set ivar = 1
       method demo() 1

      class c2 extends c1
       method initialize() set ivar = 2
       let obj = new c2() in
         begin
             send obj demo()
         end")

(run "class c1 extends object
  field ivar1
  method initialize()set ivar1 = 1

  method setiv1(n)set ivar1 = n
  method getiv1()ivar1

  method foo() 1
  method call-foo-from-superclass() send self foo()


class c2 extends c1
  field ivar2
  method initialize()
   begin super initialize(); set ivar2 = 1 end

  method foo() 2

  method setiv2(n)set ivar2 = n
  method getiv2()ivar2

  method self-and-super-foo()
    list( send self foo(),  super foo())

  method test-self-from-super()
     super call-foo-from-superclass()


let o = new c2 ()
    t1 = 0 t2 = 0 t3 = 0 t4 = 0
in begin
       send o setiv1(33);
       list(
         send o getiv1(),
         send o self-and-super-foo(),
         send o call-foo-from-superclass(),
         send o test-self-from-super()
         )
      end")

(run-all)
