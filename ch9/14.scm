(load-relative "../libs/init.scm")
(load-relative "./base/classes/test.scm")
(load-relative "./base/classes/store.scm")
(load-relative "./base/classes/data-structures.scm")
(load-relative "./base/classes/environments.scm")
(load-relative "./base/classes/lang.scm")
(load-relative "./base/classes/interp.scm")
(load-relative "./base/classes/classes.scm")
(load-relative "./base/classes/class-cases.scm")

;; use static dispatch when call a method with self.
;; add self-name to method for recording current host class name
;; use this to defend malicious subclass.

(define debug? (make-parameter #t))

(define-datatype method method?
  (a-method
   (vars (list-of symbol?))
   (body expression?)
   (self-name symbol?)  ;; add self-name
   (super-name symbol?)
   (field-names (list-of symbol?))))

(define-datatype environment environment?
  (empty-env)
  (extend-env
   (bvars (list-of symbol?))
   (bvals (list-of reference?))
   (saved-env environment?))
  (extend-env-rec**
   (proc-names (list-of symbol?))
   (b-varss (list-of (list-of symbol?)))
   (proc-bodies (list-of expression?))
   (saved-env environment?))
  (extend-env-with-self-and-super
   (self object?)
   (self-class symbol?)  ;; add self-class
   (super-name symbol?)
   (saved-env environment?)))

(define method-decls->method-env
  (lambda (m-decls self-name super-name field-names)
    (map
     (lambda (m-decl)
       (cases method-decl m-decl
              (a-method-decl (method-name vars body)
                             (list method-name
                                   (a-method vars body self-name super-name field-names)))))
     m-decls)))


;; initialize-class-decl! : ClassDecl -> Unspecified
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
           (a-class-decl (c-name s-name f-names m-decls)
                         (let ((f-names
                                (append-field-names
                                 (class->field-names (lookup-class s-name))
                                 f-names)))
                           (add-to-class-env!
                            c-name
                            (a-class s-name f-names
                                     (merge-method-envs
                                      (class->method-env (lookup-class s-name))
                                      (method-decls->method-env
                                       m-decls c-name s-name f-names)))))))))


;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;
(define apply-env
  (lambda (env search-sym)
    (cases environment env
           (empty-env ()
                      (error 'apply-env "No binding for ~s" search-sym))
           (extend-env (bvars bvals saved-env)
                       (cond
                        ((location search-sym bvars)
                         => (lambda (n)
                              (list-ref bvals n)))
                        (else
                         (apply-env saved-env search-sym))))
           (extend-env-rec** (p-names b-varss p-bodies saved-env)
                             (cond
                              ((location search-sym p-names)
                               => (lambda (n)
                                    (newref
                                     (proc-val
                                      (procedure
                                       (list-ref b-varss n)
                                       (list-ref p-bodies n)
                                       env)))))
                              (else (apply-env saved-env search-sym))))
           (extend-env-with-self-and-super (self class-name super-name saved-env)
                                           (case search-sym
                                             ((%self) self)
                                             ((%super) super-name)
                                             ((%self-class) class-name)
                                             (else (apply-env saved-env search-sym)))))))


;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp

           (const-exp (num) (num-val num))

           (var-exp (var) (deref (apply-env env var)))

           (diff-exp (exp1 exp2)
                     (let ((val1
                            (expval->num
                             (value-of exp1 env)))
                           (val2
                            (expval->num
                             (value-of exp2 env))))
                       (num-val
                        (- val1 val2))))

           (sum-exp (exp1 exp2)
                    (let ((val1
                           (expval->num
                            (value-of exp1 env)))
                          (val2
                           (expval->num
                            (value-of exp2 env))))
                      (num-val
                       (+ val1 val2))))

           (zero?-exp (exp1)
                      (let ((val1 (expval->num (value-of exp1 env))))
                        (if (zero? val1)
                            (bool-val #t)
                            (bool-val #f))))

           (if-exp (exp0 exp1 exp2)
                   (if (expval->bool (value-of exp0 env))
                       (value-of exp1 env)
                       (value-of exp2 env)))

           (let-exp (vars exps body)
                    (if (instrument-let)
                        (printf "entering let ~s~%" vars))
                    (let ((new-env
                           (extend-env
                            vars
                            (map newref (values-of-exps exps env))
                            env)))
                      (if (instrument-let)
                          (begin
                            (printf "entering body of let ~s with env =~%" vars)
                            (pretty-print (env->list new-env))
                            (printf "store =~%")
                            (pretty-print (store->readable (get-store-as-list)))
                            (printf "~%")
                            ))
                      (value-of body new-env)))

           (proc-exp (bvars body)
                     (proc-val
                      (procedure bvars body env)))

           (call-exp (rator rands)
                     (let ((proc (expval->proc (value-of rator env)))
                           (args (values-of-exps rands env)))
                       (apply-procedure proc args)))

           (letrec-exp (p-names b-varss p-bodies letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec** p-names b-varss p-bodies env)))

           (begin-exp (exp1 exps)
                      (letrec
                          ((value-of-begins
                            (lambda (e1 es)
                              (let ((v1 (value-of e1 env)))
                                (if (null? es)
                                    v1
                                    (value-of-begins (car es) (cdr es)))))))
                        (value-of-begins exp1 exps)))

           (assign-exp (x e)
                       (begin
                         (setref!
                          (apply-env env x)
                          (value-of e env))
                         (num-val 27)))


           (list-exp (exps)
                     (list-val
                      (values-of-exps exps env)))

           ;; new cases for CLASSES language
           (new-object-exp (class-name rands)
                           (let ((args (values-of-exps rands env))
                                 (obj (new-object class-name)))
                             (apply-method
                              (find-method class-name 'initialize)
                              obj
                              (object->class-name obj)
                              args)
                             obj))

           (self-exp ()
                     (apply-env env '%self))

           (method-call-exp (obj-exp method-name rands)
                            (let ((args (values-of-exps rands env))
                                  (obj (value-of obj-exp env)))
                              (if (eq? (car obj-exp) 'self-exp)
                                  (let ((cur-class (apply-env env '%self-class)))
                                    (apply-method
                                     (find-method cur-class method-name)
                                     obj
                                     (object->class-name obj)
                                     args))
                                  (apply-method
                                   (find-method (object->class-name obj) method-name)
                                   obj
                                   (object->class-name obj)
                                   args))))

           (super-call-exp (method-name rands)
                           (let ((args (values-of-exps rands env))
                                 (obj (apply-env env '%self)))
                             (apply-method
                              (find-method (apply-env env '%super) method-name)
                              obj
                              (object->class-name obj)
                              args)))
           )))


;; apply-method : Method * Obj * Listof(ExpVal) -> ExpVal
(define apply-method
  (lambda (m self self-class args)
    (cases method m
        (a-method (vars body self-name super-name field-names)
          (value-of body
            (extend-env vars (map newref args)
              (extend-env-with-self-and-super
                self self-name super-name
                (extend-env field-names (object->fields self)
                  (empty-env)))))))))


(run "class oddeven extends object
method initialize () 1
method even (n)
if zero?(n) then 1 else send self odd(-(n,1))
method odd (n)
if zero?(n) then 0 else send self even(-(n,1))
let obj = new oddeven() in
 send obj odd(13)")

;; => 1

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

;; => 1

(run-all)
;; some cases will failed for this changes
