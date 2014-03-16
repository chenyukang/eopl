(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/store.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/environments.scm")
(load-relative "./base/lang.scm")
(load-relative "./base/interp.scm")
(load-relative "./base/classes.scm")
(load-relative "./base/class-cases.scm")


(define now (make-hash-table))

(hash-table-set! now 'yukang 1)
now

(hash-table-set! now 'yilin 2)
now

(define demo
  (lambda (a-hash)
    (hash-table-for-each a-hash
			 (lambda (k v)
			   (printf "now: ~s ~s\n" k v)))))

(demo now)

;; find-method : Sym * Sym -> Method
(define find-method
  (lambda (c-name name)
    (let ((m-env (class->method-env (lookup-class c-name))))
      (if (hash-table-exists? m-env name)
	  (hash-table-ref m-env name)
	   (report-method-not-found name)))))

(define report-method-not-found
  (lambda (name)
    (eopl:error 'find-method "unknown method ~s" name)))

;; merge-method-envs : MethodEnv * MethodEnv -> MethodEnv
(define merge-method-envs
  (lambda (super-m-env new-m-env)
    (append new-m-env super-m-env)))

;; a method environment looks like ((method-name method) ...)
(define method-environment?
  hash-table?)


(define initialize-class-env!
  (lambda (c-decls)
    (set! the-class-env
          (list
           (list 'object (a-class #f '() (make-hash-table)))))
    (for-each initialize-class-decl! c-decls)))

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
                                       m-decls s-name f-names)))))))))



(run-all)
