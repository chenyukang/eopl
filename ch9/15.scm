(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/store.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/environments.scm")
(load-relative "./base/lang.scm")
(load-relative "./base/interp.scm")
(load-relative "./base/classes.scm")
(load-relative "./base/class-cases.scm")

;; add static variable for CLASS
(define static-field?
  (list-of
   (lambda (p)
     (and
      (pair? p)
      (symbol? (car p))
      (expression? (cadr p))))))

(define-datatype class class?
  (a-class
   (super-name (maybe symbol?))
   (field-names (list-of symbol?))
   (static-names static-field?)
   (method-env method-environment?)))

(define initialize-class-env!
  (lambda (c-decls)
    (set! the-class-env
          (list
           (list 'object (a-class #f '() '() '()))))
    (for-each initialize-class-decl! c-decls)))

;; initialize-class-decl! : ClassDecl -> Unspecified
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
           (a-class-decl (c-name s-name f-names f-static-names m-decls)
                         (let ((f-names
                                (append-field-names
                                 (class->field-names (lookup-class s-name))
                                 f-names)))
                           (add-to-class-env!
                            c-name
                            (a-class s-name f-names
                                     f-static-names
                                     (merge-method-envs
                                      (class->method-env (lookup-class s-name))
                                      (method-decls->method-env
                                       m-decls s-name f-names)))))))))


(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
           (a-class (super-name field-names static-fields method-env)
                    super-name))))

(define class->field-names
  (lambda (c-struct)
    (cases class c-struct
           (a-class (super-name field-names static-field? method-env)
                    field-names))))

(define class->method-env
  (lambda (c-struct)
    (cases class c-struct
           (a-class (super-name  field-names static-field? method-env)
                    method-env))))

(define the-grammar
  '((program ((arbno class-decl) expression) a-program)

    (expression (number) const-exp)
    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("+" "(" expression "," expression ")")
     sum-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" (arbno identifier "=" expression) "in" expression)
     let-exp)

    (expression
     ("proc" "(" (separated-list identifier ",") ")" expression)
     proc-exp)

    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    (expression
     ("letrec"
      (arbno identifier "(" (separated-list identifier ",") ")"
             "=" expression)
      "in" expression)
     letrec-exp)

    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)

    (expression
     ("set" identifier "=" expression)
     assign-exp)

    (expression
     ("list" "(" (separated-list expression ",") ")" )
     list-exp)

    ;; new productions for oop
    (class-decl
     ("class" identifier
      "extends" identifier
      (arbno "field" identifier)
      (arbno static-field-decl)
      (arbno method-decl)
      )
     a-class-decl)

    (method-decl
     ("method" identifier
      "("  (separated-list identifier ",") ")" ; method formals
      expression
      )
     a-method-decl)

    (static-field-decl
     ("static" identifier "=" expression)
      static-field-exp)

    (expression
     ("new" identifier "(" (separated-list expression ",") ")")
     new-object-exp)

    ;; this is special-cased to prevent it from mutation
    (expression
     ("self")
     self-exp)

    (expression
     ("send" expression identifier
      "("  (separated-list expression ",") ")")
     method-call-exp)

    (expression
     ("super" identifier    "("  (separated-list expression ",") ")")
     super-call-exp)

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

(run "class c1 extends object  3")

(run-all)
