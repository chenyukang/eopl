(load-relative "../libs/init.scm")
(load-relative "./base/typed-oo/lang.scm")
(load-relative "./base/typed-oo/test.scm")
(load-relative "./base/typed-oo/store.scm")
(load-relative "./base/typed-oo/interp.scm")
(load-relative "./base/typed-oo/checker.scm")
(load-relative "./base/typed-oo/environments.scm")
(load-relative "./base/typed-oo/classes.scm")
(load-relative "./base/typed-oo/static-classes.scm")
(load-relative "./base/typed-oo/data-structures.scm")
(load-relative "./base/typed-oo/static-data-structures.scm")
(load-relative "./base/typed-oo/tests.scm")


;; add inheritance for interface.
;; see new stuff part.

(define debug? (make-parameter #f))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

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
     ("proc" "(" (separated-list identifier ":" type ",") ")" expression)
     proc-exp)

    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    (expression
     ("letrec"
      (arbno type identifier "(" (separated-list identifier ":" type ",") ")"
             "=" expression)
      "in" expression)
     letrec-exp)

    (expression
     ("begin" expression (arbno ";" expression) "end")
     begin-exp)

    (expression
     ("set" identifier "=" expression)
     assign-exp)

    ;; non-empty lists for typechecked version
    (expression
     ("list" "(" expression (arbno "," expression) ")" )
     list-exp)

    ;; new productions for oop
    (class-decl
     ("class" identifier
      "extends" identifier
      (arbno "implements" identifier)
      (arbno "field" type identifier)
      (arbno method-decl)
      )
     a-class-decl)

    (method-decl
     ("method" type identifier
      "("  (separated-list identifier  ":" type ",") ")" ; method formals
      expression
      )
     a-method-decl)

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

    ;; new productions for typed-oo
    ;; new stuff
    (class-decl
     ("interface" identifier
      (arbno "extends" identifier)
      (arbno abstract-method-decl))
     an-interface-decl)

    (abstract-method-decl
     ("method" type identifier
      "("  (separated-list identifier ":" type ",") ")" )
     an-abstract-method-decl)

    (expression
     ("cast" expression identifier)
     cast-exp)

    (expression
     ("instanceof" expression identifier)
     instanceof-exp)

    (type ("int") int-type)
    (type ("bool") bool-type)
    (type ("void") void-type)
    (type
     ("(" (separated-list type "*") "->" type ")")
     proc-type)
    (type
     ("listof" type)
     list-type)

    (type (identifier) class-type) ;; new for typed oo

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; syntactic operations on types ;;;;;;;;;;;;;;;;

;; initialize-class-decl! : ClassDecl -> Unspecified
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
           ;; interfaces don't affect runtime
	   ;; new stuff
           (an-interface-decl (interface-name parents method-decls) '())
           (a-class-decl (class-name super-name interface-names field-types field-names method-decls)
                         (let ((field-names
                                (append-field-names
                                 (class->field-names (lookup-class super-name))
                                 field-names)))
                           (add-to-class-env!
                            class-name
                            (a-class
                             super-name
                             field-names
                             (merge-method-envs
                              (class->method-env (lookup-class super-name))
                              (method-decls->method-env
                               method-decls super-name field-names)))))))))


;; new stuff
(define super-iface-tenvs
  (lambda (iface-names)
    (if (null? iface-names)
	'()
        (merge-method-tenvs (static-class->method-tenv
			     (lookup-static-class (car iface-names)))
			    (super-iface-tenvs (cdr iface-names))))))

(define add-class-decl-to-static-class-env!
  (lambda (c-decl)
    (cases class-decl c-decl
	   ;; new stuff, merge super-ifaces with current ifaces.
           (an-interface-decl (i-name super-ifaces abs-m-decls)
                              (let ((m-tenv
				     (merge-method-tenvs
				      (super-iface-tenvs super-ifaces)
				      (abs-method-decls->method-tenv abs-m-decls))))
                                (check-no-dups! (map car m-tenv) i-name)
                                (add-static-class-binding!
                                 i-name (an-interface m-tenv))))

           (a-class-decl (c-name s-name i-names
                                 f-types f-names m-decls)
                         (let ((i-names
                                (append
                                 (static-class->interface-names
                                  (lookup-static-class s-name))
                                 i-names))
                               (f-names
                                (append-field-names
                                 (static-class->field-names
                                  (lookup-static-class s-name))
                                 f-names))
                               (f-types
                                (append
                                 (static-class->field-types
                                  (lookup-static-class s-name))
                                 f-types))
                               (method-tenv
                                (let ((local-method-tenv
                                       (method-decls->method-tenv m-decls)))
                                  (check-no-dups!
                                   (map car local-method-tenv) c-name)
                                  (merge-method-tenvs
                                   (static-class->method-tenv
                                    (lookup-static-class s-name))
                                   local-method-tenv))))
                           (check-no-dups! i-names c-name)
                           (check-no-dups! f-names c-name)
                           (check-for-initialize! method-tenv c-name)
                           (add-static-class-binding! c-name
                                                      (a-static-class
                                                       s-name i-names f-names f-types method-tenv)))))))

;; check-class-decl! : ClassDecl -> Unspecified
(define check-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
	   ;;new stuff
           (an-interface-decl (i-name super-ifaces abs-method-decls)
                              #t)
           (a-class-decl (class-name super-name i-names
                                     field-types field-names method-decls)
                         (let ((sc (lookup-static-class class-name)))
                           (for-each
                            (lambda (method-decl)
                              (check-method-decl! method-decl
                                                  class-name super-name
                                                  (static-class->field-names sc)
                                                  (static-class->field-types sc)))
                            method-decls))
                         (for-each
                          (lambda (i-name)
                            (check-if-implements! class-name i-name))
                          i-names)
                         ))))




(check "interface sum_iface
          method int sum()
        interface sub_iface
          method int sub()
        interface operator extends sum_iface extends sub_iface
          method int is-zero()

        class number extends object
           implements operator
           field int value
         method void initialize(v : int)
             begin
                set value = v
             end
         method int sum()  +(value, value)
         method int sub()  -(value, value)
         method int is-zero() if zero?(value) then 1 else 0
       let obj = new number(1) in
          list(send obj sum(),
               send obj sub(),
               send obj is-zero())")

(check-all)
(run-all)
