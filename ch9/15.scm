(load-relative "../libs/init.scm")
(load-relative "./base/classes/test.scm")
(load-relative "./base/classes/store.scm")
(load-relative "./base/classes/data-structures.scm")
(load-relative "./base/classes/environments.scm")
(load-relative "./base/classes/lang.scm")
(load-relative "./base/classes/interp.scm")
(load-relative "./base/classes/classes.scm")
(load-relative "./base/classes/class-cases.scm")

;; add static variable for CLASS
;; eval the static field part when initilize the class

(define-datatype method method?
  (a-method
   (vars (list-of symbol?))
   (body expression?)
   (class-name symbol?)
   (super-name symbol?)
   (field-names (list-of symbol?))))


(define static-field?
  (list-of
   (lambda (p)
     (and
      (pair? p)
      (symbol? (car p))
      (reference? (cdr p))))))

(define-datatype class class?
  (a-class
   (super-name (maybe symbol?))
   (static-names static-field?)
   (field-names (list-of symbol?))
   (method-env method-environment?)))

(define value-of-static-field
  (lambda (static)
    (let ((name (cadr static))
	  (ref  (newref
		 (value-of (caddr static) (init-env)))))
      (cons name ref))))

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
           (a-class-decl (c-name s-name f-static-names f-names m-decls)
                         (let ((f-names
                                (append-field-names
                                 (class->field-names (lookup-class s-name))
                                 f-names)))
                           (add-to-class-env!
                            c-name
                            (a-class s-name
				     (map value-of-static-field
					  f-static-names)
				     f-names
                                     (merge-method-envs
                                      (class->method-env (lookup-class s-name))
                                      (method-decls->method-env
                                       m-decls c-name s-name f-names)))))))))


(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
           (a-class (super-name static-field field-names method-env)
                    super-name))))

(define class->field-names
  (lambda (c-struct)
    (cases class c-struct
           (a-class (super-name static-field field-names method-env)
                    field-names))))

(define class->method-env
  (lambda (c-struct)
    (cases class c-struct
           (a-class (super-name  static-field field-names method-env)
                    method-env))))

(define class->static-fields
  (lambda (c-struct)
    (cases class c-struct
	   (a-class (super-name static-field field-names method-env)
		    static-field))))

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
      (arbno static-field-decl)
      (arbno "field" identifier)
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


;; method-decls->method-env :
;; Listof(MethodDecl) * ClassName * Listof(FieldName) -> MethodEnv
(define method-decls->method-env
  (lambda (m-decls class-name super-name field-names)
    (map
     (lambda (m-decl)
       (cases method-decl m-decl
	      (a-method-decl (method-name vars body)
			     (list method-name
				   (a-method vars body class-name super-name field-names)))))
     m-decls)))


(define extend-env-with-static
  (lambda (class-name env)
    (let ((static-fields (class->static-fields (lookup-class class-name))))
      (extend-env
       (map (lambda(x)
	      (car x)) static-fields)
       (map (lambda(x)
	      (cdr x)) static-fields)
       env))))

;; apply-method : Method * Obj * Listof(ExpVal) -> ExpVal
(define apply-method
  (lambda (m self args)
    (cases method m
        (a-method (vars body class-name super-name field-names)
          (value-of body
		    (extend-env vars (map newref args)
				(extend-env-with-static class-name
              (extend-env-with-self-and-super
                self super-name
                (extend-env field-names (object->fields self)
                  (empty-env))))))))))


  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

(run "class c1 extends object  3")

(run "class c1 extends object
static next-serial-number = 1
field my-serial-number
method get-serial-number () my-serial-number
method initialize ()
     begin
      set my-serial-number = next-serial-number;
      set next-serial-number = +(next-serial-number,1)
    end
      let o1 = new c1()
          o2 = new c1()
      in list(send o1 get-serial-number(),
              send o2 get-serial-number())")

;; -> (1 2)
(run-all)
