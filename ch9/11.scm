(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/store.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/environments.scm")
(load-relative "./base/lang.scm")
(load-relative "./base/interp.scm")
(load-relative "./base/classes.scm")
(load-relative "./base/class-cases.scm")

;; add private, protected, public property for method of a class
;; see new stuff


(define debug? (make-parameter #t))

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
      (arbno method-decl)
      )
     a-class-decl)

    (method-decl
     ("pri-method" identifier
      "("  (separated-list identifier ",") ")" ; method formals private
      expression
      )
     a-pri-method-decl)

    (method-decl
     ("pro-method" identifier
      "("  (separated-list identifier ",") ")" ; method formals protected
      expression
      )
     a-pro-method-decl)

    (method-decl
     ("method" identifier
      "("  (separated-list identifier ",") ")" ; method formals public
      expression
      )
     a-pub-method-decl)

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


;; new stuff
(define-datatype class class?
  (a-class
   (super-name (maybe symbol?))
   (field-names (list-of symbol?))
   (method-env-private method-environment?)
   (method-env-protected method-environment?)
   (method-env-public method-environment?)))


(define initialize-class-env!
  (lambda (c-decls)
    (set! the-class-env
          (list
           (list 'object (a-class #f '() '() '() '()))))
    (for-each initialize-class-decl! c-decls)))


(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
           (a-class (super-name field-names
				method-pri-env
				method-pro-env
				method-pub-env)
                    super-name))))

(define class->field-names
  (lambda (c-struct)
    (cases class c-struct
           (a-class (super-name field-names
				method-pri-env
				method-pro-env
				method-pub-env)
                    field-names))))


(define class->method-env
  (lambda (c-struct type)
    (cases class c-struct
           (a-class (super-name  field-names
				 method-pri-env
				 method-pro-env
				 method-pub-env)
		    (cond ((eq? type 'pri) method-pri-env)
			  ((eq? type 'pro) method-pro-env)
			  ((eq? type 'pub) method-pub-env)
			  (else
			   (error "class->method-env: %s" type)))))))


;; initialize-class-decl! : ClassDecl -> Unspecified
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
           (a-class-decl (c-name s-name f-names m-decls)
                         (let* ((f-names
                                (append-field-names
                                 (class->field-names (lookup-class s-name))
                                 f-names))
			       (all-methods (method-decls->method-env
					     m-decls s-name f-names)))
                           (add-to-class-env!
                            c-name
                            (a-class s-name f-names
				     (filter-method 'pri all-methods)
                                     (merge-method-envs
                                      (class->method-env (lookup-class s-name) 'pro)
				      (filter-method 'pro all-methods))
				     (merge-method-envs
				      (class->method-env (lookup-class s-name) 'pub)
				      (filter-method 'pub all-methods)))))))))


(define filter-method
  (lambda (type methods)
    (map
     (lambda (a-meth)
       (cdr a-meth))
     (filter (lambda (a-meth)
	       (eq? (car a-meth) type))
	     methods))))

;; method-decls->method-env :
;; Listof(MethodDecl) * ClassName * Listof(FieldName) -> MethodEnv
(define method-decls->method-env
  (lambda (m-decls super-name field-names)
    (map
     (lambda (m-decl)
       (cases method-decl m-decl
              (a-pub-method-decl (method-name vars body)
				 (list 'pub method-name
				       (a-method vars body
					     super-name field-names)))
	      (a-pri-method-decl (method-name vars body)
				 (list 'pri method-name
				       (a-method vars body
						 super-name field-names)))
	      (a-pro-method-decl (method-name vars body)
				 (list 'pro method-name
				       (a-method vars body
						 super-name field-names)))))
     m-decls)))

(define find-method
  (lambda (c-name name)
    (let ((m-env (class->method-env (lookup-class c-name) 'pub)))
      (let ((maybe-pair (assq name m-env)))
        (if (pair? maybe-pair) (cadr maybe-pair)
            (report-method-not-found name))))))


(run "class c1 extends object
  field ivar1
  method initialize() set ivar1 = 1

class c2 extends c1
  method initialize()
   begin
    set ivar1 = 1
   end
  method setiv1(n) set ivar1 = n
  method getiv1() ivar1

let o = new c2 ()
    t1 = 0
in begin
       send o setiv1(33);
       send o getiv1()
   end
")

(run "class aclass extends object
       field i
        method initialize(x) set i = x
        method m(y) -(i,-(0,y))

      let o1 = new aclass(3)
        in send o1 m(10)")


(run-all)
