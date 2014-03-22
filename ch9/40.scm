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


;; see new stuff

;; Modify the design of the language so that every field declaration
;; contains an expression that is used to initialize the field.
;; Such a design has the advantage that a checked program will never refer
;; to an uninitialized value.


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
      (arbno "field" type identifier "=" expression)
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

    (class-decl
     ("interface" identifier (arbno abstract-method-decl))
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


;; new stuff
(define check-type!
  (lambda (ty1 ty2)
    (if (equal? ty1 ty2)
	  #t
	  (error 'check-type!
	       "Type didn't match: ~s != ~s"
	       ty1 ty2))))

;; new stuff
(define check-for-field!
  (lambda (f-types f-exps tenv)
    (letrec ((check-equal-type-list
		(lambda (l1 l2)
		  (if (null? l1)
		    (null? l2)
                    (begin
		      (check-type!
		       (car l1) (car l2))
		      (check-equal-type-list (cdr l1) (cdr l2)))))))
      (let ((f-exp-types (map (lambda(e) (type-of e tenv))
			      f-exps)))
	(check-equal-type-list f-types f-exp-types)))))

;;;;;;;;;;;;;;;; syntactic operations on types ;;;;;;;;;;;;;;;;
(define add-class-decl-to-static-class-env!
  (lambda (c-decl)
    (cases class-decl c-decl
           (an-interface-decl (i-name abs-m-decls)
                              (let ((m-tenv
                                     (abs-method-decls->method-tenv abs-m-decls)))
                                (check-no-dups! (map car m-tenv) i-name)
                                (add-static-class-binding!
                                 i-name (an-interface m-tenv))))
           (a-class-decl (c-name s-name i-names
                                 f-types f-names f-exps m-decls)
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
			   ;;new stuff
			   (check-for-field! f-types f-exps method-tenv)
                           (add-static-class-binding! c-name
                                                      (a-static-class
                                                       s-name i-names f-names f-types method-tenv)))))))


;; initialize-class-decl! : ClassDecl -> Unspecified
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
           ;; interfaces don't affect runtime
           (an-interface-decl (interface-name method-decls) '())
	   ;; new stuff
           (a-class-decl (class-name super-name interface-names field-types field-names field-exps method-decls)
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


;; check-class-decl! : ClassDecl -> Unspecified
(define check-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
           (an-interface-decl (i-name abs-method-decls)
                              #t)
           (a-class-decl (class-name super-name i-names
                                     field-types field-names field-exps method-decls)
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


(run " class c1 extends object
  field int val1 = 1
  field bool val2 = zero?(1)
  method int initialize() 1
  method int m1()1

let o1 = new c1()
in send o1 m1()")

(check " class c1 extends object
  field int val1 = 1
  field bool val2 = zero?(1) % this will report a error when val2 initialize with int
  method int initialize() 1
  method int m1() 1

let o1 = new c1()
in send o1 m1()")

;; run-all, check-all will have some fail cases
;; because modification on lang
