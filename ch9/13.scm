(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/store.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/environments.scm")
(load-relative "./base/lang.scm")
(load-relative "./base/interp.scm")
(load-relative "./base/classes.scm")
(load-relative "./base/class-cases.scm")

;; add final property for method


;; grammar for the CLASSES language.  Based on IMPLICIT-REFS, plus
;; multiple-argument procedures, multiple-declaration letrecs, and
;; multiple-declaration lets.

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
     ("method" identifier
      "("  (separated-list identifier ",") ")" ; method formals
      expression
      )
     a-method-decl)

    (method-decl
     ("final" "method" identifier
      "(" (separated-list identifier ",") ")"
      expression
      )
     a-final-method-decl)

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


(define-datatype method method?
  (a-method
   (vars (list-of symbol?))
   (body expression?)
   (super-name symbol?)
   (field-names (list-of symbol?))))

;; initialize-class-env! : Listof(ClassDecl) -> Unspecified
;; Page: 344
(define initialize-class-env!
  (lambda (c-decls)
    (set! the-class-env
          (list
           (list 'object (a-class #f '() '() '()))))
    (for-each initialize-class-decl! c-decls)))

;; method-decls->method-env :
;; Listof(MethodDecl) * ClassName * Listof(FieldName) -> MethodEnv
(define method-decls->method-env
  (lambda (m-decls super-name field-names)
    (map
     (lambda (m-decl)
       (cases method-decl m-decl
              (a-method-decl (method-name vars body)
                             (list #f method-name       ;;not final
                                   (a-method vars body super-name field-names)))
	      (a-final-method-decl (method-name vars body)
				   (list #t method-name   ;;final
					 (a-method vars body super-name field-names)))))
     m-decls)))



(define in-final
  (lambda (finals)
    (lambda (method)
      (if (null? finals)
	  #f
	  (if (eq? (cadr method) (car finals))
	      #t
	      ((in-final (cdr finals)) method))))))

(define class->method-env-no-final
  (lambda (c-struct)
    (cases class c-struct
	   (a-class (super-name field-names method-env final-names)
		    (let ((methods (class->method-env c-struct)))
		      (remove (in-final final-names) methods))))))

(define collect-method
  (lambda (method)
    (cdr method)))

(define collect-name
  (lambda (method)
    (cadr method)))

(define final?
  (lambda (method)
    (car method)))

;; initialize-class-decl! : ClassDecl -> Unspecified
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
           (a-class-decl (c-name s-name f-names m-decls)
                         (let* ((f-names
                                (append-field-names
                                 (class->field-names (lookup-class s-name))
                                 f-names))
				(methods
				 (method-decls->method-env m-decls s-name f-names))
				(final-names
				 (map collect-name (filter final? methods))))
                           (add-to-class-env!
                            c-name
                            (a-class s-name f-names
                                     (merge-method-envs
                                      (class->method-env-no-final (lookup-class s-name))
				      (map collect-method methods))
				     final-names)))))))

(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
           (a-class (super-name field-names method-env finals)
                    super-name))))

(define class->field-names
  (lambda (c-struct)
    (cases class c-struct
           (a-class (super-name field-names method-env finals)
                    field-names))))

(define class->method-env
  (lambda (c-struct)
    (cases class c-struct
           (a-class (super-name  field-names method-env finals)
                    method-env))))

(run "class oddeven extends object
method initialize () 1
final method even (n)
if zero?(n) then 1 else send self odd(-(n,1))
final method odd (n)
if zero?(n) then 0 else send self even(-(n,1))
let obj = new oddeven() in
 send obj even(12)")


(run-all)
