(load-relative "../libs/init.scm")
(load-relative "./base/classes/test.scm")
(load-relative "./base/classes/store.scm")
(load-relative "./base/classes/data-structures.scm")
(load-relative "./base/classes/environments.scm")
(load-relative "./base/classes/lang.scm")
(load-relative "./base/classes/interp.scm")
(load-relative "./base/classes/classes.scm")
(load-relative "./base/classes/class-cases.scm")

;; add final property for method
;; when extend final method, I will not report a error,
;; but just ignore it, and when call this method, It will finally call the
;; base method.

(define debug? (make-parameter #t))

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

(define-datatype class class?
  (a-class
   (super-name (maybe symbol?))
   (field-names (list-of symbol?))
   (method-env method-environment?)
   (final-names (list-of symbol?))))


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



(define collect-method
  (lambda (method)
    (cdr method)))

(define collect-name
  (lambda (method)
    (cadr method)))

(define final?
  (lambda (method)
    (car method)))


(define in-final?
  (lambda (finals)
    (lambda (method)
      (if (null? finals)
	  #f
	  (if (eq? (cadr method) (car finals))
	      #t
	      ((in-final? (cdr finals)) method))))))

(define class->method-env-no-final
  (lambda (c-struct)
    (cases class c-struct
	   (a-class (super-name field-names method-env final-names)
		    (let ((methods (class->method-env c-struct)))
		      (remove (in-final final-names) methods))))))


(define merge-method-envs
  (lambda (super-m-env new-m-env finals)
    (let* ((not-finals (remove (in-final? finals) new-m-env)))
      (append (map collect-method not-finals) super-m-env))))


;; initialize-class-decl! : ClassDecl -> Unspecified
(define initialize-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
           (a-class-decl (c-name s-name f-names m-decls)
                         (let* ((super-class (lookup-class s-name))
                                (f-names
                                (append-field-names
                                 (class->field-names super-class)
                                 f-names))
                                (methods
                                 (method-decls->method-env m-decls s-name f-names))
                                (final-names
                                 (map collect-name (filter final? methods))))
                           (add-to-class-env!
                            c-name
                            (a-class s-name f-names
                                     (merge-method-envs
                                      (class->method-env super-class)
                                      methods
                                      (class->final-names super-class))
                                     final-names)))))))

(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
           (a-class (super-name field-names method-env finals)
                    super-name))))

(define class->final-names
  (lambda (c-struct)
    (cases class c-struct
           (a-class (super-name field-names method-env finals)
                    finals))))

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
 send obj odd(13)")

(run "
class oddeven extends object
 method initialize () 1
 final method even(n)
       if zero?(n) then 1 else send self odd(-(n, 1))
 final method odd(n)
       if zero?(n) then 0 else send self even(-(n, 1))

class bug-oddeven extends oddeven
 method initialize () 1
 method even(n)
       if zero?(n) then 0 else send self odd(-(n, 1))
 method odd(n)
       if zero?(n) then 0 else send self even(-(n, 1))
 let o1 = new bug-oddeven()
 in send o1 odd(13)")

(run-all)
