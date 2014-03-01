(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/equal-type.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cases.scm")
(load-relative "./base/simplemodule-lang.scm")



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
  '(
    (program
     ((arbno module-definition)
      expression)
     a-program)

    (module-definition
     ("module" identifier
      "interface" interface
      "body"
      (arbno module-definition)
      module-body)
     a-module-definition)


    (interface
     ("[" (arbno declaration) "]")
     simple-iface)


    (declaration
     (identifier ":" type)
     val-decl)


    (module-body
     ("[" (arbno definition) "]")
     defns-module-body)


    (definition
      (identifier "=" expression)
      val-defn)


    ;; new expression:
    (expression
     ("from" identifier "take" identifier)
     qualified-var-exp)

    ;; new types

    (type
     (identifier)
     named-type)

    (type
     ("from" identifier "take" identifier)
     qualified-type)


      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; no changes in grammar below here
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (expression (number) const-exp)

    (expression
     (identifier)
     var-exp)

    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("proc" "(" identifier ":" type ")" expression)
     proc-exp)

    (expression
     ("(" expression expression ")")
     call-exp)

    (expression
     ("letrec"
      type identifier "(" identifier ":" type ")"
      "=" expression "in" expression)
     letrec-exp)

    (type
     ("int")
     int-type)

    (type
     ("bool")
     bool-type)

    (type
     ("(" type "->" type ")")
     proc-type)

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

  ;;;;;;;;;;;;;;;; syntactic tests and observers ;;;;;;;;;;;;;;;;

(define module-definition->name
  (lambda (m-defn)
    (cases module-definition m-defn
           (a-module-definition (m-name m-type inner-module m-body)
                                m-name))))


(define module-definition->interface
  (lambda (m-defn)
    (cases module-definition m-defn
           (a-module-definition (m-name m-type inner-module m-body)
                                m-type))))


(define module-definition->body
  (lambda (m-defn)
    (cases module-definition m-defn
           (a-module-definition (m-name m-type inner-module m-body)
                                m-body))))

;; add-module-defns-to-env : Listof(Defn) * Env -> Env
(define add-module-defns-to-env
  (lambda (defs env)
    (if (null? defs)
        env
        (cases module-definition (car defs)
               (a-module-definition (m-name iface inner-modules m-body)
				    (let ((new-module-env
					   (add-module-defns-to-env inner-modules env)))
                                    (add-module-defns-to-env
                                     (cdr defs)
                                     (extend-env-with-module
                                      m-name
                                      (value-of-module-body m-body new-module-env)
                                      env))))))))

;; add-module-defns-to-tenv : Listof(ModuleDefn) * Tenv -> Tenv
(define add-module-defns-to-tenv
  (lambda (defns tenv)
    (if (null? defns)
        tenv
        (cases module-definition (car defns)
               (a-module-definition (m-name expected-iface inner-modules m-body)
				    (let ((new-module-tenv (add-module-defns-to-tenv inner-modules tenv)))
				      (let ((actual-iface (interface-of m-body new-module-tenv)))
					(if (<:-iface actual-iface expected-iface tenv)
					    (let ((new-tenv
						   (extend-tenv-with-module
						    m-name
						    expected-iface
						    tenv)))
                                            (add-module-defns-to-tenv
                                             (cdr defns) new-tenv))
                                          (report-module-doesnt-satisfy-iface
                                           m-name expected-iface actual-iface)))))))))




(run "module m1
    interface
      [u : int
        v : int]
        body
    module m2
     interface [v : int]
     body [v = 33]
   [u = 44
   v = -(from m2 take v, 1)]
  from m1 take u")


(check "module m1
    interface
      [u : int
        v : int]
        body
    module m2
     interface [v : int]
     body [v = 33]
   [u = 44
   v = -(from m2 take v, 1)]
  from m1 take u")


(run-all)
(check-all)
