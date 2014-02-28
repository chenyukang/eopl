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
      "body" module-body)
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

    ;; new stuff
    (module-body
     ("let" identifier "=" expression "in" module-body)
     let-module-body)


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


;; value-of-module-body : ModuleBody * Env -> TypedModule
(define value-of-module-body
  (lambda (m-body env)
    (cases module-body m-body
           (defns-module-body (defns)
             (simple-module
              (defns-to-env defns env)))
	   (let-module-body (var exp body)
			    (let  ((val (value-of exp env)))
			      (let ((new-env (extend-env var val env)))
				(value-of-module-body body new-env)))))))

(define add-module-defns-to-env
  (lambda (defs env)
    (if (null? defs)
        env
        (cases module-definition (car defs)
               (a-module-definition (m-name iface m-body)
                                    (add-module-defns-to-env
                                     (cdr defs)
                                     (extend-env-with-module
                                      m-name
                                      (value-of-module-body m-body env)
                                      env)))))))



;; interface-of : ModuleBody * Tenv -> Iface
(define interface-of
  (lambda (m-body tenv)
    (cases module-body m-body
           (defns-module-body (defns)
             (simple-iface
              (defns-to-decls defns tenv)))
	   (let-module-body (var exp body)
			    '()))))



;; (run "module even-odd
;;       interface
;;     [even : int -> int
;;          odd  : int > int ]
;;      body
;;         let x = 2
;;          in [ even = proc(x) -(x, 2)
;;       odd =  proc(x) -(x ,3) ]")


(run "module even-odd
  interface
     [even : (int -> int)
      odd  : (int -> int) ]
  body
  let x = 1 in
 [ even = proc(x : int) -(x, 1)
	odd = proc(x : int) -(x, 2) ]
 from even-odd take odd")


(run "module even-odd
  interface
     [even : int
      odd  : int ]
  body
  let x = 1 in
 [ even = x
   odd =  -(x, 2) ]
 from even-odd take odd")

;;(run "proc(x : int) -(x, 1)")

(run-all)
(check-all)
