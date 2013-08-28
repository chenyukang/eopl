;; This is 6-grammar.scm:  grammar for typed-oo language

(let ((time-stamp "Time-stamp: <2000-12-15 15:18:22 wand>"))
  (let ((date  (substring time-stamp 13 29)))
    (eopl:printf "grammar.scm: ~a~%" date)))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (number (digit (arbno digit)) number)
    ))
  
(define the-grammar
  '((program ((arbno class-decl) expression) a-program)

    (expression (number) lit-exp)
    (expression ("true") true-exp)        
    (expression ("false") false-exp)        
    (expression (identifier) var-exp)   
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (expression
      ("if" expression "then" expression "else" expression)
      if-exp)

    (expression
      ("let" (arbno identifier "=" expression) "in" expression)
      let-exp)

    (expression                                
      ("proc" "(" (separated-list type-exp identifier ",") ")" expression)
      proc-exp)
    (expression
      ("(" expression (arbno expression) ")")
      app-exp)
    (expression
      ("letrec"
        (arbno type-exp identifier
          "(" (separated-list type-exp identifier ",") ")" 
          "=" expression)
        "in" expression)
      letrec-exp)
    (expression ("set" identifier "=" expression) varassign-exp)
    (expression
      ("begin" expression (arbno ";" expression) "end")
      begin-exp)
    (expression
      ("list" "(" expression (arbno "," expression) ")")
      list-exp)
    (expression
      ("cons" "(" expression "," expression ")")
      cons-exp)
    (expression
      ("car" "(" expression ")" )
      car-exp)
    (expression
      ("cdr" "(" expression ")" )
      cdr-exp)
    (expression 
      ("nil" "[" type-exp "]")
      nil-exp)
    (expression
      ("null?" "(" expression ")" )
      null?-exp)

    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-test-prim)

    (class-decl
      (abstraction-specifier                ; new for ch6
       "class" identifier
       "extends" identifier
         
         (arbno "field" type-exp identifier)
         (arbno method-decl)
         )
      a-class-decl)

    (abstraction-specifier                ; new for ch6
      ()
      concrete-specifier)

    (abstraction-specifier
      ("abstract")
      abstract-specifier)

    (field-decl                         
      ("field" type-exp identifier)
      a-field-decl)

    (method-decl                        
      ("method"
        type-exp        
        identifier 
        "("  (separated-list type-exp identifier ",") ")" ; method ids
         expression  )
      a-method-decl)

    (method-decl                        ; ch 6
      ("abstractmethod" type-exp identifier 
        "("  (separated-list type-exp identifier ",") ")" ; method ids
        )                               ; no body
      an-abstract-method-decl)

    (expression 
      ("new" identifier "(" (separated-list expression ",") ")")
      new-object-exp)
    (expression
      ("send" expression identifier
        "("  (separated-list expression ",") ")")
      method-app-exp)
    (expression
      ("super" identifier
        "("  (separated-list expression ",") ")")
      super-call-exp)

    ;; these do not appear in ordinary programs
    (expression
      ("apply-method-indexed" expression number
          "("  (separated-list expression ",") ")")
      apply-method-indexed-exp)

    (expression
      ("lexvar" number number)
      lexvar-exp)

    (expression
     ("cast" expression identifier)
     cast-exp)

    (expression
      ("instanceof" expression identifier)
      instanceof-exp)

    (type-exp ("int") int-type-exp)             
    (type-exp ("bool") bool-type-exp)           
    (type-exp ("void") void-type-exp)      ; new for ch 6
    (type-exp (identifier) class-type-exp) ; new for ch 6
    (type-exp                               
      ("(" (separated-list type-exp "*") "->" type-exp ")")
      proc-type-exp)
    (type-exp
      ("list" type-exp)
      list-type-exp)

    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

