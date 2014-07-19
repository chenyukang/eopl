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
  '((program (expression) a-program)

    (expression (number) const-exp)

    (expression
     ("-" "(" expression "," expression ")")
     diff-exp)

    (expression
     ("+" "(" (separated-list expression ",") ")")
     sum-exp)

    (expression
     ("zero?" "(" expression ")")
     zero?-exp)

    (expression
     ("if" expression "then" expression "else" expression)
     if-exp)

    (expression
     ("letrec"
      (arbno identifier "(" (arbno identifier) ")"
             "=" expression)
      "in"
      expression)
     letrec-exp)

    (expression (identifier) var-exp)

    (expression
     ("let" identifier "=" expression "in" expression)
     let-exp)

    (expression
     ("proc" "(" (arbno identifier) ")" expression)
     proc-exp)

    (expression
     ("(" expression (arbno expression) ")")
     call-exp)

    ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))


;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;
(define cps-out-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "_" "-" "?")))
     symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    ))

(define cps-out-grammar
  '((cps-out-program (tfexp) cps-a-program)

    (simple-expression (number) cps-const-exp)

    (simple-expression (identifier) cps-var-exp)

    (simple-expression
     ("-" "(" simple-expression "," simple-expression ")")
     cps-diff-exp)

    (simple-expression
     ("zero?" "(" simple-expression ")")
     cps-zero?-exp)

    (simple-expression
     ("+" "(" (separated-list simple-expression ",") ")")
     cps-sum-exp)

    (simple-expression
     ("proc" "(" (arbno identifier) ")" tfexp)
     cps-proc-exp)

    (tfexp
     (simple-expression)
     simple-exp->exp)

    (tfexp
     ("let" identifier "=" simple-expression "in" tfexp)
     cps-let-exp)

    (tfexp
     ("letrec"
      (arbno identifier "(" (arbno identifier) ")"
             "=" tfexp)
      "in"
      tfexp)
     cps-letrec-exp)

    (tfexp
     ("if" simple-expression "then" tfexp "else" tfexp)
     cps-if-exp)

    (tfexp
     ("(" simple-expression (arbno simple-expression) ")")
     cps-call-exp)

    ))

;;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;
(sllgen:make-define-datatypes cps-out-lexical-spec cps-out-grammar)

(define cps-show-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes cps-out-lexical-spec cps-out-grammar)))

(define cps-out-scan&parse
  (sllgen:make-string-parser cps-out-lexical-spec cps-out-grammar))

