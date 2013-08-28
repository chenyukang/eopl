(let ((time-stamp "Time-stamp: <2001-01-03 13:46:27 dfried>"))
  (eopl:printf "4-4.scm: language with type inference ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level interface ;;;;;;;;;;;;;;;;

(define type-check
  (lambda (string)
    (type-to-external-form
      (type-of-program
        (scan&parse string)))))

(define run
  (lambda (string)
    (eval-program (scan&parse string))))

(define all-groups '(lang4-2 lang4-3 lang4-4))

(define run-all
  (lambda ()
    (run-experiment run use-execution-outcome
      all-groups all-tests)))

(define run-one
  (lambda (test-name)
    (run-test run test-name)))

(define check-all
  (lambda ()
    (run-experiment type-check use-checker-outcome
      all-groups all-tests)))

(define check-one
  (lambda (test-name)
    (run-test type-check test-name)))

(define equal-external-reps? equal-up-to-gensyms?)

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier
      (letter (arbno (or letter digit "_" "-" "?")))
      symbol)
    (number (digit (arbno digit)) number)))

(define the-grammar
  '((program (expression) a-program)
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
      ("let" (arbno  identifier "=" expression) "in" expression)
      let-exp)
    (expression                         
      ("proc" "(" (separated-list optional-type-exp identifier ",") ")" expression)
      proc-exp)
    (expression
      ("(" expression (arbno expression) ")")
      app-exp)

    (expression
      ("letrec"
        (arbno optional-type-exp identifier
          "(" (separated-list optional-type-exp identifier ",") ")"
          "=" expression) "in" expression) 
      letrec-exp)

    (expression
      ("lettype" identifier "=" type-exp
        (arbno type-exp identifier
          "(" (separated-list type-exp identifier ",") ")"
          "=" expression)
        "in" expression)
      lettype-exp)

    (primitive ("+")     add-prim)
    (primitive ("-")     subtract-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-test-prim)

    (type-exp ("int") int-type-exp)             
    (type-exp ("bool") bool-type-exp)           
    (type-exp                               
      ("(" (separated-list type-exp "*") "->" type-exp ")")
      proc-type-exp)
    (type-exp (identifier) tid-type-exp)        

    (optional-type-exp                           ; new for 4-4
      ("?")
      no-type-exp)
    (optional-type-exp
      (type-exp)
      a-type-exp)

    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatype
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;

(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp) (type-of-expression exp (empty-tenv))))))

(define type-of-expression
  (lambda (exp tenv)
    (cases expression exp
      (lit-exp (number) int-type)
      (true-exp () bool-type)
      (false-exp () bool-type)
      (var-exp (id) (apply-tenv tenv id))
      (if-exp (test-exp true-exp false-exp)
        (let
          ((test-type (type-of-expression test-exp tenv))
           (false-type (type-of-expression false-exp tenv))
           (true-type (type-of-expression true-exp tenv)))
          ;; these tests either succeed or raise an error
          (check-equal-type! test-type bool-type test-exp)
          (check-equal-type! true-type false-type exp)
          true-type))
      (proc-exp (texps ids body)
        (type-of-proc-exp texps ids body tenv))      
      (primapp-exp (prim rands)
        (type-of-application
          (type-of-primitive prim)
          (types-of-expressions rands tenv)
          prim rands exp))
      (app-exp (rator rands) 
        (type-of-application
          (type-of-expression rator tenv)
          (types-of-expressions rands tenv)
          rator rands exp))
      (let-exp (ids rands body) (type-of-let-exp ids rands body tenv))
      (letrec-exp (result-texps proc-names texpss idss bodies
                    letrec-body)
        (type-of-letrec-exp
          result-texps proc-names texpss idss bodies
          letrec-body tenv))
      (lettype-exp (type-name texp
                     result-texps proc-names texpss
                     idss bodies lettype-body)
        (type-of-lettype-exp type-name texp
          result-texps proc-names texpss idss bodies
          lettype-body tenv))
      )))

(define type-of-proc-exp
  (lambda (texps ids body tenv)
    (let ((arg-types (expand-optional-type-expressions texps tenv)))
      (let ((result-type
              (type-of-expression body
                (extend-tenv ids arg-types tenv))))
        (proc-type arg-types result-type)))))

(define type-of-application             
  (lambda (rator-type actual-types rator rands exp)
    (let ((result-type (fresh-tvar)))
      (check-equal-type!
        rator-type
        (proc-type actual-types result-type)
        exp)
      result-type)))

(define types-of-expressions
  (lambda (rands tenv)
    (map
      (lambda (exp) (type-of-expression exp tenv))
      rands)))

(define type-of-let-exp
  (lambda (ids rands body tenv)
    (let ((tenv-for-body
            (extend-tenv
              ids
              (types-of-expressions rands tenv)
              tenv)))
      (type-of-expression body tenv-for-body))))

(define type-of-letrec-exp
  (lambda (result-texps proc-names arg-optional-texpss idss bodies
            letrec-body tenv) 
    (let
      ((arg-typess
         (map
           (lambda (optional-texps)
             (expand-optional-type-expressions optional-texps tenv))
           arg-optional-texpss))
       (result-types
         (expand-optional-type-expressions result-texps tenv)))
    (let ((the-proc-types
            (map proc-type arg-typess result-types)))
      (let ((tenv-for-body                 ;^ type env for all proc-bodies
              (extend-tenv proc-names the-proc-types tenv)))
        (for-each                     
          (lambda (ids arg-types body result-type)
            (check-equal-type!
              (type-of-expression 
                body
                (extend-tenv ids arg-types tenv-for-body))
              result-type
              body))
          idss arg-typess bodies result-types)
        (type-of-expression letrec-body tenv-for-body))))))

(define extend-tenv-with-typedef-exp
  (lambda (typename texp tenv)
    (extend-tenv-with-typedef typename
      (expand-type-expression texp tenv)
      tenv)))

(define extend-tenv-with-type-exps
  (lambda (ids texps tenv)
    (extend-tenv ids
      (expand-type-expressions texps tenv)
      tenv)))

(define fresh-type
  (let ((serial-number 0))
    (lambda (s)
      (set! serial-number (+ serial-number 1))
      (atomic-type 
        (string->symbol
          (string-append
            (symbol->string s)
            (number->string serial-number)))))))

(define type-of-lettype-exp
  (lambda (type-name texp
            result-texps proc-names arg-texpss idss bodies
            lettype-body tenv)
    (let
      ((the-new-type (fresh-type type-name))
       (rhs-texps  
         (map proc-type-exp arg-texpss result-texps)))
      (let
        ((tenv-for-implementation
           ;; here type definition is known-- bind it to its definition
           (extend-tenv-with-typedef-exp type-name texp tenv))
         (tenv-for-client
           ;; here the defined type is opaque-- bind it to a new atomic type
           (extend-tenv-with-typedef type-name the-new-type tenv)))
        (let
          ((tenv-for-proc             ; type env for all proc-bodies
             (extend-tenv-with-type-exps
               proc-names rhs-texps tenv-for-implementation))
           (tenv-for-body                   ; type env for body
             (extend-tenv-with-type-exps
               proc-names rhs-texps tenv-for-client)))
          (for-each                     
            (lambda (ids arg-texps body result-texp)
              (check-equal-type!
                (type-of-expression 
                  body
                  (extend-tenv-with-type-exps
                    ids arg-texps tenv-for-proc))
                (expand-type-expression
                  result-texp tenv-for-proc)
                body))
            idss arg-texpss
            bodies result-texps)
          (type-of-expression lettype-body tenv-for-body))))))

;;;;;;;;;;;;;;;; types ;;;;;;;;;;;;;;;;

(define-datatype type type?
  (atomic-type (name symbol?))
  (proc-type
    (arg-types (list-of type?))
    (result-type type?))
  (tvar-type                            ;\new3
    (serial-number integer?)
    (container vector?)))

;;; selectors and extractors for types

(define atomic-type?
  (lambda (ty)
    (cases type ty
      (atomic-type (name) #t)
      (else #f))))

(define proc-type?
  (lambda (ty)
    (cases type ty
      (proc-type (arg-types result-type) #t)
      (else #f))))

(define tvar-type?
  (lambda (ty)
    (cases type ty
      (tvar-type (sn cont) #t)
      (else #f))))

(define atomic-type->name
  (lambda (ty)
    (cases type ty
      (atomic-type (name) name)
      (else (eopl:error 'atomic-type->name
              "Not an atomic type: ~s" ty)))))

(define proc-type->arg-types
  (lambda (ty)
    (cases type ty
      (proc-type (arg-types result-type) arg-types)
      (else (eopl:error 'proc-type->arg-types
              "Not a proc type: ~s" ty)))))

(define proc-type->result-type
  (lambda (ty)
    (cases type ty
      (proc-type (arg-types result-type) result-type)
      (else (eopl:error 'proc-type->arg-types
              "Not a proc type: ~s" ty)))))

(define tvar-type->serial-number
  (lambda (ty)
    (cases type ty
      (tvar-type (sn c) sn)
      (else (eopl:error 'tvar-type->serial-number
              "Not a tvar-type: ~s" ty)))))

(define tvar-type->container
  (lambda (ty)
    (cases type ty
      (tvar-type (sn vec) vec)
      (else (eopl:error 'tvar-type->container
              "Not a tvar-type: ~s" ty)))))

;;; type variables

;; in a tvar-type, the container is a vector of length 1.  It can
;; contain either: 
;; 1.  nil, meaning that nothing is known about this tvar
;; 2.  a (pointer to) a type
;; also:  type structures may never be cyclic.

(define fresh-tvar
  (let ((serial-number 0))
    (lambda ()
      (set! serial-number (+ 1 serial-number))
      (tvar-type serial-number (vector '())))))

(define tvar->contents
  (lambda (ty)
    (vector-ref (tvar-type->container ty) 0)))

(define tvar-set-contents!
  (lambda (ty val)
    (vector-set! (tvar-type->container ty) 0 val)))

(define tvar-non-empty?
  (lambda (ty)
    (not (null? (vector-ref (tvar-type->container ty) 0)))))

(define expand-optional-type-expressions
  (lambda (otexps tenv)
    (map
      (lambda (otexp)
        (expand-optional-type-expression otexp tenv))
      otexps)))

(define expand-optional-type-expression
  (lambda (otexp tenv)
    (cases optional-type-exp otexp
      (no-type-exp () (fresh-tvar))
      (a-type-exp (texp) (expand-type-expression texp tenv)))))

(define expand-type-expressions
  (lambda (texps tenv)
    (map 
      (lambda (texp)
        (expand-type-expression texp tenv))
      texps)))

(define expand-type-expression
  (lambda (texp tenv)
    (letrec 
      ((loop (lambda (texp)
               (cases type-exp texp
                 (tid-type-exp (id) (apply-tenv-typedef tenv id))
                 (int-type-exp () (atomic-type 'int))
                 (bool-type-exp () (atomic-type 'bool))
                 (proc-type-exp (arg-texps result-texp)
                   (proc-type
                     (map loop arg-texps)
                     (loop result-texp)))))))
      (loop texp))))

;;;;;;;;;;;;;;;; the unifier ;;;;;;;;;;;;;;;;

;;; cases is very cumbersome in this application, so don't use it!

(define check-equal-type!               
  (lambda (t1 t2 exp)
    (cond
      ((eqv? t1 t2))     ;^ succeed with void result
      ((tvar-type? t1) (check-tvar-equal-type! t1 t2 exp))
      ((tvar-type? t2) (check-tvar-equal-type! t2 t1 exp))
      ((and (atomic-type? t1) (atomic-type? t2))
       (if (not
             (eqv?
               (atomic-type->name t1)
               (atomic-type->name t2)))
         (raise-type-error t1 t2 exp)))
      ((and (proc-type? t1) (proc-type? t2))
       (let ((arg-types1 (proc-type->arg-types t1))
             (arg-types2 (proc-type->arg-types t2))
             (result-type1 (proc-type->result-type t1))
             (result-type2 (proc-type->result-type t2)))
         (if (not
               (= (length arg-types1) (length arg-types2)))
           (raise-wrong-number-of-arguments t1 t2 exp)
           (begin
             (for-each
               (lambda (t1 t2)
                 (check-equal-type! t1 t2 exp))
               arg-types1 arg-types2)
             (check-equal-type!
               result-type1 result-type2 exp)))))
      (else (raise-type-error t1 t2 exp)))))

(define check-tvar-equal-type!
  (lambda (tvar ty exp)
    (if (tvar-non-empty? tvar)
      (check-equal-type! (tvar->contents tvar) ty exp)
      (begin
        (check-no-occurrence! tvar ty exp)
        (tvar-set-contents! tvar ty)))))

(define check-no-occurrence!
  (lambda (tvar ty exp)
    (letrec
      ((loop
         (lambda (ty1)
           (cases type ty1
             (atomic-type (name) #t)         ;^ <void> not permitted here
             (proc-type (arg-types result-type)
               (begin
                 (for-each loop arg-types)
                 (loop result-type)))
             (tvar-type (num vec)
               (if (tvar-non-empty? ty1)
                 (loop (tvar->contents ty1))
                 (if (eqv? tvar ty1)
                   (raise-occurrence-check tvar ty exp))))))))
      (loop ty))))

(define raise-type-error
  (lambda (t1 t2 exp)
    (eopl:error 'check-equal-type!
      "Type mismatch: ~s doesn't match ~s in ~s~%"
      (type-to-external-form t1)
      (type-to-external-form t2)
      exp)))

(define raise-wrong-number-of-arguments
  (lambda (t1 t2 exp)
    (eopl:error 'check-equal-type!
      "Different numbers of arguments ~s and ~s in ~s~%"
      (type-to-external-form t1)
      (type-to-external-form t2)
      exp)))

(define raise-occurrence-check
  (lambda (tvnum t2 exp)
    (eopl:error 'check-equal-type!
      "Can't unify: ~s occurs in type ~s in expression ~s~%" 
      tvnum
      (type-to-external-form t2)
      exp)))

;;; types of primitives

(define int-type (atomic-type 'int))
(define bool-type (atomic-type 'bool))

(define binop-type (proc-type (list int-type int-type) int-type))
(define unop-type  (proc-type (list int-type) int-type))
(define int->bool-type (proc-type (list int-type) bool-type))

(define type-of-primitive
  (lambda (prim)
    (cases primitive prim
      (add-prim  ()     binop-type)
      (subtract-prim () binop-type)
      (mult-prim  ()    binop-type)
      (incr-prim  ()    unop-type)
      (decr-prim  ()    unop-type)
      (zero-test-prim () int->bool-type))))


;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;
    
(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
    (syms (list-of symbol?))
    (vals (list-of type?))
    (tenv type-environment?))
  (typedef-record      ;\new4
    (name symbol?)
    (definition type?)
    (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)
(define extend-tenv-with-typedef typedef-record) 

(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
        (eopl:error 'apply-tenv
          "Variable ~s unbound in type environment" sym))
      (extended-tenv-record (syms vals tenv)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (list-ref vals pos)
            (apply-tenv tenv sym))))
      (typedef-record (name type tenv)
        (apply-tenv tenv sym)))))

(define apply-tenv-typedef                    
  (lambda (tenv0 sym)
    (let loop ((tenv tenv0))
      (cases type-environment tenv
        (empty-tenv-record ()
          (eopl:error 'apply-tenv
            "Type variable ~s unbound in type environment ~s"
            sym tenv0))
        (extended-tenv-record (syms vals tenv) (loop tenv))
        (typedef-record (name type tenv) 
          (if (eqv? name sym) type (loop tenv)))))))

;;;;;;;;;;;;;;;; external form of types ;;;;;;;;;;;;;;;;

(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (atomic-type (name) name)
      (proc-type (arg-types result-type)
        (append
          (arg-types-to-external-form arg-types)
          '(->)
          (list (type-to-external-form result-type))))
      (tvar-type (serial-number container) ;\new7
        (if (tvar-non-empty? ty)
          (type-to-external-form (tvar->contents ty))
          (string->symbol
            (string-append
              "tvar"
              (number->string serial-number))))))))

          

(define arg-types-to-external-form
  (lambda (types)
    (if (null? types)
      '()
      (if (null? (cdr types))
        (list (type-to-external-form (car types)))
        (cons
          (type-to-external-form (car types))
          (cons '*
                (arg-types-to-external-form (cdr types))))))))



;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

(define eval-program
  (lambda (pgm)
      (cases program pgm
        (a-program (exp)
          (eval-expression exp (empty-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (true-exp () 1)
      (false-exp () 0)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
        (if (true-value? (eval-expression test-exp env))
          (eval-expression true-exp env)
          (eval-expression false-exp env)))
      (let-exp (ids rands body)
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      (proc-exp (texps ids body) (closure ids body env))

      (app-exp (rator rands)
        (let ((proc (eval-expression  rator env))
              (args (eval-rands rands env)))
          (if (procval? proc)           ; should always be true in
                                        ; typechecked code
            (apply-procval proc args)
            (eopl:error 'eval-expression 
              "Attempt to apply non-procedure ~s" proc))))

      (letrec-exp (result-texps proc-names texpss idss bodies
                    letrec-body)
        (eval-expression letrec-body
          (extend-env-recursively proc-names idss bodies env)))

      (lettype-exp (type-name texp
                     result-texps proc-names texpss
                     idss bodies lettype-body)
        (eval-expression lettype-body
          (extend-env-recursively proc-names idss bodies env)))

      )))

(define eval-rands
  (lambda (exps env)
    (map
      (lambda (exp) (eval-expression exp env))
      exps)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim  () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim  () (* (car args) (cadr args)))
      (incr-prim  () (+ (car args) 1))
      (decr-prim  () (- (car args) 1))
      (zero-test-prim () (if (zero? (car args)) 1 0))
      )))

;;;;;;;;;;;;;;;; booleans ;;;;;;;;;;;;;;;;

(define true-value?
  (lambda (x)
    (not (zero? x))))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

(define-datatype procval procval?
  (closure 
    (ids (list-of symbol?)) 
    (body expression?)
    (env environment?)))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
        (eval-expression body (extend-env ids args env))))))

;;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

(define-datatype environment environment? 
  (empty-env-record)             
  (extended-env-record
    (syms (list-of symbol?))
    (vals vector?)
    (env environment?)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
            (vector-ref vals pos)
            (apply-env old-env sym)))))))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))
 
(define extend-env-recursively
  (lambda (proc-names idss exps old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids exp)
              (vector-set! vec pos (closure ids exp env)))
            (iota len) idss exps)
          env)))))

(define rib-find-position
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

; ;;;;;;;;;;;;;;;; hooks for test harness ;;;;;;;;;;;;;;;;

; (define equal-external-reps?
;   (lambda (rep1 rep2)
;     (cond
;       ((eqv? rep1 rep2) #t)
;       ((and (symbol? rep1) (symbol? rep2))
;        ;; should really check to see that the mapping is consistent
;        (same-symbol-up-to-gensym? rep1 rep2))
;       ((and (pair? rep1) (pair? rep2))
;        (and
;          (equal-external-reps? (car rep1) (car rep2))
;          (equal-external-reps? (cdr rep1) (cdr rep2))))
;       (else #f))))

; (define same-symbol-up-to-gensym?
;   (lambda (sym1 sym2)
;     (let loop ((lst1 (symbol->list sym1))
;                (lst2 (symbol->list sym2)))
;       (cond
;         ((and (list-of-digits? lst1) (list-of-digits? lst2)) #t)
;         ((eqv? (car lst1) (car lst2))
;          (loop (cdr lst1) (cdr lst2)))
;         (else #f)))))

; (define symbol->list
;   (lambda (x) (string->list (symbol->string x))))

; (define list-of-digits?
;   (lambda (lst)
;     (cond
;       ((null? lst) #t)
;       ((char-numeric? (car lst))
;        (list-of-digits? (cdr lst)))
;       (else #f))))

