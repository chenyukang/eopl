;;; 8-5set.scm - conversion to cps with print, set, and letcc

(let ((time-stamp "Time-stamp: <2000-12-21 14:48:55 wand>"))
  (eopl:printf "8-5set.scm - convert to cps with set ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level ;;;;;;;;;;;;;;;;

(define cps
  (lambda (string)
    (initialize-gensymbol-counter!)
    (unparse (cps-of-program (scan&parse string)))))

;;; This algorithm gives different output than the algorithm of 8-4,
;;; so the tests of lang8-4 are repeated in lang8-5set with the
;;; proper outcomes.

(define cps-all
  (lambda ()
    (run-experiment cps use-execution-outcome
      '(lang8-5set)        
      all-tests)))

(define cps-one
  (lambda (test-name)
    (run-test cps test-name)))

(define equal-external-reps? equal-up-to-gensyms?)  ; defined in
                                                    ; test-harness.scm

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number   (digit (arbno digit)) number)))

(define the-grammar
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression
      (primitive "(" (separated-list expression ",") ")")
      primapp-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression
      ("let" (arbno  identifier "=" expression) "in" expression)
      let-exp)
    (expression
      ("proc" "(" (separated-list identifier ",") ")" expression)
      proc-exp)
    (expression ("(" expression (arbno expression) ")") app-exp)
    (expression                         
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression)
      letrec-exp)

;;; print, in source and target langs:
    (expression ("print" "(" expression ")") print-exp)
    (expression ("printc" "(" expression "," expression ")" ) printc-exp)

;;; for source language:
    (expression ("set" identifier "=" expression) varassign-exp)

;;; for target language:
    (expression ("setc" identifier expression expression) varassignc-exp)
    (expression ("derefc" identifier expression) derefc-exp)
    (expression ("%generated-var" identifier) genvar-exp)

;;; letcc, in source lang only:
    (expression ("letcc" identifier "in" expression) letcc-exp) 
    (expression ("throw" expression "to" expression) throw-exp) 

;;; make primitives match unparse.
    (primitive ("+")    plus-prim)
    (primitive ("-")    minus-prim)
    (primitive ("*")    mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive ("zero?") zero?-prim)
    (primitive ("car")  car-prim)
    (primitive ("cdr")  cdr-prim)    
    (primitive ("cons") cons-prim)
    (primitive ("emptylist") empty-prim) ; ??
    (primitive ("null?") null?-prim)
    (primitive ("list")  list-prim)
    ))

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

(define list-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

;;; **************** gensym ****************

;;; need this here for init-k (sorry)

(define gensymbol
  (let ((n 0))
    (lambda (s)
      (set! n (+ n 1))
      (let ((s (if (string? s) s (symbol->string s))))
        (string->symbol
          (string-append s (number->string n)))))))

;; actually, we lied:  we need to be able to reset the gensymbol
;; counter

(define initialize-gensymbol-counter! '*)
(define gensymbol '*)

(let ((n 0))
  (set! initialize-gensymbol-counter!
    (lambda () (set! n 0)))
  (set! gensymbol
    (lambda (s)
      (set! n (+ n 1))
      (let ((s (if (string? s) s (symbol->string s))))
        (string->symbol
          (string-append s (number->string n)))))))

(define genvar-exp?
  (lambda (x)
    (cases expression x
      (genvar-exp (id) #t)
      (else #f))))

;;; **************** top-level ****************

(define run
  (lambda (string)
    (unparse (cps-of-program (scan&parse string)))))

;;; **************** cps transformer ****************

(define k-id (gensymbol "k"))
(define k-var-exp (genvar-exp k-id))

(define cps-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp) 
        (proc-exp (list k-id)
          (cps-of-expression exp k-var-exp))))))

(define cps-of-simple-exp ;^ translation for simple expressions
  (lambda (exp)
    (cases expression exp
      (lit-exp (datum) exp)
      ;^ vars are no longer simple!
      ;^     (var-exp (id) (var-exp id))
      ;^; but target-vars are:
      (genvar-exp (id) (genvar-exp id))
      (primapp-exp (prim rands) ;^ rands are known to be simple
        (primapp-exp prim (map cps-of-simple-exp rands)))
      (if-exp (test-exp true-exp false-exp)
        (if-exp
          (cps-of-simple-exp test-exp)
          (cps-of-simple-exp true-exp)
          (cps-of-simple-exp false-exp)))
      (let-exp (ids rands body)
        (let-exp ids (map cps-of-simple-exp rands)
          (cps-of-simple-exp body)))
      (proc-exp (ids body)
        (proc-exp
          (append ids (list k-id))
          (cps-of-expression body k-var-exp)))
      (letrec-exp (proc-names idss bodies letrec-body)
        (letrec-exp proc-names
          (map
            (lambda (ids)
              (append ids (list k-id)))
            idss)
          (map
            (lambda (body)
              (cps-of-expression body k-var-exp))
            bodies)
          (cps-of-simple-exp letrec-body)))
      (else
        (eopl:error 'cps-of-simple-exp
          "can't call cps-of-simple-exp on non-simple expression ~s"
              exp)))))

(define cps-of-expression 
  (lambda (exp k)
    (if (non-simple? exp)
      (cases expression exp
        (var-exp (id) (derefc-exp id k))
        (if-exp (test-exp true-exp false-exp)
          (cps-of-if-exp test-exp true-exp false-exp k))
        (primapp-exp (prim rands)
          (cps-of-primapp-exp prim rands k))
        (let-exp (ids rands body) 
          (cps-of-let-exp ids rands body k))
        (app-exp (rator rands)
          (cps-of-app-exp rator rands k))
        (letrec-exp (proc-names idss bodies letrec-body)
          (cps-of-letrec-exp proc-names idss bodies letrec-body k))
        (print-exp (exp) (cps-of-print-exp exp k))
        (letcc-exp (id body) (cps-of-letcc-exp id body k))
        (throw-exp (value-exp cont-exp)
          (cps-of-throw-exp value-exp cont-exp k))
        (varassign-exp (id exp)
          (cps-of-varassign-exp id exp k))
        (else
          (eopl:error 'cps-of-expression
            "can't call on  ~s" exp))
        )
      (csimple exp k))))

(define csimple
  (lambda (exp k)
    (cases expression k
      (proc-exp (ids body)
        (let-exp ids (list (cps-of-simple-exp exp)) body))
      (else (app-exp k (list (cps-of-simple-exp exp)))))))

(define cps-of-if-exp
  (lambda (test-exp true-exp false-exp k)
    (if (non-simple? test-exp)
      (let ((v-id (gensymbol "v")))
        (cps-of-expression test-exp
          (proc-exp (list v-id)
            (cps-of-expression 
              (if-exp (genvar-exp v-id) true-exp false-exp)
              k))))
      (if-exp (cps-of-simple-exp test-exp)
        (cps-of-expression true-exp k)
        (cps-of-expression false-exp k)))))

(define cps-of-primapp-exp
  (lambda (prim rands k)
    (let ((pos (list-index non-simple? rands))
          (v-id (gensymbol "v")))
            ;^; this always succeeds, because the expression is known to be
            ;^; non-simple. 
      (cps-of-expression (list-ref rands pos)
        (proc-exp (list v-id)
          (cps-of-expression
            (primapp-exp
              prim
              (list-set rands pos (genvar-exp v-id)))
            k))))))

(define cps-of-app-exp
  (lambda (rator rands k)
    (if (non-simple? rator)
      (let ((v-id (gensymbol "v")))
        (cps-of-expression rator
          (proc-exp (list v-id)
            (cps-of-expression 
              (app-exp (genvar-exp v-id) rands)
               k))))
      (cps-of-app-exp-simple-rator rator rands k))))

(define cps-of-app-exp-simple-rator
  (lambda (rator rands k)
    (let ((pos (list-index non-simple? rands)))
      (if (number? pos)
        (let ((v-id (gensymbol "v")))
          (cps-of-expression (list-ref rands pos)
            (proc-exp (list v-id)
              (cps-of-expression
                (app-exp
                  rator
                  (list-set rands pos (genvar-exp v-id)))
                k))))
        (app-exp
          (cps-of-simple-exp rator)
          (append (map cps-of-simple-exp rands) (list k)))))))

(define cbindk
  (lambda (exp k)
    (let-exp (list k-id) (list k)
      (cps-of-expression exp k-var-exp))))

(define cps-of-letrec-exp
  (lambda (proc-names idss bodies letrec-body k)
    (if (genvar-exp? k)
      (letrec-exp
        proc-names
        (map
          (lambda (ids)
            (append ids (list k-id)))
          idss)
        (map
          (lambda (body)
            (cps-of-expression body k-var-exp))
          bodies)
        (cps-of-expression letrec-body k))
      (cbindk
        (letrec-exp proc-names idss bodies letrec-body)
        k))))

(define cps-of-let-exp
  (lambda (ids rands body k)
    (let ((pos (list-index non-simple? rands)))
      (if (number? pos)
        (let ((z-id (gensymbol "z")))
          (cps-of-expression
            (list-ref rands pos)
            (proc-exp (list z-id)
              (cps-of-expression
                (let-exp
                  ids
                  (list-set rands pos (genvar-exp z-id))
                body)
              k))))
        (if (genvar-exp? k)
          (let-exp ids (map cps-of-simple-exp rands)
            (cps-of-expression body k))
          (cbindk (let-exp ids rands body) k))))))

(define cps-of-print-exp
  (lambda (exp k)
    (if (non-simple? exp)
      (let ((v-id (gensymbol "v")))
        (cps-of-expression
          exp
          (proc-exp (list v-id)
            (printc-exp (genvar-exp v-id) k))))
      (printc-exp (cps-of-simple-exp exp) k))))

(define cps-of-throw-exp
  (lambda (value-exp cont-exp k)
    (if (non-simple? value-exp)
      (cps-of-throw-non-simple-value value-exp cont-exp k)
      (cps-of-throw-simple-value     value-exp cont-exp k))))

(define cps-of-throw-non-simple-value
  (lambda (value-exp cont-exp k)
    (let ((k-id (gensymbol "k")))
      (cps-of-expression value-exp
        (proc-exp (list k-id)
          (cps-of-expression 
            (throw-exp (genvar-exp k-id) cont-exp)
            k))))))

(define cps-of-throw-simple-value
  (lambda (value-exp cont-exp k)
    (if (non-simple? cont-exp)
      (cps-of-throw-simple-value-non-simple-cont value-exp cont-exp k)
      (csimple value-exp (cps-of-simple-exp cont-exp)))))

(define cps-of-throw-simple-value-non-simple-cont
  (lambda (value-exp cont-exp k)
    (let ((v-id (gensymbol "v")))
      (cps-of-expression cont-exp
        (proc-exp (list v-id)
          (cps-of-expression
            (throw-exp value-exp (genvar-exp v-id))
            k))))))

(define cps-of-letcc-exp
  (lambda (id body k)
    (if (genvar-exp? k)
      (let-exp (list id) (list k)
        (cps-of-expression body k))
      (cbindk (letcc-exp id body) k))))

(define cps-of-varassign-exp
  (lambda (id exp k)
    (if (non-simple? exp)
      (let ((v-id (gensymbol "v")))
        (cps-of-expression
          exp
          (proc-exp (list v-id)
            (varassignc-exp id (genvar-exp v-id) k))))
      (varassignc-exp id (cps-of-simple-exp exp) k))))

;;; **************** syntactic stuff ****************

(define non-simple?
  (lambda (exp)
    (cases expression exp
      (lit-exp (datum) #f)
      (var-exp (id) #t)
      (genvar-exp (id) #f)
      (primapp-exp (prim rands)
        (exists? non-simple? rands))
      (if-exp (test-exp true-exp false-exp)
        (or
          (non-simple? test-exp)
          (non-simple? true-exp)
          (non-simple? false-exp)))
      (let-exp (ids rands body)
        (or
          (exists? non-simple? rands)
          (non-simple? body)))
      (proc-exp (ids body) #f)
      (app-exp (rator rands) #t)
      (letrec-exp (proc-names idss bodies letrec-body)
        (non-simple? letrec-body))
      (print-exp (exp) #t)
      (varassign-exp (id exp) #t)
      (letcc-exp (id body) #t)
      (throw-exp (value-exp cont-exp) #t)
      (else (eopl:error 'non-simple?
              "shouldn't call non-simple? on non-source expression ~s"
              exp))
      )))

;; unparse: convert target code back to Scheme for easy evaluation.

(define unparse  
  (lambda (exp)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) id)
      (genvar-exp (id) id)
      (primapp-exp (prim rands)
        (cons (unparse-prim prim) (map unparse rands)))
      (if-exp (test-exp true-exp false-exp)
        (list 'if
          (unparse test-exp)
          (unparse true-exp)
          (unparse false-exp)))
      (let-exp (ids rands body)
        (list 'let (map
                     (lambda (id rand)
                       (list id (unparse rand)))
                     ids rands)
          (unparse body)))
      (proc-exp (ids body)
        (list 'lambda ids (unparse body)))
      (app-exp (rator rands)
        (cons (unparse rator) (map unparse rands)))
      (letrec-exp (proc-names idss bodies letrec-body)
        (list 'letrec
          (map
            (lambda (proc-name ids body)
              (list proc-name (list 'lambda ids (unparse body))))
            proc-names idss bodies)
          (unparse letrec-body)))
;      (print-exp (exp) (list 'write (unparse exp)))
      (print-exp (exp)
        (eopl:error 'unparse
          "print should not appear in target expression ~s"
          exp))
      (printc-exp (exp cont) (list 'printc (unparse exp) (unparse cont)))
      (letcc-exp (id body)
        (list 'letcc id (unparse body)))
      (throw-exp (value-exp cont-exp)
        (list 'throw (unparse value-exp) (unparse cont-exp)))
;      (varassign-exp (id exp) (list 'set! id (unparse exp)))
      (varassign-exp (id exp)
        (eopl:error 'unparse
          "varassign should not appear in target expression ~s"
          exp))
      (varassignc-exp (id exp cont)
        (list 'setc id (unparse exp) (unparse cont)))
      (derefc-exp (id cont)
        (list 'derefc id (unparse cont)))
      )))

(define unparse-prim
  (lambda (p)
    (cases primitive p
      (plus-prim () '+)
      (minus-prim () '-)
      (mult-prim () '*)
      (incr-prim () 'add1)
      (decr-prim () 'sub1)
      (zero?-prim () 'zero?)
      (car-prim () 'car)
      (cdr-prim () 'cdr)
      (cons-prim () 'cons)
      (null?-prim () 'null?)
      (empty-prim () 'empty)
      (list-prim () 'list)
      )))

;; for running the cps'd programs in Scheme
(define derefc (lambda (x k) (k x)))

(define-syntax setc
  (syntax-rules ()
    ((setc var val k)
     (begin (set! var val) (k (void))))))

;;;;;;;;;;;;;;;; miscellaneous ;;;;;;;;;;;;;;;;

(define list-index                      ; in srfi-1
  (lambda (pred ls)
    (cond 
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((ans (list-index pred (cdr ls))))
              (if (number? ans) (+ 1 ans) #f))))))

(define list-set                        ; definitely ours!
  (lambda (ls index new)
    (if (zero? index)
      (cons new (cdr ls))
      (cons (car ls) (list-set (cdr ls) (- index 1) new)))))

(define exists?                         ; srfi-1 calls this ANY
  (lambda (pred lv)
    (cond
      ((null? lv) #f)
      ((pred (car lv)) #t)
      (else (exists? pred (cdr lv))))))

;; **************** stuff from srfi-3 ****************

(define intersection                    ; srfi-3 calls this intersectionq
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (cons (car set1) (intersection (cdr set1) set2)))
      (else (intersection (cdr set1) set2)))))

(define union                           ; srfi-3 calls this unionq
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((memv (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(define setdiff                         ; srfi-3 calls this
                                        ; list-difference(q,v) 
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2) (setdiff (cdr set1) set2) )
      (else (cons (car set1) (setdiff (cdr set1) set2))))))

(define mapunion                        ; (foldr union '() sets)
  (lambda (f sets)
    (cond
      ((null? sets) '())
      (else (union (f (car sets)) (mapunion f (cdr sets)))))))
