;;; 8-4danvy.scm - one-pass conversion to cps

(let ((time-stamp "Time-stamp: <2000-12-19 15:40:30 wand>"))
  (eopl:printf "8-4danvy.scm - convert to cps in one pass ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level ;;;;;;;;;;;;;;;;

(define cps
  (lambda (string)
    (initialize-gensymbol-counter!)
    (unparse (cps-of-program (scan&parse string)))))

(define cps-all
  (lambda ()
    (run-experiment cps use-execution-outcome
      '(lang8-4) all-tests)))

(define cps-one
  (lambda (test-name)
    (run-test cps test-name)))

; check only for completion, sorry
(define equal-external-reps?
  (lambda (x y)                         
    #t))

;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

;; language of section 3.6

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
; no letcc or throw
;    (expression ("letcc" identifier "in" expression) letcc-exp) 
;    (expression ("throw" expression "to" expression) throw-exp) 
; no set
;     (expression ("set" identifier "=" expression) varassign-exp)

;; make primitives match unparse.
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

(define show-the-datatypes
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

;;; **************** gensym ****************

;;; need this here for init-k (sorry)

;; printed version

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

;;; **************** cps transformer ****************

(define k-id (gensymbol "k"))
(define k-var-exp (var-exp k-id))
(define new-ids
  (lambda (ids)
    (append ids (list k-id))))
(define var-exp?
  (lambda (exp)
    (cases expression exp
      (var-exp (id) #t)
      (else #f))))

(define possible-eta  
  (lambda (exp)
    (cases expression exp
      (proc-exp (ids body)
        (cases expression body
          (app-exp (rator rands)
            (if (singleton? rands)
              (cases expression (car rands)
                (var-exp (id)
                  (if (singleton? ids)
                    (if (eqv? (car ids) id) ;and not (occurs-free? id body)
                      rator
                      exp)
                    exp))
                (else exp))
              exp))
          (else exp)))
      (else exp))))

(define singleton?
  (lambda (ls)
    (and (not (null? ls))
         (null? (cdr ls)))))

(define non-simple?
  (lambda (exp)
    (cases expression exp
      (lit-exp (datum) #f)
      (var-exp (id) #f)
      (proc-exp (ids body) #f)
      (else #t))))

(define cps-of-program        
  (lambda (pgm)
    (cases program pgm
      (a-program (exp)
        (proc-exp (list k-id) (cps-of-tail-pos exp))))))

(define cps-of-expression 
  (lambda (exp k)
    (if (non-simple? exp)
      (cases expression exp
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
;         (letcc-exp (id body) (cps-of-letcc-exp id body k))
;         (throw-exp (value-exp cont-exp)
;           (cps-of-throw-exp value-exp cont-exp k))
        (else
          (eopl:error 'cps-of-expression
            "Can't call on  ~s" exp)))
      (csimple exp k)
      )))

(define cps-of-simple-exp 
  (lambda (exp)
    (cases expression exp
      (proc-exp (ids body)
        (proc-exp (new-ids ids)
          (cps-of-tail-pos body)))
      (lit-exp (datum) (lit-exp datum))
      (var-exp (id) (var-exp id))
      (else
        (error 'cps-of-simple-exp
          "Only procedures are simple:~s" exp)))))

;;; ***** new stuff ******
;------------------------------------------------------
(define csimple     
  (lambda (exp k)
    (k (cps-of-simple-exp exp))))

(define cps-of-tail-pos
  (lambda (exp)
    (cps-of-expression exp
      (lambda (res)
        (app-exp k-var-exp (list res))))))

(define cps-of-app-exp  
  (lambda (rator rands k)
    (let ((cont-exp
            (let ((v-id (gensymbol "v")))
              (proc-exp (list v-id)
                (k (var-exp v-id))))))
      (cps-of-expression rator
        (lambda (rator-res)
          (cps-of-rands rands
            (lambda (rands-res)
              (app-exp rator-res
                (append rands-res
                  (list cont-exp))))))))))

(define cps-of-rands    
  (lambda (rands k)
    (if (null? rands)
      (k '())
      (cps-of-expression (car rands)
        (lambda (rand-res)
          (cps-of-rands (cdr rands)
            (lambda (rands-res)
              (k (cons rand-res rands-res)))))))))

(define cps-of-let-exp  
  (lambda (ids rands body k)
    (let ((cont-exp
            (let ((v-id (gensymbol "v")))
              (proc-exp (list v-id)
                (k (var-exp v-id))))))
      (cbindk (cps-of-rands rands
                (lambda (rands-res)
                  (let-exp ids rands-res
                    (cps-of-tail-pos body))))
        cont-exp))))
;------------------------------------------------------------
(define cps-of-if-exp   
  (lambda (test-exp true-exp false-exp k)
    (let ((cont-exp
            (let ((v-id (gensymbol "v")))
              (proc-exp (list v-id)
                (k (var-exp v-id))))))
      (cbindk
        (cps-of-expression test-exp
          (lambda (test-exp-res)
            (if-exp test-exp-res
              (cps-of-tail-pos true-exp)
              (cps-of-tail-pos false-exp))))
        cont-exp))))

(define cps-of-primapp-exp
  (lambda (prim rands k)
    (cps-of-rands rands
      (lambda (rands-res)
        (k (primapp-exp prim rands-res))))))

(define cbindk
  (lambda (exp cont-exp)
    (let-exp (list k-id) (list cont-exp) exp)))

(define cps-of-letrec-exp 
  (lambda (proc-names idss bodies letrec-body k)
    (let ((cont-exp
            (let ((v-id (gensymbol "v")))
              (proc-exp (list v-id) (k (var-exp v-id))))))
      (cbindk
        (letrec-exp
          proc-names
          (map new-ids idss)
          (map cps-of-tail-pos bodies)
          (cps-of-tail-pos letrec-body)) cont-exp))))

(define cps-of-letcc-exp  
  (lambda (id body k)
    (let ((cont-exp
            (let ((v-id (gensymbol "v")))
              (proc-exp (list v-id) (k (var-exp v-id))))))
      (let ((exp (let-exp (list id) (list k-var-exp)
                   (cps-of-tail-pos body))))
        (cbindk exp cont-exp)))))

(define cps-of-throw-exp 
  (lambda (value-exp cont-exp k)
    (cps-of-expression value-exp
      (lambda (value-exp-res)
        (cps-of-expression cont-exp
          (lambda (cont-exp-res)
            (app-exp cont-exp-res (list value-exp-res))))))))

;;; **************** syntactic stuff ****************

(define unparse  ;;; The quasiquote had to be removed to work on
                 ;;; MzScheme.
                 ;;; Mitch sez: nah, you've just got find the right library.
  (lambda (exp)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) id)
      (primapp-exp (prim rands)
        (cons (unparse-prim prim) (map unparse rands)))
      (if-exp (test-exp true-exp false-exp)
        (list 'if
          (unparse test-exp)
          (unparse true-exp)
          (unparse false-exp)))
      (let-exp (ids rands body)
        (list 'let (map (lambda (id rand)
                          (list id (unparse rand)))
                     ids rands)
          (unparse body)))
      (proc-exp (ids body)
        (list 'lambda ids (unparse body)))
      (app-exp (rator rands)
        (cons (unparse rator) (map unparse rands)))
      (letrec-exp (proc-names idss bodies letrec-body)
        (list 'letrec
          (map (lambda (proc-name ids body)
                 (list proc-name (list 'lambda ids (unparse body))))
            proc-names idss bodies)
          (unparse letrec-body)))
;       (letcc-exp (id body)
;         (list 'letcc id (unparse body)))
;       (throw-exp (value-exp cont-exp)
;         (list 'throw (unparse value-exp) (unparse cont-exp)))
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

;;; **************** tests ****************


(define pgma "proc (f,x) (f x)")

(define pgmb "proc (f,g,x) (f (g x))")

(define pgmc "proc (x,y) +(x,y)")

(define pgmd "proc (f,g,x) (f sub1((g zero?(x))))")

(define pgme "(f let a = 1 in 2)")

(define pgmf "+(x, let a = 1 in 2)")

(define pgmg  "letrec
  fact(x) = 7
in 6")

(define pgmh  "letrec
  fact(x) = x
in (fact 6)")

(define pgmi  "letrec
  fact(x) = *(x, (fact sub1(x)))
in (fact 6)")

(define pgmj1 "letcc j in +(4, 3)")

(define pgmj2 "+(1, letcc j in +(4, 3))")

(define pgmk1
  "letcc j in +(4, throw (f x) to car (cons (j, emptylist())))")

(define pgmk2
  "+(1, letcc j in +(4, throw (f x) to car (cons (j, emptylist()))))")

(define pgmm1 "letcc j in +(4, throw (f x) to (proc (x) x j))")

(define pgmm2 "+(1, letcc j in +(4, throw (f x) to (proc (x) x j)))")

(define pgmn1 "letcc j in +(4, throw (f x) to j)")

(define pgmn2 "+(1, letcc j in +(4, throw (f x) to j))")

;;;;;;;;;;

(define pgm1  "letrec
  fact(x) = if zero?(x) then 1 else *(x, (fact sub1(x)))
in (fact 6)")

(define pgm2  "letrec
         even(x) = if zero?(x) then 1 else (odd sub1(x))
         odd(x)  = if zero?(x) then 0 else (even sub1(x))
       in (odd 13)")

(define pgm3
  "letrec even(odd,x) =  if zero?(x) then 1 else (odd sub1(x))
   in letrec  odd(x)  = if zero?(x) then 0 else (even odd sub1(x))
   in (odd 13)")

(define pgm4 "+(1, letcc j in +(4, throw 3 to j))")

(define pgm5 "add1(if (f zero?(x)) then 1 else *(x, (fact sub1(x))))")

(define pgm6
  '(let ((a (f x)) (b (g x)) (c (h (i (j (t x))))))
     (cons a (cons b (cons c (empty))))))

(define pgm6 "
let a = (f x) b = (g x) c = (h (i (j (t x))))
in cons(a,cons(b,cons(c,emptylist())))")

;; curried map, using poor-man's Y
(define pgm7
  '(proc (f)
     (let ((foo (proc (foo ls)
                  (if (null? ls)
                    (empty)
                    (cons (f (car ls)) (foo foo (cdr ls)))))))
       (proc (ls) (foo foo ls)))))

(define pgm7 "
proc(f) let foo = proc(foo,ls) 
                   if null?(ls)
                   then emptylist()
                   else cons((f car(ls)), (foo foo cdr(ls)))
        in proc(ls)(foo foo ls)")

; a let that tries to capture a variable from the continuation.
(define pgm8
  '(f (let ((f 3)) (+ f 1))))

(define pgm8 "
 (f let f = 3 in add1(f))")

(define pgm9
  '(k (g (let ((g 3)) (f g x)))))

(define pgm9 "(k (g let g=3 in (f g x)))")

(define pgm10
  '(k (g (letcc g (f (g x))))))

(define pgm10 "let x = 4 g = proc(n)*(n,n) in (g letcc g in (f throw x to g))")

(define pgm11
  '(even
     (letrec
       ((even (x) (if (zero? x) 1 (odd (sub1 x))))
        (odd (x) (if (zero?) 0 (even (sub1 x)))))
       (odd 13))))

(define pgm11 "  (even
  letrec
    even(x) = if zero?(x) then 1 else (odd sub1(x))
    odd(x)  = if zero?(x) then 0 else (even sub1(x))
  in (odd 13))" )

(define test-all
  (lambda ()
    (for-each
      (lambda (pgm)
        (eopl:pretty-print pgm)
        (newline)
        (eopl:pretty-print (run pgm))
        (newline))
      (list pgma pgmb pgmc pgmd pgme pgmf pgmg pgmh pgmi pgmj1 pgmj2
        pgmk1 pgmk2 pgmm1 pgmm2 pgmn1 pgmn2
        pgm1 pgm2 pgm3 pgm4 pgm5 pgm6 pgm7 pgm8 pgm9 pgm11))))

;; pgm6 pgm7 pgm8 pgm9 pgm10 pgm11))))
