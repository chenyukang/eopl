;;; eight-5.scm - conversion to cps

(let ((time-stamp "Time-stamp: <2000-12-08 10:46:38 dfried>"))
  (eopl:printf "eight-4.scm - convert to cps ~a~%"
    (substring time-stamp 13 29)))

;; grammar

(define lex7
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number   (digit (arbno digit)) number)))

;; language of section 3.6, plus letcc.

(define gram7
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
    (expression                         ; 3-6
      ("letrec"
        (arbno identifier "(" (separated-list identifier ",") ")"
          "=" expression)
        "in" expression)
      letrec-exp)
    (expression ("letcc" identifier "in" expression) letcc-exp) ; new
    (expression ("throw" expression "to" expression) throw-exp) ; new in 9-2
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

(sllgen:make-define-datatypes lex7 gram7)

(define scan&parse
  (sllgen:make-string-parser lex7 gram7))

(define just-scan
  (sllgen:make-string-scanner lex7 gram7))

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lex7 gram7)))

;;; **************** gensym ****************

;;; need this here for init-k (sorry)

(define gensymbol
  (let ((n 0))
    (lambda (s)
      (set! n (+ n 1))
      (let ((s (if (string? s) s (symbol->string s))))
        (string->symbol
          (string-append s (number->string n)))))))

(define var-exp?
  (lambda (x)
    (cases expression x
      (var-exp (id) #t)
      (else #f))))

;;; **************** top-level ****************

(define run
  (lambda (string)
    (unparse (cps-of-program (scan&parse string)))))

;;; **************** cps transformer ****************

(define k-id (gensymbol "k"))

(define k-var-exp (var-exp k-id))

(define cps-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp) 
        (proc-exp (list k-id)
          (cps-of-expression exp k-var-exp))))))

(define cps-of-simple-exp ;^ translation for simple expressions
  (lambda (exp)
    (cases expression exp
      (lit-exp (datum) (lit-exp datum))
      (var-exp (id) (var-exp id))
      (primapp-exp (prim rands) ;^ rands are known to be simple
        (primapp-exp prim (map cps-of-simple-exp rands)))
      (if-exp (test-exp true-exp false-exp)
        (if-exp
          (cps-of-simple-exp test-exp)
          (cps-of-simple-exp true-exp)
          (cps-of-simple-exp false-exp)))
      (let-exp (ids rands body)
        (let-exp ids
          (map cps-of-simple-exp rands)
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
      (app-exp (rator rands)
        (eopl:error 'cps-of-simple-exp
          "Can't call on application ~s" exp))
      (letcc-exp (id body)
        (eopl:error 'cps-of-simple-exp
          "Can't call on letcc ~s"
          exp))
      (throw-exp (value cont)
        (eopl:error 'cps-of-simple-exp
          "Can't call on throw ~s"
          exp))
      )))

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
          (cps-of-letrec-exp
            proc-names idss bodies letrec-body k))
        (letcc-exp (id body) (cps-of-letcc-exp id body k))
        (throw-exp (value-exp cont-exp)
          (cps-of-throw-exp value-exp cont-exp k))
        (else
          (eopl:error 'cps-of-expression
            "Can't call on  ~s" exp))
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
              (if-exp (var-exp v-id) true-exp false-exp)
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
            (primapp-exp prim
              (list-set rands pos (var-exp v-id)))
            k))))))

(define cps-of-app-exp
  (lambda (rator rands k)
    (if (non-simple? rator)
      (let ((v-id (gensymbol "v")))
        (cps-of-expression rator
          (proc-exp (list v-id)
            (cps-of-expression
              (app-exp (var-exp v-id) rands)
              k))))
      (cps-of-app-exp-simple-rator rator rands k))))

(define cps-of-app-exp-simple-rator
  (lambda (rator rands k)
    (let ((pos (list-index non-simple? rands)))
      (if (number? pos)
        (let ((v-id (gensymbol "v")))
          (cps-of-expression
            (list-ref rands pos)
            (proc-exp (list v-id)
              (cps-of-expression
                (app-exp rator
                  (list-set rands pos (var-exp v-id)))
                k))))
        (app-exp (cps-of-simple-exp rator)
          (append
            (map cps-of-simple-exp rands)
            (list k)))))))

(define cbindk
  (lambda (exp k)
    (let-exp (list k-id) (list k)
      (cps-of-expression exp k-var-exp))))

(define cps-of-letrec-exp
  (lambda (proc-names idss bodies letrec-body k)
    (if (var-exp? k)
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
    (if (var-exp? k)
      (let ((pos (list-index non-simple? rands)))
        (if (number? pos)
          (let ((z-id (gensymbol "z")))
            (cps-of-expression
              (list-ref rands pos)
              (proc-exp (list z-id)
                (cps-of-expression
                  (let-exp ids
                    (list-set rands pos (var-exp z-id))
                    body)
                  k))))
          (let-exp ids (map cps-of-simple-exp rands)
            (cps-of-expression body k))))
      (cbindk (let-exp ids rands body) k))))

(define cps-of-throw-exp
  (lambda (value-exp cont-exp k)
    (if (non-simple? cont-exp)
      (cps-of-throw-non-simple-value value-exp cont-exp k)
      (cps-of-throw-simple-value     value-exp cont-exp k))))

(define cps-of-throw-non-simple-value
  (lambda (value-exp cont-exp k)
    (let ((new-var (gensymbol "k")))
      (cps-of-expression value-exp
        (proc-exp (list new-var)
          (cps-of-expression 
            (throw-exp new-var cont-exp)
            k))))))

(define cps-of-throw-simple-value
  (lambda (value-exp cont-exp k)
    (if (non-simple? cont-exp)
      (cps-of-throw-simple-value-non-simple-cont value-exp cont-exp k)
      (app-exp
        (cps-of-simple-exp cont-exp)
        (list (cps-of-simple-exp value-exp))))))

(define cps-of-throw-simple-value-non-simple-cont
  (lambda (value-exp cont-exp k)
    (let ((new-var (gensymbol "v")))
      (cps-of-expression cont-exp
        (proc-exp (list new-var)
          (cps-of-expression
            (throw-exp value-exp new-var)
            k))))))

(define cps-of-letcc-exp
  (lambda (id body k)
    (if (var-exp? k)
      (let-exp (list id) (list k)
        (cps-of-expression body k))
      (cbindk (letcc-exp id body) k))))


;;; **************** syntactic stuff ****************

(define non-simple?
  (lambda (exp)
    (cases expression exp
      (lit-exp (datum) #f)
      (var-exp (id) #f)
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
      (letcc-exp (id body) #t)
      (throw-exp (value-exp cont-exp) #t)
      )))

; (define simple?
;   (lambda (exp)
;     (not (non-simple? exp))))

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
      (letcc-exp (id body)
        (list 'letcc id (unparse body)))
      (throw-exp (value-exp cont-exp)
        (list 'throw (unparse value-exp) (unparse cont-exp)))
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

;;; **************** even more misc stuff ****************

;; supplied in sllgen
; (define list-of
;   (lambda (pred)
;     (lambda (ls)
;       (and (list? ls) (andmap pred ls)))))


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


;;; **************** tests ****************


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
      (list pgm1 pgm2 pgm3 pgm4 pgm5 pgm6 pgm7 pgm8 pgm9 pgm11))))

;; pgm6 pgm7 pgm8 pgm9 pgm10 pgm11))))
