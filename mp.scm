(load-relative "../libs/init.scm")

(define (contains? l x)
  (if (equal? l '())
      #f
      (or
       (equal? (car l) x)
       (contains? (cdr l) x))))

(define fold-left
  (lambda (function the-list)
    (cond
     ((eqv? (length the-list) 1)
      (function (car the-list)))
     ((eqv? (length the-list) 2)
      (function (car the-list) (cadr the-list)))
     (else
      (let ([lhs (car the-list)]
            [rhs (cadr the-list)])
        (fold-left function (cons (function lhs rhs) (cddr the-list))))))))

(define replicate
  (lambda (element n)
    (cond
     ((zero? n)
      '())
     (else
      (cons element (replicate element (- n 1)))))))

(define list-last
  (lambda (l)
    (if (equal? (length l) 1)
        (car l)
        (list-last (cdr l)))))


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
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression
      ("=" "(" expression "," expression ")")
      equal-exp)

     (expression
      ("<" "(" expression "," expression ")")
      less-exp)

     (expression
      (">" "(" expression "," expression ")")
      large-exp)

     (expression
      ("-" "(" (separated-list expression ",") ")")
      diff-exp)

     (expression
      ("+" "(" (separated-list expression ",") ")")
      sum-exp)

     (expression
      ("*" "(" (separated-list expression ",") ")")
      mul-exp)

     (expression
      ("/" "(" (separated-list expression ",") ")")
      div-exp)

     (expression
      ("(set" expression expression ")")
      set-exp)

     (expression
      ("if" expression "then" expression "else" expression "end")
      if-exp)

     (expression
      ("begin" expression ";" (arbno expression ";") "end")
      begin-exp)

     ;; (expression
     ;;  (obj-expression (arbno "." identifier))
     ;;  obj-call-exp)

     ;; (obj-expression (identifier) var-exp)

     (expression
      ("let" (arbno identifier "=" expression) "in" expression "end")
      let-exp)

     (expression (identifier) var-exp)

     (expression
      ("proc" "(" (arbno identifier) ")" expression "end")
      proc-exp)

     (expression
      ("(" expression (arbno expression) ")")
      call-exp)

     (expression
      ("letrec"
       (arbno identifier "=" expression)
       "in" expression "end")
      letrec-exp)


     ;; (obj-expression
     ;;  ("extend" expression "with" (arbno member-decl))
     ;;  extend-exp)

     ;; (member-decl
     ;;  ("public" identifier "=" expression ";")
     ;;  a-member-public-decl)

     ;; (member-decl
     ;;  ("protected" identifier "=" expression ";")
     ;;  a-member-protect-decl)

     ;; (obj-expression
     ;;  ("super")
     ;;  super-exp)

     ;; (obj-expression
     ;;  ("self")
     ;;  self-exp)
     ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-specq the-grammar)))

(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

;;;;;;;;;;;;;;;; syntactic operations on types ;;;;;;;;;;;;;;;;
;;; datetype ;;;
(define-datatype expression expression?
  (var-exp
   (var identifier?))
  (const-exp
   (num number?))
  (set-exp
   (arg1 expression?)
   (arg2 expression?))
  (if-exp
   (predicate-exp expression?)
   (test-true-exp expression?)
   (teset-false-exp expression?))
  (diff-exp
   (args (list-of expression?)))
  (sum-exp
   (args (list-of expression?)))
  (mul-exp
   (args (list-of expression?)))
  (div-exp
   (args (list-of expression?)))
  (equal-exp
   (arg1 expression?)
   (arg2 expression?))
  (large-exp
   (arg1 expression?)
   (arg2 expression?))
  (less-exp
   (arg1 expression?)
   (arg2 expression?))
  (begin-exp
   (first expression?)
   (rest (list-of expression?)))
  (proc-exp
   (vars (list-of identifier?))
   (body expression?))
  (let-exp
   (vars (list-of identifier?))
   (vals (list-of expression?))
   (exp expression?))
  (letrec-exp
   (vars (list-of identifier?))
   (vals (list-of expression?))
   (exp expression?))
  (call-exp
   (proc expression?)
   (body (list-of expression?))))

(define-datatype expval expval?
  (num-val
   (value number?))
  (bool-val
   (boolean boolean?))
  (proc-val
   (proc proc?)))

;;; extractors:

;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (v)
    (cases expval v
           (num-val (num) num)
           (else (expval-extractor-error 'num v)))))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (v)
    (cases expval v
           (bool-val (bool) bool)
           (else (expval-extractor-error 'bool v)))))

;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (v)
    (cases expval v
           (proc-val (proc) proc)
           (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (error 'expval-extractors "Looking for a ~s, found ~s"
           variant value)))


;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;
(define-datatype proc proc?
  (procedure
   (bvar (list-of symbol?))
   (body expression?)
   (env environment?)))
                                        ;
(define empty-env
  (lambda ()
    (lambda (search-var)
      'not-found)))

(define extend-env
  (lambda (saved-env saved-var saved-val)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))))

(define extend-env-dynamic
  (lambda (env closure-env)
    (lambda (search-var)
      (let ([temp (apply-env env search-var)])
        (if (equal? temp 'not-found)
            (apply-env closure-env search-var)
            temp)))))
                                        ;
(define multiarg-extend-env
  (lambda (env vars vals)
    (if (equal? vars '())
        env
        (let ([env-temp (extend-env env (car vars) (car vals))])
          (multiarg-extend-env env-temp (cdr vars) (cdr vals))))))

(define extend-env-rec
  (lambda (env ids)
    (letrec ([myself
              (lambda (search-id)
                (if (contains? ids search-id)
                    (let ([val (apply-env env search-id)])
                      (if (closure-type? val)
                          (cases closure-type val
                                 (closure (params body closure-env)
                                          (closure params body myself)))
                          val))
                    (apply-env env search-id)))])
      myself)))

(define apply-env
  (lambda (env search-var)
    (env search-var)))

(define closure-body-checker (lambda (x) #t))

(define set-closure-body-checker
  (lambda (x)
    (set! closure-body-checker x)))
                                        ;
(define-datatype closure-type closure-type?
  (closure
   (args (list-of symbol?))
   (body closure-body-checker)
   (env procedure?)))


;; (define-datatype environment environment?
;;   (empty-env)
;;   (extend-env
;;    (bvar symbol?)
;;    (bval expval?)
;;    (saved-env environment?))
;;   (extend-env-rec*
;;    (proc-names (list-of symbol?))
;;    (b-vars (list-of symbol?))
;;    (proc-bodies (list-of expression?))
;;    (saved-env environment?)))

(define init-env
  (lambda ()
    (extend-env
     'true (bool-val #t)
     (extend-env
      'false (bool-val #f)
      (empty-env)))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;
;; (define apply-env
;;   (lambda (env search-sym)
;;     (cases environment env
;;            (empty-env ()
;;                       (error 'apply-env "No binding for ~s" search-sym))
;;            (extend-env (bvar bval saved-env)
;;                        (if (eqv? search-sym bvar)
;;                            bval
;;                            (apply-env saved-env search-sym)))
;;            (extend-env-rec* (p-names b-vars p-bodies saved-env)
;;                             (cond
;;                              ((location search-sym p-names)
;;                               => (lambda (n)
;;                                    (proc-val
;;                                     (procedure
;;                                      (list-ref b-vars n)
;;                                      (list-ref p-bodies n)
;;                                      env))))
;;                              (else (apply-env saved-env search-sym)))))))

;; (define apply-env
;;   (lambda (env search-sym)
;;     (cases environment env
;;            (empty-env ()
;;                       (error 'apply-env "No binding for ~s" search-sym))
;;            (extend-env (bvar bval saved-env)
;;                        (if (eqv? search-sym bvar)
;;                            bval
;;                            (apply-env saved-env search-sym)))
;;            (extend-env-rec* (p-names b-vars p-bodies saved-env)
;;                             (cond
;;                              ((location search-sym p-names)
;;                               => (lambda (n)
;;                                    (let ((body (list-ref p-bodies n)))
;;                                      (begin
;;                                        (printf "return now: ~s~%" body)
;;                                        (cases proc body
;;                                               (procedure bvar pbody penv
;;                                                          (proc-val
;;                                                           (procedure bvar
;;                                                                      pbody
;;                                                                      env))))))))
;;                              (else (apply-env saved-env search-sym)))))))


;; (define location
;;   (lambda (sym syms)
;;     (cond
;;      ((null? syms) #f)
;;      ((eqv? sym (car syms)) 0)
;;      ((location sym (cdr syms))
;;       => (lambda (n)
;;            (+ n 1)))
;;      (else #f))))

;; ;; new stuff
;; (define extend-env*
;;   (lambda (vars vals env)
;;     (if (null? vars)
;;         env
;;         (extend-env (car vars) (car vals)
;;                     (extend-env* (cdr vars) (cdr vals) env)))))


;; apply-procedure : Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 args)
    (begin
      (printf "apply-procedure: ~s ~s~%" proc1 args)
    (cases proc proc1
           (procedure (vars body saved-env)
                      (let ((new-env (extend-env* vars args saved-env)))
                        (value-of body new-env)))))))


;; value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
           (a-program (exp1)
                      (value-of exp1 (init-env))))))

(define args-val
  (lambda (vals env)
    (let ((arg-vals
           (map (lambda(x) (value-of x env)) vals)))
      (map (lambda(x) (expval->num x)) arg-vals))))

(define value-of-ops-iter
  (lambda (op now vals)
    (if (null? vals)
          now
          (value-of-ops-iter op (op now (car vals))
                        (cdr vals)))))

(define value-of-ops
  (lambda (op vals)
    (value-of-ops-iter op (car vals) (cdr vals))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (begin
      (printf "now: ~s~%" exp)
    (cases expression exp
           (const-exp (num)
                      (begin
                        (printf "xx: ~s~%" num)
                        (num-val num)))

           (var-exp (var) (apply-env env var))

           (equal-exp (exp1 exp2)
                       (let ((val1 (value-of exp1 env))
                             (val2 (value-of exp2 env)))
                         (let ((num1 (expval->num val1))
                               (num2 (expval->num val2)))
                           (bool-val
                            (= num1 num2)))))

           (less-exp (exp1 exp2)
                      (let ((val1 (value-of exp1 env))
                            (val2 (value-of exp2 env)))
                        (let ((num1 (expval->num val1))
                              (num2 (expval->num val2)))
                          (bool-val
                           (< num1 num2)))))
           (large-exp (exp1 exp2)
                         (let ((val1 (value-of exp1 env))
                               (val2 (value-of exp2 env)))
                           (let ((num1 (expval->num val1))
                                 (num2 (expval->num val2)))
                             (bool-val
                              (> num1 num2)))))

           (set-exp (arg1 arg2)
                    (bool-val #t))

           ;; (obj-call-exp (args)
           ;;               (bool-val #t))

           (diff-exp (args)
                     (let ((arg-vals (args-val args env)))
                       (num-val (value-of-ops - arg-vals))))

           (sum-exp (args)
                     (let ((arg-vals (args-val args env)))
                       (num-val (value-of-ops + arg-vals))))

           (mul-exp (args)
                     (let ((arg-vals (args-val args env)))
                       (num-val (value-of-ops * arg-vals))))

           (div-exp (args)
                     (let ((arg-vals (args-val args env)))
                       (num-val (value-of-ops / arg-vals))))

           (if-exp (exp1 exp2 exp3)
                   (let ((val1 (value-of exp1 env)))
                     (if (expval->bool val1)
                         (value-of exp2 env)
                         (value-of exp3 env))))

           (begin-exp (first rest)
                      (let ((first-val (value-of first env)))
                        (if (null? rest)
                            first-val
                            (value-of (begin-exp (car rest) (cdr rest))
                                      env))))
           (proc-exp (vars body)
                     (proc-val (procedure vars body env)))

           (call-exp (rator rands)
                     (let ((proc (expval->proc (value-of rator env)))
                           (args (map (lambda(x)
                                        (value-of x env)) rands)))
                       (apply-procedure proc args)))

           (let-exp (vars exps body)
                    (value-of body
                              (extend-env* vars (map (lambda(x)
                                                       (value-of x env))
                                                     exps) env)))
           (letrec-exp (vars bodies letrec-body)
                       (value-of letrec-body
                                 (extend-env-rec (get-last-env vars bodies env) vars)))
           ))))

(define get-last-env
  (lambda (vars exprs env)
    (cond
     ((equal? vars '())
      env)
     (else
      (let ([value (value-of (car exprs) env)])
        (get-last-env (cdr vars) (cdr exprs) (extend-env env (car vars) value)))))))


(define run
  (lambda (string)
    (value-of-program (scan&parse string))))


(scan&parse "true")
(scan&parse "1")
(scan&parse "+(1, 2, 3, 5)")
(scan&parse "-(10, 1, 1, 5)")
(scan&parse "+(1, 8)")
(scan&parse "begin 1; end")
(scan&parse "begin 1; 2; end")
(scan&parse "(proc (x) x end 1)")
(scan&parse "( proc (x y) +(x, y) end 1 2 )")
(scan&parse "let x = 1 in x end")
(scan&parse "(let f = proc (x y) +(x,y) end in f end 1 2)")
(scan&parse "let plus = proc (x y) +(x,y) end
           minus = proc (x y) -(x, y) end
        in
          begin
            (plus 1 2);
            (minus 4 1);
          end
       end")

(scan&parse "=(10, 10)")
(scan&parse "<(0, 10)")
(scan&parse "<(10, 1)")
(scan&parse ">(10, 2)")
(scan&parse ">(1, 20)")
(scan&parse "if true then 1 else 0 end")
(scan&parse "if <(2, 1) then true else false end")
(scan&parse "if >(10, 1) then true else false end")
(scan&parse "if >(1, 10) then true else false end")
(scan&parse "if =(1, 1) then true else false end")
(scan&parse "if =(1, 2) then true else false end")

(run "true")
(run "1")           ;; => 1
(run "+(1, 2, 3, 5)") ;; =>  11
(run "-(10, 1, 1, 5)") ;; =>  3
(run "+(1, 8)") ;; => 9
(run "-(10, 5)") ;; => 5
(run "begin 1; end") ;; => 1
(run "begin 1; 2; end") ;; => 2
(run "(proc (x) x end 1)") ;; => 1
(run "( proc (x y) +(x, y) end 1 2 )") ;; =>  3
(run "let x = 1 in x end") ;; => 1
(run "(let f = proc (x y) +(x,y) end in f end 1 2)") ;; => 3

(run "let plus = proc (x y) +(x,y) end
           minus = proc (x y) -(x, y) end
        in
          begin
            (plus 1 2);
            (minus 4 1);
          end
       end") ;; => 3

(run "=(10, 10)")
(run "<(0, 10)")
(run "<(10, 1)")
(run ">(10, 2)")
(run ">(1, 20)")
(run "if true then 1 else 0 end") ;; => 1

(run "if <(2, 1) then true else false end") ;; => false
(run "if >(10, 1) then true else false end") ;; => true
(run "if >(1, 10) then true else false end") ;; => false
(run "if =(1, 1) then true else false end")  ;; => true
(run "if =(1, 2) then true else false end")  ;; => false


(run "if true then (let f = proc (x) x end in f end 2) else false end") ;; => 2

(run "let x = 1 in (proc (f) (f x) end
                      proc (x) +(x, 1) end) end") ;; => 2

(scan&parse "letrec fact = proc(n)
                       if =(n, 0) then 1 else *(n, (fact -(n, 1))) end end
         in (fact 5) end")

(run "letrec fact = proc(n)
                       if =(n, 0) then 1 else *(n, (fact -(n, 1))) end end
         in (fact 5) end")
;;Expected output: 120

(run "letrec even =
                proc (n)
                  if =(n, 0)
                    then true
                    else (odd -(n, 1))
                  end
                end
              odd =
                proc (n)
                  if =(n, 0)
                    then false
                    else (even -(n, 1))
                  end
                end
       in (even 0) end")
;; Expected output: true

(scan&parse "letrec even =
                proc (n)
                  if =(n, 0)
                    then true
                    else (odd -(n, 1))
                  end
                end
              odd =
                proc (n)
                  if =(n, 0)
                    then false
                    else (even -(n, 1))
                  end
                end
       in (even 1) end")
;;Expected output: false

(scan&parse "letrec even =
                proc (n)
                  if =(n, 0)
                    then true
                    else (odd -(n, 1))
                  end
                end
              odd =
                proc (n)
                  if =(n, 0)
                    then false
                    else (even -(n, 1))
                  end
                end
       in (even 10) end")
;;Expected output: true

(scan&parse "letrec even =
                proc (n)
                  if =(n, 0)
                    then true
                    else (odd -(n, 1))
                  end
                end
              odd =
                proc (n)
                  if =(n, 0)
                    then false
                    else (even -(n, 1))
                  end
                end
       in (even 11) end")
;;Expected output: false

(scan&parse "letrec
        f3 =
          proc (n)
            if =(n, 0)
              then 3
              else (f2 -(n, 1))
            end
          end
        f2 =
          proc (n)
            if =(n, 0)
              then 2
              else (f1 -(n, 1))
            end
          end
       f1 =
          proc (n)
            if =(n, 0)
              then 1
              else (f3 -(n, 1))
            end
          end
      in (f3 3) end")
;;Expected output: 3

(scan&parse "letrec
        f3 =
          proc (n)
            if =(n, 0)
              then 3
              else (f2 -(n, 1))
            end
          end
        f2 =
          proc (n)
            if =(n, 0)
              then 2
              else (f1 -(n, 1))
            end
          end
       f1 =
          proc (n)
            if =(n, 0)
              then 1
              else (f3 -(n, 1))
            end
          end
      in (f3 0) end")
;;Expected output: 3

(scan&parse "letrec
        f3 =
          proc (n)
            if =(n, 0)
              then 3
              else (f2 -(n, 1))
            end
          end
        f2 =
          proc (n)
            if =(n, 0)
              then 2
              else (f1 -(n, 1))
            end
          end
       f1 =
          proc (n)
            if =(n, 0)
              then 1
              else (f3 -(n, 1))
            end
          end
      in (f3 10) end")
;;Expected output: 2

(scan&parse "super")

(scan&parse "let x = 1
       in begin
            (set x 2) ;
            x ;
          end
       end")
;;Expected output: 2

(scan&parse "let x = 1
           y = x
       in
         begin
           (set y 2) ;
           x ;
         end
       end")
;;Expected output: 1

(scan&parse "let x = 1
           y = x
       in
         begin
           (set x 2) ;
           y ;
         end
       end")
;;Expected output: 1

(scan&parse "let x = 1 in
         begin
           (set x proc () 5 end) ;
           (x) ;
         end
       end")
;;Expected output: 5

(scan&parse "let x = 1
           y = 5 in
         begin
           (set x proc () y end) ;
           (x) ;
         end
       end")
;;Expected output: 5

(scan&parse "let a = extend EmptyObj with
                  public x = 1 ;
              in a.x end")


(scan&parse "let a = extend EmptyObj with
         public b = extend EmptyObj with
           public c = extend EmptyObj with
              public x = 1;;; in a.b.c.x end")
;;Expected output: 1

(scan&parse "let a = extend EmptyObj with
               public x = 1;
           b = extend a with
               public y = 2;
           c = extend b with
               public z = 3;
       in
        c.z
       end")
;;Expected output: 3


(scan&parse "let a = extend EmptyObj with
               public x = 1;
           b = extend a with
       in
         b.x
       end")
;;Expected output: 1

(scan&parse "let obj =
          extend
              extend
                  extend EmptyObj with public getVal = proc() 3 end;
              with public getVal = proc() 5 end; public getSuper = proc() (super.getVal) end;
          with public getSuper = proc() (super.getSuper) end;
      in (obj.getSuper) end")
;;Expected output: 3

(scan&parse "let z = 1 in
        let a = extend EmptyObj with public x = z; in
          a.x
        end
       end")
;;Expected output: 1

(scan&parse "let a = extend EmptyObj with
               public x = 1 ;
               public getX = proc () self.x end ;
       in
         (a.getX)
       end")
;;Expected output: 1

(scan&parse "let fobj = extend EmptyObj with
                  public fact = proc (n)
                    if =(n, 0)
                     then 1
                     else *(n, (self.fact -(n,1)))
                    end
                  end ;
       in
         (fobj.fact 5)
       end")
;;Expected output: 120

(scan&parse "let a = extend EmptyObj with
               public m1 = proc () (self.m2) end;
               public m2 = proc () 1 end ;

           b = extend a with
               public m1 = proc () (super.m1) end ;
               public m2 = proc () 2 end ;
       in (b.m1) end")
;;Expected output: 2

(scan&parse "let node = extend EmptyObj with
         public next = 0 ;
         public element = 0 ;
         public setElement = proc (x) (set self.element x) end ;
         public setNext = proc (x) (set self.next x) end ;

         public apply = proc (f)
           begin
             (set self.element (f self.element));
             if =(self.next, 0) then 0 else (self.next.apply f) end ;
           end
         end;

         in let head = extend node with
                a = extend node with
                b = extend node with
            in
               begin
                 (head.setNext a) ;
                 (a.setNext b) ;
                 (head.apply proc (x) +(x, 1) end) ;
                 if =(head.element, head.next.element)
                   then if =(head.next.element, head.next.next.element)
                          then =(head.element, 1)
                          else false
                        end
                   else false
                 end ;
               end
            end
        end")
;;Expected output: true

(scan&parse "let a = extend EmptyObj with protected x = 1;
       in a.x end")
;;Expected output: undefined

(scan&parse "let x = 1 in x end")
;;Expected output: 1

(scan&parse "extend EmptyObj with public x = 1; . x")
;;Expected output: 1
