(let ((time-stamp "Time-stamp: <2001-05-10 16:08:58 dfried>"))
  (eopl:printf "test-suite.scm: unified test suite ~a~%" (substring time-stamp 13 29)))

(initialize-test-suite!)

;;;;;;;;;;;;;;;; chapter 3 ;;;;;;;;;;;;;;;;

;; a few tests for 3-1:

(add-test! 'lang3-1 'pgm3-1-1 "1" 1)

(add-test! 'lang3-1 'pgm3-1-2 "add1(x)" 11)

;; this should raise an error
(add-test! 'lang3-1 'test3-1-unbound-variable "foo" 'error)

(add-test! 'lang3-1 'test3-1-primitive-app-2 "+(x,3)" 13)

;; serious tests for 3-5:

(add-test! 'lang3-5 'pgm1 "1" 1)

(add-test! 'lang3-1 'pgm2 "add1(x)" 11)

;; this should raise an error
(add-test! 'lang3-5 'unbound-variable "foo" 'error)

(add-test! 'lang3-1 'primitive-app-2 "+(x,3)" 13)

(add-test! 'lang3-5 'pgm3 "if zero?(1) then 3 else 4" 4)

(add-test! 'lang3-5 'pgm4 "if zero?(0) then 3 else 4" 3)

(add-test! 'lang3-5 'pgm5 "let x = 3 y = 4 in +(x,y)" 7)

(add-test! 'lang3-1 'nested-prim-app "+(i,*(3,4))" 13)

(add-test! 'lang3-5 'let-1 "let x = 5 y = 6 in +(x,y)" 11)

(add-test! 'lang3-5 'let-to-proc-1 "(proc (x,y) +(x,y) 5 6)" 11)

(add-test! 'lang3-5 'nested-let-1 "
let z = 5 x = 3
in let x = 4
       y = +(x,z)         % here x = 3
   in *(z, +(x,y))        % here x = 4" 60)

(add-test! 'lang3-5 'nested-let-2 "
(proc (z,x)
 (proc (x,y) *(z, +(x,y)) 4 +(x,z))
 5 3)
" 60)

(add-test! 'lang3-5 'nested-let-3 "let z = let x = 3 in *(x,x) in z" 9)

(add-test! 'lang3-5 'bind-a-proc "let f = proc(x,y)+(x,y) in (f 3 4)" 7)
(add-test! 'lang3-5 'apply-a-proc "(proc(x,y)-(x,y)  4 3)" 1)


(add-test! 'lang3-5 'infinite-loop "
let fix =  proc (f)
            let d = proc (x) proc (z) (f (x x) z)
            in proc (n) (f (d d) n)
in let loop = (fix proc (f,x) (f x))
   in (loop 1)" 'dontrun)

(add-test! 'lang3-5 'poor-mans-letrec "
let fix =  proc (f)
            let d = proc (x) proc (z) (f (x x) z)
            in proc (n) (f (d d) n)
    t4m = proc (f,x) if x then +(4,(f -(x,1))) else 0
in let times4 = (fix t4m)
   in (times4 3)" 12)

(add-test! 'lang3-6 'fact-of-6  "letrec
  fact(x) = if zero?(x) then 1 else *(x, (fact sub1(x)))
in (fact 6)" 720)

(add-test! 'lang3-6 'odd-of-13  "letrec
         even(x) = if zero?(x) then 1 else (odd sub1(x))
         odd(x)  = if zero?(x) then 0 else (even sub1(x))
       in (odd 13)" 1)

(add-test! 'lang3-6 'HO-nested-letrecs
  "letrec even(odd,x) =  if zero?(x) then 1 else (odd sub1(x))
   in letrec  odd(x)  = if zero?(x) then 0 else (even odd sub1(x))
   in (odd 13)" 1)

(add-test! 'lang3-7 'gensym-test
"let g = let count = 0 in proc() 
                        let d = set count = add1(count)
                        in count
in +((g), (g))"
3)

(add-test! 'lang3-7 'even-odd-via-set "
let x = 0
in letrec even() = if zero?(x) then 1 
                                  else let d = set x = sub1(x)
                                       in (odd)
              odd()  = if zero?(x) then 0 
                                  else let d = set x = sub1(x)
                                       in (even)
   in let d = set x = 13 in (odd)" 1)

(add-test! 'lang3-8ref 'basic-by-ref "
let x = 13
    f = proc (y) set y = add1(y)
in begin
    (f x); 
    x
   end"
14)

(add-test! 'lang3-8ref 'swap-by-ref "
let a = 3
    b = 4
    swap = proc (x, y)
             let temp = x
             in begin
                  set x = y;
                  set y = temp
                end
in begin
     (swap a b);
     -(a,b)
   end"
1)                                      ; 1 by reference, -1 by value



(add-test! 'lang3-8name 'basic-by-name "
let x = 13
    f = proc (y) set y = add1(y)
in begin
    (f x); 
    x
   end"
14)

(add-test! 'lang3-8name 'swap-by-name "
let a = 3
    b = 4
    swap = proc (x, y)
             let temp = x
             in begin
                  set x = y;
                  set y = temp
                end
in begin
     (swap a b);
     -(a,b)
   end"
1)                                      ; 1 by reference, -1 by value

(add-test! 'lang3-8name 'ignore-arg-name "
let a = 3
in let f = proc () set a = add1(a)
       g = proc (x) 11
   in begin 
        (g (f));
        a
      end"
3)

(add-test! 'lang3-8name 'check-multiple-evals-by-name "
let g = let count = 0 in proc () begin 
                                   set count = add1(count); 
                                   count
                                 end
in (proc(x) +(x,x)
    (g))"
3)                                      ; 3 for name, 2 for need

(add-test! 'lang3-8need 'basic-by-need "
let x = 13
    f = proc (y) set y = add1(y)
in begin
    (f x); 
    x
   end"
14)

(add-test! 'lang3-8need 'swap-by-need "
let a = 3
    b = 4
    swap = proc (x, y)
             let temp = x
             in begin
                  set x = y;
                  set y = temp
                end
in begin
     (swap a b);
     -(a,b)
   end"
1)                                      ; 1 by reference, -1 by value

(add-test! 'lang3-8need 'ignore-arg-by-need "
let a = 3
in let f = proc () set a = add1(a)
       g = proc (x) 11
   in begin 
        (g (f));
        a
      end"
3)

(add-test! 'lang3-8need 'check-multiple-evals-by-need "
let g = let count = 0 in proc () begin 
                                   set count = add1(count); 
                                   count
                                 end
in (proc(x) +(x,x)
    (g))"
2)                                      ; 3 for name, 2 for need


;; the test harness doesn't look at output, so these tests are pretty
;; useless. 

(add-test! 'lang3-9 'print-1 "print(10)" #t) 

(add-test! 'lang3-9 'pgm3-9-1 "var x,y; {x = 3; y = 4; print(+(x,y))}"
  #t)

(add-test! 'lang3-9 'pgm3-9-2 "
var x,y,z; {x = 3; y = 4; z = 0; 
            while x do {z = +(z,y); x = sub1(x)};
            print(z)}"
  #t)

(add-test! 'lang3-9 'pgm3-9-3 "
var x; {x = 3; print(x);
        var x; {x = 4; print(x)};
        print(x)}"
  #t)

(add-test! 'lang3-9 'pgm3-9-4 "
var f,x; {f = proc (x, y) *(x,y);
          x = 3;
          print((f 4 x))}"
  #t)


;;;;;;;;;;;;;;;; chapter 4 ;;;;;;;;;;;;;;;;

(add-typed-test! 'lang4-2 'literal "1" 'int 1)

;; this one should now raise a type error
(add-typed-test! 'lang4-2 'primitive-app-1 "add1(x)" 'error 'dontrun)

(add-typed-test! 'lang4-2 'primitive-app-2-typed "+(2,3)" 'int 5)

(add-typed-test! 'lang4-2 'primitive-app-3 "+(2,true)" 'error 'dontrun)

(add-typed-test! 'lang4-2 'type-a-proc-1 "proc(int x, bool y) x"
  '(int * bool -> int) 'dontrun)

(add-typed-test! 'lang4-2 'type-ho-proc-1
      "proc(int x, (int * int -> bool) f) (f x 3)"
      '(int * (int * int -> bool) -> bool)
      'dontrun)

(add-typed-test! 'lang4-2 'type-ho-proc-2
      "proc(int x, (int * int -> bool) f) (f x true)"
      'error 'dontrun)

(add-typed-test! 'lang4-2 'apply-a-proc-typed
  "(proc(int x, int y)-(x,y)  4 3)"
  'int 1)

(add-typed-test! 'lang4-2 'simple-let "let x = 3 y = 4 in +(x,y)" 'int 7) 

(add-typed-test! 'lang4-2 'bind-a-proc-typed
  "let f = proc (int x) add1(x) in (f 4)" 'int 5) 

(add-typed-test! 'lang4-2 'bind-a-proc-return-proc
  "let f = proc (int x) add1(x) in f"
  '(int -> int) 'dontrun)

(add-typed-test! 'lang4-2 'apply-ho-proc "
let apply = proc ((int * int -> int) f, int x) (f add1(x) x)
    g     = proc (int x, int y) *(x,y)
in (apply g 3)"
  'int 12)

(add-typed-test! 'lang4-2 'wrong-no-of-args "let f = proc (int x) add1(x) in (f 4 3)" 'error 'dontrun)

(add-typed-test! 'lang4-2 'unbound-variable-2 "let f = proc (int x) add1(x) in (g 4 3)" 'error 'dontrun)

(add-typed-test! 'lang4-2 'apply-a-proc-2-typed "(proc (int x) add1(x) 4)" 'int 5)

(add-typed-test! 'lang4-2 'apply-a-letrec "
letrec int f(int x, int y) = +(x,y) 
in (f 40 50)"
  'int 90)

(add-typed-test! 'lang4-2 'letrec-return-fact "
letrec 
  int fact(int x)= if zero?(x) then 1 else *(x, (fact sub1(x)))
in fact" '(int -> int) 'dontrun)

(add-typed-test! 'lang4-2 'letrec-apply-fact "
letrec 
  int fact(int x)= if zero?(x) then 1 else *(x, (fact sub1(x)))
in (fact 6)" 'int 720)

(add-typed-test! 'lang4-2 'letrec-return-odd "
letrec int odd(int x)  = if zero?(x) then 0 else (even sub1(x))
       int even(int x) = if zero?(x) then 1 else (odd  sub1(x))
in odd"
  '(int -> int) 'dontrun)

(add-typed-test! 'lang4-2 'letrec-apply-odd "
letrec int odd(int x)  = if zero?(x) then 0 else (even sub1(x))
       int even(int x) = if zero?(x) then 1 else (odd  sub1(x))
in (odd 13)" 'int 1)

(add-typed-test! 'lang4-2 'apply-fcn-to-wrong-type "
letrec int odd(int x)  = if zero?(x) then 0 else (even sub1(x))
       int even(int x) = if zero?(x) then 1 else (odd  sub1(x))
in (even odd)" 'error 'dontrun)

(add-typed-test! 'lang4-2 'types-fail-but-program-works "
let fix =  proc (int f)
            let d = proc (int x) proc (int z) (f (x x) z)
            in proc (int n) (f (d d) n)
    t4m = proc (int f, int x) if x then +(4,(f -(x,1))) else 0
in let times4 = (fix t4m)
   in (times4 3)" 'error 12)

;;; a good one to add types to.
; (add-typed-test! 'lang4-4 'pgm11
;   "letrecproc even(odd,x) =  if zero?(x) then 1 else (odd sub1(x))
;    in letrecproc  odd(x)  = if zero?(x) then 0 else (even odd sub1(x))
;    in (odd 13)" 'error 1)

(add-typed-test! 'lang4-3 'lettype-type-check-exported-fcn "
lettype foo = int
 foo m(int x) = add1(x)
in m" '(int -> foo) 'dontrun)

(add-typed-test! 'lang4-3 'lettype-apply-exported-fcn-1 "
lettype foo = int
 foo m(int x) = add1(x)
in (m 3)"
  'foo 4)

(add-typed-test! 'lang4-3 'lettype-foo-isnt-int  "
lettype foo = int
 foo m (int x) = add1(x)
in add1((m 3))"
  'error 5)

(add-typed-test! 'lang4-3 'lettype-error-in-defns "
lettype ff = (int -> int)
  ff zero-ff (int k) = 0                % a real type error!
  ff extend-ff (int k, int val, ff old-ff) =
       proc (int k1) if zero?(-(k1,k)) then val else (old-ff k1)
  int apply-ff (int k, ff f) = (f k)
in let ff1 = (extend-ff 1 11 (extend-ff 2 22 zero-ff))
   in (apply-ff ff1 2)"
'error 'dontrun)

(add-typed-test! 'lang4-3 'opaque-finite-fcns-0 "
lettype ff = (int -> int)
  ff zero-ff () = proc (int k) 0               
  ff extend-ff (int k, int val, ff old-ff) =
       proc (int k1) if zero?(-(k1,k)) then val else (old-ff k1)
  int apply-ff (ff f, int k) = (f k)
in let ff1 = (extend-ff 1 11 (extend-ff 2 22 (zero-ff)))
   in (apply-ff ff1 2)" 'int 22)



(add-typed-test! 'lang4-3 'opaque-finite-fcns-letrec-of-internal-fcns "
lettype ff = (int -> int)
  ff zero-ff () = proc (int k) 0               
  ff extend-ff (int k, int val, ff old-ff) =
       proc (int k1) if zero?(-(k1,k)) then val else (apply-ff old-ff k1)
  int apply-ff (ff f, int k) = (f k)
in let ff1 = (extend-ff 1 11 (extend-ff 2 22 (zero-ff)))
   in (apply-ff ff1 2)" 'int 22)

(add-typed-test! 'lang4-3 'opaque-finite-fcns-try-violating-abstraction-boundary "
lettype ff = (int -> int)
  ff zero-ff () = proc (int k) 0               
  ff extend-ff (int k, int val, ff old-ff) =
       proc (int k1) if zero?(-(k1,k)) then val else (old-ff k1)
  int apply-ff (ff f, int k) = (f k)
in let ff1 = (extend-ff 1 11 (extend-ff 2 22 (zero-ff)))
   in (ff1 2)"
  'error 22)                         ; 'error, 22


(add-typed-test! 'lang4-3 'myint1 "
lettype
  myint = int  % like the integers, but zero is represented as 1!
  myint zero() = 1
  myint succ(myint x) = add1(x)
  myint pred(myint x) = sub1(x)
  bool  iszero?(myint x) =  zero?(-(x,1))
in (succ (zero))"
  'myint 2)   ; myint, 2

(add-typed-test! 'lang4-3 'myint2 "
lettype
  myint = int  % like the integers, but zero is represented as 1!
  myint zero() = 1
  myint succ(myint x) = add1(x)
  myint pred(myint x) = sub1(x)
  bool  iszero?(myint x) =  zero?(-(x,1))
in add1((zero))"
  'error 2)   

;;; tests of type inference

(add-typed-test! 'lang4-4 'pgm4b "let f = proc (? x) add1(x) in (f 4)"
  'int 5)

(add-typed-test! 'lang4-4 'pgm4-0b "let f = proc (? x) add1(x) in f"
  '(int -> int) 'dontrun)

(add-typed-test! 'lang4-4 'pgm4-1b "
let apply = proc (?f, ? x) (f add1(x) x)
    g     = proc (? x, ? y) *(x,y)
in (apply g 3)"
  'int 12)                        ; int, 12

(add-typed-test! 'lang4-4 'pgm6b "letrec int f(? x,? y) = +(x,y) 
              in (f 40 50)"
  'int 90)            ; int, 90

(add-typed-test! 'lang4-4 'pgm7b "
letrec 
  ? fact(? x)= if zero?(x) then 1 else *(x, (fact sub1(x)))
in fact"
  '(int -> int) 'dontrun)

(add-typed-test! 'lang4-4 'pgm8b "
letrec ? odd(? x)  = if zero?(x) then 0 else (even sub1(x))
       ? even(? x) = if zero?(x) then 1 else (odd  sub1(x))
in odd" '(int -> int) 'dontrun)


(add-typed-test! 'lang4-4 'pgm8ab "
letrec ? odd(? x)  = if zero?(x) then 0 else (even sub1(x))
       ? even(bool x) = if zero?(x) then 1 else (odd  sub1(x))
in (odd 13)" 'error 'dontrun)

;; circular type
(add-typed-test! 'lang4-4 'circular-type "
let fix =  proc (? f)
            let d = proc (? x) proc (? z) (f (x x) z)
            in proc (? n) (f (d d) n)
    t4m = proc (? f, ? x) if zero?(x) then 0 else +(4,(f -(x,1)))
in let times4 = (fix t4m)
   in (times4 3)"
  'error 12)

(add-typed-test! 'lang4-4 'pgm11b
  "letrec ? even(? odd, ? x) =  if zero?(x) then 1 else (odd sub1(x))
   in letrec  ? odd(? x)  = if zero?(x) then 0 else (even odd sub1(x))
   in (odd 13)"
  'int 1)

(add-typed-test! 'lang4-4 'infer-in-body-using-opaque-type "
lettype
  myint = int  % like the integers, but zero is represented as 1!
  myint zero() = 1
  myint succ(myint x) = add1(x)
  myint pred(myint x) = sub1(x)
  bool  iszero?(myint x) =  zero?(-(x,1))
in letrec ? plus(? x,? y) = if (iszero? x) then y else (succ (plus (pred x) y))
in plus"
  '(myint * myint -> myint)  'dontrun)

(add-typed-test! 'lang4-4 'infer-in-impl-1 "
lettype
  myint = int  % like the integers, but zero is represented as 1!
  myint zero() = 1
  myint succ(myint x) = let f = proc(? x) x in add1((f x))
  myint pred(myint x) = sub1(x)
  bool  iszero?(myint x) =  zero?(-(x,1))
in letrec ? plus(? x,? y) = if (iszero? x) then y else (succ (plus (pred x) y))
in plus"
      '(myint * myint -> myint)  'dontrun)

(add-typed-test! 'lang6 'create-empty-class
  "class c1 extends object  3" 'int 3)

(add-typed-test! 'lang6 'create-class-with-method "
class c1 extends object 
 field int y 
 method int gety()y
  
33 "
'int 33)

(add-typed-test! 'lang6 'create-object "
class c1 extends object  
 method int initialize()0 
let o1 = new c1() in 11
" 'int 11)


(add-typed-test! 'lang6 'send-msg-1 "
class c1 extends object 
 field int s
 method void initialize()set s = 44
 method int gets()s
 method void sets(int v)set s = v                               

let o1 = new c1() in send o1 gets()
"
'int 44)

;; these tests are more serious


(add-typed-test! 'lang6 'send-msg-2 "
class c1 extends object 
          field int s 
          method void initialize()set s = 44 
          method int gets()s
          method int sets(int v)set s = v  % type error here
                
let o1 = new c1() 
    t1 = 0
    t2 = 0 
in begin
     set t1 = send o1 gets();
     send o1 sets(33);
     set t2 = send o1 gets();
     list(t1, t2)
  end
"
'error '(44 33))

;;;;;;;;;;;;;;;; chapter 5 ;;;;;;;;;;;;;;;;

(add-test! 'oop 'create-empty-class
  "class c1 extends object  3" 3)

(add-test! 'oop 'create-class-with-method "
class c1 extends object 
  field y 
  method gety () y 
33                       % 33 is the body"
33)

(add-test! 'oop 'create-object "
class c1 extends object  
 method initialize()0 
let o1 = new c1() in 11
" 11)


(add-test! 'oop 'send-msg-1 "
class c1 extends object 
  field s 
  method initialize() set s = 44
  method gets()s
  method sets(v)set s = v
  
let o1 = new c1() in send o1 gets()
"
44)

(add-test! 'oop 'send-msg-2 "
class c1 extends object 
  field s 
  method initialize() set s = 44
  method gets()s
  method sets(v)set s = v
  
let o1 = new c1() 
    t1 = 0
    t2 = 0 
in begin
     set t1 = send o1 gets();
     send o1 sets(33);
     set t2 = send o1 gets();
     list(t1, t2)
  end
"
'(44 33))

(add-test! 'oop 'test-self-1 "
class c extends object 
  field s
  method initialize(v)set s = v
  method sets(v)set s = v
  method gets()s
  method testit()send self sets(13)
  
let o = new c (11)
       t1 = 0
       t2 = 0
   in begin 
       set t1 = send o gets();
       send o testit();
       set t2 = send o gets();
       list(t1,t2)
      end" '(11 13))

(add-test! 'oop 'chris-1 "
class aclass extends object 
  field i
  method initialize(x) set i = x
  method m(y) +(i,y)
  
let o1 = new aclass(3)
in send o1 m(2)"
5)

(add-test! 'oop 'for-book-1 "
class c1 extends object
  field i
  field j
  method initialize(x) begin set i = x; set j = -(0,x) end
  method countup(d) begin set i = +(i,d); set j = -(j,d) end
  method getstate()list(i,j)
  
let o1 = new c1(3)
    t1 = 0
    t2 = 0
in begin
    set t1 = send o1 getstate();
    send o1 countup(2);
    set t2 = send o1 getstate();
    list(t1,t2)
   end"
'((3 -3) (5 -5)))


(add-test! 'oop 'odd-even-via-self "
class oddeven extends object 
  method initialize()1
  method even(n)if zero?(n) then 1 else send self odd(sub1(n))
  method odd(n) if zero?(n) then 0 else send self even(sub1(n))
  
let o1 = new oddeven() in send o1 odd(13)"
1)

;;; inheritance starts here

(add-test! 'oop 'inherit-1 "
class c1 extends object 
  field ivar1
  method initialize()set ivar1 = 1
  
class c2 extends c1 
  field ivar2
  method initialize() 
   begin
    super initialize(); 
    set ivar2 = 1
   end
  method setiv1(n)set ivar1 = n
  method getiv1()ivar1
  
let o = new c2 ()
    t1 = 0
in begin
       send o setiv1(33);
       send o getiv1()
   end                      
" 33)

(add-test! 'oop 'inherit-2 "
class c1 extends object 
  field ivar1
  method initialize()set ivar1 = 1

  method setiv1(n)set ivar1 = n
  method getiv1()ivar1

  method foo()1
  method call-foo-from-superclass()send self foo()

  
class c2 extends c1 
  field ivar2
  method initialize() 
   begin super initialize(); set ivar2 = 1 end
   

  method foo()2

  method setiv2(n)set ivar2 = n
  method getiv2()ivar2

  method self-and-super-foo()
    list( send self foo(),  super foo())

  method test-self-from-super()
     super call-foo-from-superclass()

   
let o = new c2 ()
    t1 = 0 t2 = 0 t3 = 0 t4 = 0
in begin
       send o setiv1(33);
       list(
         send o getiv1(),
         send o self-and-super-foo(),
         send o call-foo-from-superclass(),
         send o test-self-from-super()
         )
      end                      
" '(33 (2 1) 2 2))

;     (inherit-2 ,inherit-2 (33 (2 1) 2  2 22 33))

(add-test! 'oop 'inherit-3 "
class c1 extends object 
  method initialize()1
  method m1()1
  
class c2 extends c1 
  method m1()super m1()
  method m2()2
  
class c3 extends c2 
  method m1()3
  method m2()super m2()
  method m3()super m1()
  
let o = new c3 ()
in list( send o m1(),
         send o m2(),
         send o m3()
        )
" '(3 2 1))

; Chris's first example
; program
;   let a = new aclass init(3)
;   in call a m(2)
;   class aclass (i)
;     method init(x) fieldassign aclass this i = x;
;     method m(y) +(field aclass this i, y)
;   end
; end


(add-test! 'oop 'chris-2 "
class c1 extends object 
  method initialize() 1
  method ma()1
  method mb()send self ma()
  
class c2 extends c1   % just use c1's initialize
  method ma() 2
  
let x = new c2 ()
in list(send x ma(),send x mb())
"
'(2 2))


(add-test! 'oop 'for-book-2 "
class c1 extends object 
  method initialize()1
  method m1()1
  method m2()100
  method m3()send self m2()
  
class c2 extends c1 
  method m2()2
  
let o1 = new c1()
    o2 = new c2()
in list(send o1 m1(),           % returns 1
        send o1 m2(),           % returns 100
        send o1 m3(),           % returns 100
        send o2 m1(),           % returns 1 (from c1)
        send o2 m2(),           % returns 2 (from c2)
        send o2 m3()            % returns 2 (c1's m3 calls c2's m2)
       )
"
'(1 100 100 1 2 2))

(add-test! 'oop 'sum-leaves "
class tree extends object 
  method initialize()1
  
class interior_node extends tree 
  field left
  field right
  method initialize(l,r)
   begin
    set left = l; set right = r
   end
  method sum()+(send left sum(), send right sum())
  
class leaf_node extends tree 
  field value
  method initialize(v)set value = v
  method sum()value
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
"
12)

(add-test! 'oop 'check-shadowed-fields "
class c1 extends object 
  field x
  field y
  method initialize(v) begin set x = v; set y = 0 end
  method m1() x

class c2 extends c1 
  field x
  method initialize(v1,v2) begin set x = v2; 
                                    super initialize(v1) end
  method m2()list(x,y)

class c3 extends c2 
  field x
  method initialize(v1,v2,v3) begin set x = v3; 
                                       super initialize(v1,v2)
                                 end
  method m3()x

let o = new c3(1,2,3)
in list (send o m1(), send o m2(), send o m3())
"
'(1 (2 0) 3))

(add-test! 'oop 'static-super "
class c1 extends object
 method initialize () 1
 method m2() send self m3()
 method m3() 13
class c2 extends c1
 method m2() 22
 method m3() 23
 method m1() super m2()
class c3 extends c2
 method m2() 32
 method m3() 33
let o3 = new c3()
in send o3 m1()"
33)



(add-typed-test! 'lang6 'test-self-1 "
class c extends object 
         field int s
         method void initialize(int v)set s = v
         method void sets(int v)set s = v    
         method int gets()s
         method void testit()send self sets(13)
              
let o = new c (11)
       t1 = 0
       t2 = 0
   in begin 
       set t1 = send o gets();
       send o testit();
       set t2 = send o gets();
       list(t1,t2)
      end" '(list int) '(11 13))

;;;;;;;;;;;;;;;; chapter 6 ;;;;;;;;;;;;;;;;

(add-typed-test! 'lang6 'counter-1 "
class counter extends object
  field int count
   method void initialize()set count = 0
   method void countup()set count = add1(count)
   method int getcount()count
   
let o1 = new counter ()
    t1 = 0
    t2 = 0
in begin
    set t1 = send o1 getcount();
    send o1 countup();
    set t2 = send o1 getcount();
    list(t1,t2)
end
" '(list int) '(0 1))

(add-typed-test! 'lang6 'shared-counter-1 "
class counter extends object
  field int count
   method void initialize()set count = 0
   method void countup()set count = add1(count)
   method int getcount()count
   
class c1 extends object 
   field int n
   field counter counter1
   method void initialize(counter a_counter)
    begin
     set n = 0;
     set counter1 = a_counter
    end
   method void countup()
     begin
      send counter1 countup();
      set n = add1(n)
     end
   method list int getstate()list(n, send counter1 getcount())
   
let counter1 = new counter()
in let o1 = new c1(counter1)
       o2 = new c1(counter1)
in begin
     send o1 countup();
     send o2 countup();
     send o2 countup();
     list( send o1 getstate(),
           send o2 getstate())
   end
" '(list (list int)) '((1 3) (2 3)))


(add-typed-test! 'lang6 'inherit-1 "
class c1 extends object 
  field int ivar1
  method void initialize()set ivar1 = 1
  
class c2 extends c1 
  field int ivar2
  method void initialize() 
   begin
    super initialize(); 
    set ivar2 = 1
   end
  method void setiv1(int n)set ivar1 = n
  method int getiv1()ivar1
  
let o = new c2 ()
    t1 = 0
in begin
       send o setiv1(33);
       send o getiv1()
   end                      
" 'int 33)

(add-typed-test! 'lang6 'inherit-3 "
class c1 extends object 
  method int initialize()1
  method int m1()1
  
class c2 extends c1 
  method int initialize()1 
  method int m1()super m1()
  method int m2()2
  
class c3 extends c2 
  method int initialize()1
  method int m1()3
  method int m2()super m2()
  method int m3()super m1()
  
let o = new c3 ()
in list( send o m1(),
         send o m2(),
         send o m3()
        )
"  '(list int) '(3 2 1))

(add-typed-test! 'lang6 'chris-1 "
class aclass extends object 
  field int i
  method void initialize(int x) set i = x
  method int m(int y) +(i,y)
  
let o1 = new aclass(3)
in send o1 m(2)"
'int 5)

(add-typed-test! 'lang6 'chris-2 "
class c1 extends object 
  method int initialize() 1
  method int ma()1
  method int mb()send self ma()
  
class c2 extends c1   % just use c1's initialize
  method int ma() 2
  
let x = new c2 ()
in list(send x ma(),send x mb())
"
'(list int) '(2 2))

(add-typed-test! 'lang6 'for-book-1 "
class c1 extends object 
  field int i
  field int j
  method void initialize(int x) begin set i = x; set j = -(0,x) end
  method void countup(int d) begin set i = +(i,d); set j = -(j,d) end
  method list int getstate()list(i,j)
  
let o1 = new c1(3)
    t1 = nil[int]
    t2 = nil[int]
in begin
    set t1 = send o1 getstate();
    send o1 countup(2);
    set t2 = send o1 getstate();
    list(t1,t2)
   end"
'(list (list int)) '((3 -3) (5 -5)))


(add-typed-test! 'lang6 'odd-even-via-self "
class oddeven extends object 
  method int initialize()1
  method bool even(int n)if zero?(n) then true else send self odd(sub1(n))
  method bool odd(int n) if zero?(n) then false else send self even(sub1(n))
  
let o1 = new oddeven() in send o1 odd(13)"
'bool 1)

(add-typed-test! 'lang6 'for-book-2 "
class c1 extends object 
  method int initialize()1
  method int m1()1
  method int m2()100
  method int m3()send self m2()
  
class c2 extends c1 
  method int initialize()1
  method int m2()2
  
let o1 = new c1()
    o2 = new c2()
in list(send o1 m1(),           % returns 1
        send o1 m2(),           % returns 100
        send o1 m3(),           % returns 100
        send o2 m1(),           % returns 1 (from c1)
        send o2 m2(),           % returns 2 (from c2)
        send o2 m3()            % returns 2 (c1's m3 calls c2's m2)
       )
"
'(list int) '(1 100 100 1 2 2))

(add-typed-test! 'lang6 'sum-leaves "
class tree extends object 
  method int initialize()1
  
class interior_node extends tree 
  field node left
  field node right
  method void initialize(node l, node r)
   begin
    set left = l; set right = r
   end
  method int sum()+(send left sum(), send right sum())
  
class leaf_node extends tree 
  field int value
  method void initialize(int v)set value = v
  method int sum()value
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
"
'error 12)

(add-typed-test! 'lang6 'sum-leaves-2 "
abstract class tree extends object 
  method int initialize()1
  
class interior_node extends tree 
  field tree left
  field tree right
  method void initialize(tree l, tree r)
   begin
    set left = l; set right = r
   end
  method int sum()+(send left sum(), send right sum())
  
class leaf_node extends tree 
  field int value
  method void initialize(int v)set value = v
  method int sum()value
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
"
'error 12)

(add-typed-test! 'lang6 'sum-leaves-with-abstract-method "
abstract class tree extends object 
  method int  initialize()1
  abstractmethod int sum()
  
class interior_node extends tree 
  field tree left
  field tree right
  method void initialize(tree l, tree r)
   begin
    set left = l; set right = r
   end
  method int sum()+(send left sum(), send right sum())
  
class leaf_node extends tree 
  field int value
  method void initialize(int v)set value = v
  method int sum()value
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),   %% need subtyping to make this ok.
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
"
'int 12)


(add-typed-test! 'lang6 'equal-trees-1 "
abstract class tree extends object 
  method int  initialize()1
  abstractmethod int sum()
  abstractmethod bool equal(tree t)
  
class interior_node extends tree 
  field tree left
  field tree right
  method void initialize(tree l, tree r)
   begin
    set left = l; set right = r
   end
  method tree getleft()left
  method tree getright()right
  method int sum()+(send left sum(), send right sum())
  method bool equal(tree t) 
    if instanceof t interior_node
     then if send left equal(send cast t interior_node getleft())
          then send right equal(send cast t interior_node getright())
          else false
     else false 
     
  
class leaf_node extends tree 
  field int value
  method void initialize(int v)set value = v
  method int sum()value
  method int getvalue()value
  method bool equal(tree t) 
   if instanceof t leaf_node
    then zero?(-(value, send cast t leaf_node getvalue()))
    else false
    
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),   
            new leaf_node(4)),
          new leaf_node(5))
in send o1 equal(o1)
"
'bool 1)

(add-typed-test! 'lang6 'bad-cast-1 "
class c1 extends object 
class c2 extends object 
proc (c1 o) cast o c2
"
'error 'dontrun)

(add-typed-test! 'lang6 'good-instanceof-1 "
class c1 extends object 
class c2 extends object 
let p = proc (c1 o) instanceof o c2 in 11
"
'int 11)

(add-typed-test! 'lang6 'up-cast-1 "
class c1 extends object 
  method int initialize ()1
  method int get()2
  
class c2 extends c1 
let f = proc (c2 o) send cast o c1 get() in (f new c2())
"
'int 2)

(add-typed-test! 'lang6 'up-instance-1 "
class c1 extends object 
  method int initialize ()1
  method int get()2
  
class c2 extends c1 
let f = proc (c2 o) instanceof o c1 in (f new c2())
"
'bool 1)

(add-typed-test! 'lang6 'missing-initialize-method-1 "
class c1 extends object 
  method int initialize ()1
  method int get()2
  
class c2 extends object   % no initialize method!
let f = proc (c2 o) instanceof o c1 in (f new c2())
"
'error 'dontrun)

(add-typed-test! 'lang6 'duplicate-methods-1 "
class c1 extends object
  method int initialize() 1
class c2 extends c1
  method int m1() 1
  method int m1() 2
33"
'error 33)

(add-typed-test! 'lang6 'incomparable-instanceof-2 "
class c1 extends object 
  method int initialize ()1
  method int get()2
  
class c2 extends object 
  method int initialize () 100
    
let f = proc (c2 o) if instanceof o c1 then 1 else 2 in (f new c2())
"
'int 2)

(add-typed-test! 'lang6 'equal-trees-by-double-dispatch "
abstract class tree extends object 
  method int  initialize()1
  abstractmethod int sum()
  abstractmethod bool equal(tree t)
  abstractmethod bool equal_int(tree l, tree r)
  abstractmethod bool equal_leaf(int val)
  
class interior_node extends tree 
  field tree left
  field tree right
  method void initialize(tree l, tree r)
   begin
    set left = l; set right = r
   end
  method int sum()+(send left sum(), send right sum())
  method bool equal(tree t) send t equal_int(left, right)
  method bool equal_int(tree l, tree r) 
     if send left equal(l)
     then send right equal(r)
     else false
     
  method bool equal_leaf(int v) false
  
class leaf_node extends tree 
  field int value
  method void initialize(int v)set value = v
  method int sum()value
  method bool equal(tree t) send t equal_leaf(value)
  method bool equal_int(tree l, tree r) false
  method bool equal_leaf(int otherval) zero?(-(value, otherval))
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),   
            new leaf_node(4)),
          new leaf_node(5))
in send o1 equal(o1)
"
'bool 1)

(add-typed-test! 'lang6 'bad-super-1 "
class c1 extends object 
 method int initialize() 1
 
class c2 extends c1 
 method int m1() super m2()
 
class c3 extends c2 
 method int m2() 2
 
class c4 extends c3 
let o = new c4() in send o m1()
"
'error 'dontrun)

(add-typed-test! 'lang6 'missing-concrete "
class c1 extends object 
 method int initialize() 1
 abstractmethod int foo()
 
class c2 extends c1 
let o = new c2() in 33"
'error 33)

(add-typed-test! 'lang6 'cant-apply-abstract-method "
class c1 extends object 
 method int initialize() 1
 abstractmethod int m()
 
let o = new c1() in send o m()
"
'error 'dontrun)

(add-typed-test! 'lang6 'cant-apply-abstract-super "
abstract class c1 extends object 
  abstractmethod int m1()
 
class c2 extends c1 
 method int m1 () 1
 method int m2 () super m1 ()
 
33"
'error 'dontrun)

(add-typed-test! 'lang6 'goldberg-80 "
class c1 extends object 
  method int initialize () 1
  method int test () 1
  method int result1 () send self test ()
  
class c2 extends c1 
  method int test () 2
  
class c3 extends c2 
  method int result2 () send self result1 ()
  method int result3 () super test ()
  
class c4 extends c3 
  method int test () 4
  
let o3 = new c3 ()
    o4 = new c4 ()
in list(send o3 test(),
        send o4 result1 (),
        send o3 result2 (),   
        send o4 result2 (),
        send o3 result3 (),
        send o4 result3 ())
"
'(list int) '(2 4 2 4 2 2))

(add-typed-test! 'lang6 'abstract-method-never-overridden "
abstract class c1 extends object 
  abstractmethod int m1() 
  
class c2 extends c1 
  method int m2 ()send self m1()
  
33"
'error 'dontrun)

(add-typed-test! 'lang6 'overriding-method-changes-type-1 "
abstract class c1 extends object 
  abstractmethod int m1()
  
class c2 extends c1 
  method bool m1() false
  
33"
'error 'dontrun)

(add-typed-test! 'lang6 'test6-3-1 "
abstract class c1 extends object
 method int initialize () 1 
 method int m1 () 11
 abstractmethod int m2 () 
class c2 extends c1 
 method int m1 () 21
 method int m2 () 22
 method int m3 () 23
class c3 extends c2 
 method int m4 () 34
class c4 extends c3 
 method int m2 () 42 
proc (c3 o) send o m2()
"
'(c3 -> int) 'dontrun)

;;;;;;;;;;;;;;;; chapter 7 ;;;;;;;;;;;;;;;;

;;; most of chapter 7 reuses the languages from chapter 3.

;; only a simple test for 7.4:

(add-test! 'lang7-4 'raise-exception-1 "
let index = proc (n, l)
              letrec                            
                loop (l) = if null?(l) 
                           then raise sub1(0)  
                           else if equal?(n,car(l)) 
                                then 0   
                                else add1((loop cdr(l)))
              in try (loop l) handle proc (x) x
in (index 1 list(2,3))"
-1)

;; for 7.5, the final answers capture only a small part of the
;; program's behavior.

(add-test! 'lang7-5 'pgm7-5-1 "
let acc = 0 done = 0
in let d = spawn set acc = 20
   in letrec 
        loop () = if acc 
                  then let d = set done = 1
                       in acc
                  else (loop)
      in (loop)
" 20)

(add-test! 'lang7-5 'pgm7-5-2 "
let buf1 = 0 buf2 = 0 buf3 = 0
in let d1 = spawn set buf1 = 20
       d2 = spawn letrec 
                    loop () = if buf1 
                              then set buf2 = +(buf1,2)
                              else (loop)
                  in (loop)
       d3 = spawn letrec 
                    loop () = if buf2 
                              then set buf3 = +(buf2,2)
                              else (loop)
                  in (loop)
   in letrec
        loop () = if buf3 then buf3 else (loop)
      in (loop)
"
  24)

(add-test! 'lang7-5 'pgm7-5-3 "
letrec 
  noisy (l) = let d = print(l)
              in if null?(l) 
                 then 0 
                 else (noisy cdr(l))
in let d1 = spawn (noisy list(1,2,3,4,5))
       d2 = spawn (noisy list(6,7,8,9,10))
       d3 = spawn (noisy list(11,12,13,14,15,16,17))
   in 33
"
  33)

(add-test! 'lang7-5 'shared-variable-1 "
let l = lock list(0)
    done = 0
in let t1 = spawn let c = acquire l
                  in begin
                       setcar(c,add1(car(c)));
                       setcar(c,add1(car(c)));
                       print(list(1,car(c)));
                       set done = add1(done);
                       release l
                     end
       t2 = spawn let c = acquire l
                  in begin 
                       setcar(c,add1(car(c)));
                       setcar(c,add1(car(c)));
                       print(list(2,car(c)));
                       set done = add1(done);
                       release l
                     end
   in let v = 0
      in letrec loop() = if equal?(done,2)
                         then let c = acquire l
                              in begin
                                   set v = car(c);
                                   release l;
                                   v
                                 end
                         else (loop)
         in (loop)
"
4)  

;;;;;;;;;;;;;;;; chapter 8: cps ;;;;;;;;;;;;;;;;

(add-test! 'lang8-4 'cps-1 "letrec
  fact(x) = if zero?(x) then 1 else *(x, (fact sub1(x)))
in (fact 6)"
' (lambda (k1)
    (letrec ((fact
               (lambda (x k1)
                 (if (zero? x)
                   (k1 1)
                   (fact (sub1 x) (lambda (v1) (k1 (* x v1))))))))
      (fact 6 k1))))

(add-test! 'lang8-4 'cps-2 "letrec
         even(x) = if zero?(x) then 1 else (odd sub1(x))
         odd(x)  = if zero?(x) then 0 else (even sub1(x))
       in (odd 13)"
  '(lambda (k1)
     (letrec ((even
                (lambda (x k1) (if (zero? x) (k1 1) (odd (sub1 x) k1))))
              (odd
                (lambda (x k1) (if (zero? x) (k1 0) (even (sub1 x) k1)))))
       (odd 13 k1))))

(add-test! 'lang8-4 'cps-3 "
letrec even(odd,x) =  if zero?(x) then 1 else (odd sub1(x))
   in letrec  odd(x)  = if zero?(x) then 0 else (even odd sub1(x))
   in (odd 13)"
  '(lambda (k1)
     (letrec ((even
                (lambda (odd x k1) (if (zero? x) (k1 1) (odd (sub1 x) k1)))))
       (letrec ((odd
                  (lambda (x k1)
                    (if (zero? x) (k1 0) (even odd (sub1 x) k1)))))
         (odd 13 k1)))))

(add-test! 'lang8-4 'cps-4 "
add1(if (f zero?(x)) then 1 else *(x, (fact sub1(x))))"
  '(lambda (k1)
     (f (zero? x)
       (lambda (v2)
         (if v2
           (let ((v1 1)) (k1 (add1 v1)))
           (fact (sub1 x)
             (lambda (v3) (let ((v1 (* x v3))) (k1 (add1 v1))))))))))

(add-test! 'lang8-4 'cps-5 "
let a = (f x) b = (g x) c = (h (i (j (t x))))
in cons(a,cons(b,cons(c,emptylist())))"
  '(lambda (k1)
     (f x
       (lambda (z1)
         (g x
           (lambda (z2)
             (t x
               (lambda (v6)
                 (j v6
                   (lambda (v5)
                     (i v5
                       (lambda (v4)
                         (h v4
                           (lambda (z3)
                             (k1 (let ((a z1) (b z2) (c z3))
                                   (cons a
                                     (cons b
                                       (cons c
                                         (empty))))))))))))))))))))

(add-test! 'lang8-4 'cps-6 "
proc(f) let foo = proc(foo,ls) 
                   if null?(ls)
                   then emptylist()
                   else cons((f car(ls)), (foo foo cdr(ls)))
        in proc(ls)(foo foo ls)"
  '(lambda (k1)
     (k1 (lambda (f k1)
           (k1 (let ((foo
                       (lambda (foo ls k1)
                         (if (null? ls)
                           (k1 (empty))
                           (f (car ls)
                             (lambda (v1)
                               (foo foo
                                 (cdr ls)
                                 (lambda (v2) (k1 (cons v1 v2))))))))))
                 (lambda (ls k1) (foo foo ls k1))))))))

(add-test! 'lang8-4 'cps-7 "
 (f let f = 3 in add1(f))"
  '(lambda (k1) (f (let ((f 3)) (add1 f)) k1)))

(add-test! 'lang8-4 'cps-8 "(k (g let g=3 in (f g x)))"
  '(lambda (k1)
     (let ((k1 (lambda (v2) (g v2 (lambda (v1) (k v1 k1))))))
       (let ((g 3)) (f g x k1)))))

(add-test! 'lang8-4 'cps-8 "(even
  letrec
    even(x) = if zero?(x) then 1 else (odd sub1(x))
    odd(x)  = if zero?(x) then 0 else (even sub1(x))
  in (odd 13))"
  '(lambda (k1)
     (let ((k1 (lambda (v1) (even v1 k1))))
       (letrec ((even
                  (lambda (x k1) (if (zero? x) (k1 1) (odd (sub1 x) k1))))
                (odd
                  (lambda (x k1) (if (zero? x) (k1 0) (even (sub1 x) k1)))))
         (odd 13 k1)))))

(add-test! 'lang8-5print 'two-prints
  "(f print((g x)) print(4))"
  '(lambda (k1)
  (g x
     (lambda (v3)
       (printc
         v3
         (lambda (v1) (printc 4 (lambda (v2) (f v1 v2 k1)))))))))

(add-test! 'lang8-5print 'letcc-no-set-1
  "+(1, letcc j in +(4, throw 3 to j))"
  '(lambda (k1)
     (let ((k1 (lambda (v1) (k1 (+ 1 v1)))))
       (let ((j k1)) (j 3)))))

(add-test! 'lang8-5print 'letcc-no-set-2
  "let x = 4 g = proc(n)*(n,n) in (g letcc g in (f throw x to g))"
  '(lambda (k1)
     (let ((x 4) (g (lambda (n k1) (k1 (* n n)))))
       (let ((k1 (lambda (v1) (g v1 k1)))) (let ((g k1)) (g x))))))


;;; tests for 8-5set.scm.  These duplicate the tests in 8-4.scm, since
;;; they cps differently.

(add-test!
  'lang8-5set
  'pgm8-5-1
  "letrec
  fact(x) = if zero?(x) then 1 else *(x, (fact sub1(x)))
in (fact 6)"
  '(lambda (k1)
     (letrec ((fact
               (lambda (x k1)
                 (derefc
                   x
                   (lambda (v7)
                     (let ((v1 (zero? v7)))
                       (if v1
                           (k1 1)
                           (derefc
                             x
                             (lambda (v2)
                               (derefc
                                 fact
                                 (lambda (v4)
                                   (derefc
                                     x
                                     (lambda (v6)
                                       (let ((v5 (sub1 v6)))
                                         (v4 v5
                                             (lambda (v3)
                                               (k1 (* v2
                                                      v3))))))))))))))))))
       (derefc fact (lambda (v8) (v8 6 k1))))))
(add-test!
  'lang8-5set
  'pgm8-5-2
  "letrec
         even(x) = if zero?(x) then 1 else (odd sub1(x))
         odd(x)  = if zero?(x) then 0 else (even sub1(x))
       in (odd 13)"
  '(lambda (k1)
     (letrec ((even
               (lambda (x k1)
                 (derefc
                   x
                   (lambda (v10)
                     (let ((v6 (zero? v10)))
                       (if v6
                           (k1 1)
                           (derefc
                             odd
                             (lambda (v7)
                               (derefc
                                 x
                                 (lambda (v9)
                                   (let ((v8 (sub1 v9)))
                                     (v7 v8 k1))))))))))))
              (odd
               (lambda (x k1)
                 (derefc
                   x
                   (lambda (v5)
                     (let ((v1 (zero? v5)))
                       (if v1
                           (k1 0)
                           (derefc
                             even
                             (lambda (v2)
                               (derefc
                                 x
                                 (lambda (v4)
                                   (let ((v3 (sub1 v4)))
                                     (v2 v3 k1)))))))))))))
       (derefc odd (lambda (v11) (v11 13 k1))))))
(add-test!
  'lang8-5set
  'pgm8-5-3
  "letrec even(odd,x) =  if zero?(x) then 1 else (odd sub1(x))
   in letrec  odd(x)  = if zero?(x) then 0 else (even odd sub1(x))
   in (odd 13)"
  '(lambda (k1)
     (letrec ((even
               (lambda (odd x k1)
                 (derefc
                   x
                   (lambda (v5)
                     (let ((v1 (zero? v5)))
                       (if v1
                           (k1 1)
                           (derefc
                             odd
                             (lambda (v2)
                               (derefc
                                 x
                                 (lambda (v4)
                                   (let ((v3 (sub1 v4)))
                                     (v2 v3 k1)))))))))))))
       (letrec ((odd
                 (lambda (x k1)
                   (derefc
                     x
                     (lambda (v11)
                       (let ((v6 (zero? v11)))
                         (if v6
                             (k1 0)
                             (derefc
                               even
                               (lambda (v7)
                                 (derefc
                                   odd
                                   (lambda (v8)
                                     (derefc
                                       x
                                       (lambda (v10)
                                         (let ((v9 (sub1 v10)))
                                           (v7 v8 v9 k1)))))))))))))))
         (derefc odd (lambda (v12) (v12 13 k1)))))))
(add-test!
  'lang8-5set
  'pgm8-5-4
  "+(1, letcc j in +(4, throw 3 to j))"
  '(lambda (k1)
     (let ((k1 (lambda (v1) (k1 (+ 1 v1)))))
       (let ((j k1)) (derefc j (lambda (v3) (v3 3)))))))
(add-test!
  'lang8-5set
  'pgm8-5-5
  "add1(if (f zero?(x)) then 1 else *(x, (fact sub1(x))))"
  '(lambda (k1)
     (derefc
       f
       (lambda (v8)
         (derefc
           x
           (lambda (v10)
             (let ((v9 (zero? v10)))
               (v8 v9
                   (lambda (v2)
                     (if v2
                         (let ((v1 1)) (k1 (add1 v1)))
                         (derefc
                           x
                           (lambda (v3)
                             (derefc
                               fact
                               (lambda (v5)
                                 (derefc
                                   x
                                   (lambda (v7)
                                     (let ((v6 (sub1 v7)))
                                       (v5 v6
                                           (lambda (v4)
                                             (let ((v1 (* v3 v4)))
                                               (k1 (add1 v1))))))))))))))))))))))
(add-test!
  'lang8-5set
  'pgm8-5-6
  "
let a = (f x) b = (g x) c = (h (i (j (t x))))
in cons(a,cons(b,cons(c,emptylist())))"
  '(lambda (k1)
     (derefc
       f
       (lambda (v19)
         (derefc
           x
           (lambda (v20)
             (v19 v20
                  (lambda (z1)
                    (derefc
                      g
                      (lambda (v17)
                        (derefc
                          x
                          (lambda (v18)
                            (v17 v18
                                 (lambda (z2)
                                   (derefc
                                     h
                                     (lambda (v9)
                                       (derefc
                                         i
                                         (lambda (v11)
                                           (derefc
                                             j
                                             (lambda (v13)
                                               (derefc
                                                 t
                                                 (lambda (v15)
                                                   (derefc
                                                     x
                                                     (lambda (v16)
                                                       (v15 v16
                                                            (lambda (v14)
                                                              (v13 v14
                                                                   (lambda (v12)
                                                                     (v11 v12
                                                                          (lambda (v10)
                                                                            (v9 v10
                                                                                (lambda (z3)
                                                                                  (let ((a
                                                                                         z1)
                                                                                        (b
                                                                                         z2)
                                                                                        (c
                                                                                         z3))
                                                                                    (derefc
                                                                                      a
                                                                                      (lambda (v4)
                                                                                        (derefc
                                                                                          b
                                                                                          (lambda (v6)
                                                                                            (derefc
                                                                                              c
                                                                                              (lambda (v8)
                                                                                                (let ((v7
                                                                                                       (cons v8
                                                                                                             (empty))))
                                                                                                  (let ((v5
                                                                                                         (cons v6
                                                                                                               v7)))
                                                                                                    (k1 (cons v4
                                                                                                              v5)))))))))))))))))))))))))))))))))))))))))))
(add-test!
  'lang8-5set
  'pgm8-5-7
  "
proc(f) let foo = proc(foo,ls) 
                   if null?(ls)
                   then emptylist()
                   else cons((f car(ls)), (foo foo cdr(ls)))
        in proc(ls)(foo foo ls)"
  '(lambda (k1)
     (k1 (lambda (f k1)
           (k1 (let ((foo
                      (lambda (foo ls k1)
                        (derefc
                          ls
                          (lambda (v11)
                            (let ((v1 (null? v11)))
                              (if v1
                                  (k1 (empty))
                                  (derefc
                                    f
                                    (lambda (v8)
                                      (derefc
                                        ls
                                        (lambda (v10)
                                          (let ((v9 (car v10)))
                                            (v8 v9
                                                (lambda (v2)
                                                  (derefc
                                                    foo
                                                    (lambda (v4)
                                                      (derefc
                                                        foo
                                                        (lambda (v5)
                                                          (derefc
                                                            ls
                                                            (lambda (v7)
                                                              (let ((v6
                                                                     (cdr v7)))
                                                                (v4 v5
                                                                    v6
                                                                    (lambda (v3)
                                                                      (k1 (cons v2
                                                                                v3)))))))))))))))))))))))))
                 (lambda (ls k1)
                   (derefc
                     foo
                     (lambda (v12)
                       (derefc
                         foo
                         (lambda (v13)
                           (derefc
                             ls
                             (lambda (v14) (v12 v13 v14 k1))))))))))))))
(add-test!
  'lang8-5set
  'pgm8-5-8
  "
 (f let f = 3 in add1(f))"
  '(lambda (k1)
     (derefc
       f
       (lambda (v1)
         (let ((k1 (lambda (v2) (v1 v2 k1))))
           (let ((f 3)) (derefc f (lambda (v3) (k1 (add1 v3))))))))))
(add-test!
  'lang8-5set
  'pgm8-5-9
  "(k (g let g=3 in (f g x)))"
  '(lambda (k1)
     (derefc
       k
       (lambda (v1)
         (derefc
           g
           (lambda (v3)
             (let ((k1 (lambda (v4) (v3 v4 (lambda (v2) (v1 v2 k1))))))
               (let ((g 3))
                 (derefc
                   f
                   (lambda (v5)
                     (derefc
                       g
                       (lambda (v6)
                         (derefc x (lambda (v7) (v5 v6 v7 k1)))))))))))))))
(add-test!
  'lang8-5set
  'pgm8-5-10
  "  (even
  letrec
    even(x) = if zero?(x) then 1 else (odd sub1(x))
    odd(x)  = if zero?(x) then 0 else (even sub1(x))
  in (odd 13))"
  '(lambda (k1)
     (derefc
       even
       (lambda (v1)
         (let ((k1 (lambda (v2) (v1 v2 k1))))
           (letrec ((even
                     (lambda (x k1)
                       (derefc
                         x
                         (lambda (v12)
                           (let ((v8 (zero? v12)))
                             (if v8
                                 (k1 1)
                                 (derefc
                                   odd
                                   (lambda (v9)
                                     (derefc
                                       x
                                       (lambda (v11)
                                         (let ((v10 (sub1 v11)))
                                           (v9 v10 k1))))))))))))
                    (odd
                     (lambda (x k1)
                       (derefc
                         x
                         (lambda (v7)
                           (let ((v3 (zero? v7)))
                             (if v3
                                 (k1 0)
                                 (derefc
                                   even
                                   (lambda (v4)
                                     (derefc
                                       x
                                       (lambda (v6)
                                         (let ((v5 (sub1 v6)))
                                           (v4 v5 k1)))))))))))))
             (derefc odd (lambda (v13) (v13 13 k1)))))))))
