(let ((time-stamp "Time-stamp: <2001-05-11 11:00:35 dfried>"))
  (eopl:printf "6-checker.scm ~a~%" (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; The Type Checker ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (c-decls exp)
        (statically-elaborate-class-decls! c-decls)
        (type-of-expression exp (empty-tenv))))))

(define type-of-expression
  (lambda (exp tenv)
    (cases expression exp
      (lit-exp (number) int-type)
      (true-exp () bool-type)
      (false-exp () bool-type)
      (var-exp (id) (apply-tenv tenv id))
      (primapp-exp (prim rands)
        (type-of-application
          (type-of-primitive prim)
          (types-of-expressions rands tenv)
          prim rands exp))
      (if-exp (test-exp true-exp false-exp)
        (let
          ((test-type (type-of-expression test-exp tenv))
           (true-type (type-of-expression true-exp tenv))
           (false-type (type-of-expression false-exp tenv)))
          ;; these tests either succeed or raise an error
          (check-equal-type! test-type bool-type test-exp)
          (check-equal-type! true-type false-type exp)
          true-type))
      (let-exp (ids rands body) (type-of-let-exp ids rands body tenv))
      (proc-exp (texps ids body)
        (type-of-proc-exp texps ids body tenv))
      (app-exp (rator rands) 
        (type-of-application
          (type-of-expression rator tenv)
          (types-of-expressions rands tenv)
          rator rands exp))
      (letrec-exp (result-texps proc-names texpss idss bodies
                    letrec-body)
        (type-of-letrec-exp
          result-texps proc-names texpss idss bodies
          letrec-body tenv))
      (varassign-exp (id rhs)
        (check-is-subtype!
          (type-of-expression rhs tenv)
          (apply-tenv tenv id)
          exp)
        void-type)
      (begin-exp (exp1 exps)                ; love that abstract interpretation!
        (let loop ((acc (type-of-expression exp1 tenv))
                   (exps exps))
          (if (null? exps) acc
            (loop (type-of-expression (car exps) tenv) (cdr exps)))))
      (list-exp (exp1 exps) 
        (type-of-list-exp
          (type-of-expression exp1 tenv)
          (types-of-expressions exps tenv)
          exp1 exps))
      (cons-exp (car-exp cdr-exp) 
        (type-of-cons-exp
          (type-of-expression car-exp tenv)
          (type-of-expression cdr-exp tenv)
          exp))
      (car-exp (exp1) 
        (type-of-car-exp
          (type-of-expression exp1 tenv)
          exp))
      (cdr-exp (exp1) 
        (type-of-cdr-exp
          (type-of-expression exp1 tenv)
          exp))
      (nil-exp (texp) 
        (type-of-nil-exp
          (expand-type-expression texp)))
      (null?-exp (exp1) 
        (type-of-null?-exp 
          (type-of-expression exp1 tenv)
          exp))

      ;; object stuff begins here
      (new-object-exp (class-name rands)
        (type-of-new-obj-exp
          class-name 
          (types-of-expressions rands tenv)
          rands
          exp))
      (method-app-exp (obj-exp msg rands)
        (type-of-method-app-exp 
          (type-of-expression obj-exp tenv)
          msg
          (types-of-expressions rands tenv)
          rands
          exp))
      (super-call-exp (msg rands) 
        (type-of-super-call-exp 
          (class-type->name (apply-tenv tenv '%super))
          msg
          (types-of-expressions rands tenv)
          rands
          exp))
      (cast-exp (exp1 class-name)
        (type-of-cast-exp
          (type-of-expression exp1 tenv)
          class-name
          exp))
      (instanceof-exp (exp1 class-name)
        (type-of-instanceof-exp
          (type-of-expression exp1 tenv)
          class-name
          exp))

      (else (eopl:error 'type-of-expression
              "Illegal expression~%~s"
              exp))

      )))

(define type-of-proc-exp
  (lambda (texps ids body tenv)
    (let ((arg-types (expand-type-expressions texps)))
      (let ((result-type
              (type-of-expression body
                (extend-tenv ids arg-types tenv))))
        (proc-type arg-types result-type)))))

(define type-of-application
  (lambda (rator-type rand-types rator rands exp)
    (cases type rator-type
      (proc-type (arg-types result-type)
        (if (= (length arg-types) (length rand-types))
          (begin 
            (for-each
              check-is-subtype!   ;\new1
              rand-types arg-types rands)
            result-type)
          (eopl:error 'type-of-expression
            (string-append
              "Wrong number of arguments in expression ~s:~%"
              "expected ~s~%got ~s")
              exp
              (map type-to-external-form arg-types)
              (map type-to-external-form rand-types))))
      (else
        (eopl:error 'type-of-expression
          "Rator not a proc type:~%~s~%had rator type ~s"  
          rator (type-to-external-form rator-type))))))

(define type-of-primitive
  (lambda (prim)
    (cases primitive prim
      (add-prim  ()     (proc-type (list int-type int-type) int-type))
      (subtract-prim () (proc-type (list int-type int-type) int-type))
      (mult-prim  ()    (proc-type (list int-type int-type) int-type))
      (incr-prim  ()    (proc-type (list int-type) int-type))
      (decr-prim  ()    (proc-type (list int-type) int-type))
      (zero-test-prim () (proc-type (list int-type) bool-type)))))

(define types-of-expressions
  (lambda (rands tenv)
    (map (lambda (exp) (type-of-expression exp tenv)) rands)))

(define type-of-let-exp
  (lambda (ids rands body tenv)
    (let ((new-tenv
            (extend-tenv
              ids
              (types-of-expressions rands tenv)
              tenv)))
      (type-of-expression body new-tenv))))

(define type-of-letrec-exp
  (lambda (result-texps proc-names texpss idss bodies
            letrec-body tenv) 
    (let
      ((arg-typess
         (map
           (lambda (texps)
             (expand-type-expressions texps))
           texpss))
       (result-types (expand-type-expressions result-texps)))
      (let ((the-proc-types
              (map proc-type arg-typess result-types)))
        (let
          ((tenv-for-body               ; type env for body and procs
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

;;;;;;;;;;;;;;;; typechecking list expressions ;;;;;;;;;;;;;;;;

;; making this more flexible can be an exercise.

(define type-of-list-exp 
  (lambda (car-type other-types exp1 exps)
    (for-each 
      (lambda (other-type exp)
        (check-equal-type! car-type other-type (list exp1 exp)))
      other-types
      exps)
    (list-type car-type)))

;; ditto here.

(define type-of-cons-exp
  (lambda (car-type cdr-type exp)
    (cases type cdr-type
      (list-type (type1)
        (check-equal-type! car-type type1 exp)
        cdr-type)
      (else (eopl:error 'type-of-cons-exp
              "Cdr not a list type in expression ~%~s ~%cdr type: ~s"
              exp 
              (type-to-external-form cdr-type))))))

(define type-of-car-exp
  (lambda (type1 exp)
    (cases type type1
      (list-type (type2) type2)
      (else (eopl:error 'type-of-car-exp
              "Argument not of list type in expression ~%~s ~%type: ~s"
              exp
              (type-to-external-form type1))))))

(define type-of-cdr-exp
  (lambda (type1 exp)
    (cases type type1
      (list-type (type2) type1)
      (else (eopl:error 'type-of-cdr-exp
              "Argument not of list type in expression ~%~s ~%type: ~s"
              exp
              (type-to-external-form type1))))))

(define type-of-nil-exp
  (lambda (type1)
    (list-type type1)))

(define type-of-null?-exp
  (lambda (type1 exp)
    (cases type type1
      (list-type (type2) bool-type)
      (else (eopl:error 'type-of-null?-exp
              "Argument not of list type in expression ~%~s ~%type: ~s"
              exp
              (type-to-external-form type1))))))    

;;;;;;;;;;;;;;;; types of new expressions ;;;;;;;;;;;;;;;;

(define type-of-new-obj-exp
  (lambda (class-name rand-types rands exp)
    (cases static-class (statically-lookup-class class-name)
      (a-static-class
        (class-name super-name specifier field-ids
          field-types methods)
        (cases abstraction-specifier specifier
          (abstract-specifier ()
            (eopl:error 'type-of-new-obj-exp
              "Can't instantiate abstract class ~s"
              class-name))
          (concrete-specifier ()
            (begin
              ;; check the initialization
              (type-of-method-app-exp
                (class-type class-name) 
                'initialize
                rand-types
                rands
                exp)
              ;; and return the class type
              (class-type class-name))))))))

(define type-of-method-app-exp ;^ like find-method-and-apply
  (lambda (obj-type msg rand-types rands exp)
    (cases type obj-type
      (class-type (class-name)
        (type-of-method-app-or-super-call
          #f class-name msg rand-types rands exp))
      (else
        (eopl:error 'type-of-method-app-exp
          "Can't send message to non-object ~s in ~%~s"
          obj-type exp)))))

(define type-of-super-call-exp
  (lambda (super-name msg rand-types rands exp)
    (type-of-method-app-or-super-call
      #t super-name msg rand-types rands exp)))

(define type-of-method-app-or-super-call
  (lambda (super-call? host-name msg rand-types rands exp)
    (let ((method
            (statically-lookup-method msg
              (static-class->methods
                (statically-lookup-class host-name)))))
      (if (static-method-struct? method)
        (cases static-method-struct method
          (a-static-method-struct (method-name specifier
                                    method-type super-name)
            (let ((result-type
                    (type-of-application
                      method-type rand-types '() rands exp)))
                                        ;^ rator arg will never be used
              (if super-call?
                (cases abstraction-specifier specifier
                  (concrete-specifier () result-type)
                  (abstract-specifier ()
                    (eopl:error 'type-of-method-or-super-call
                      (string-append
                        "Super call on abstract method ~s "
                        "in class ~s in~%~s")
                      msg host-name exp)))
                result-type))))
        (eopl:error 'type-of-method-app-exp
          "Class ~s has no method for ~s in ~%~s"
          host-name msg exp)))))        

(define type-of-instanceof-exp
  (lambda (ty class-name exp)
    (cases type ty
      (class-type (name)
        (if (statically-is-subclass? class-name 'object)
          bool-type
          (eopl:error 'type-of-instanceof-exp
            "Unknown class ~s in ~%~s" name exp)))
      (else
        (eopl:error 'type-of-expression
          "~s not an object type in ~%~s" ty exp)))))

(define type-of-cast-exp
  (lambda (ty name1 exp)
    (cases type ty
      (class-type (name2)
        (if (or
              (statically-is-subclass? name1 name2)
              (statically-is-subclass? name2 name1))
          (class-type name1)
          (eopl:error 'type-of-expression
            "~s incomparable with ~s in ~%~s"
            name1 name2 exp)))
      (else
        (eopl:error 'type-of-expression
          "~s not an object type in ~%~s"
          ty exp)))))

;^;;;;;;;;;;;;;;; typechecking class decls ;;;;;;;;;;;;;;;;

(define-datatype static-class static-class?
  (a-static-class
    (class-name symbol?)  
    (super-name symbol?)
    (specifier abstraction-specifier?)  
    (field-ids (list-of symbol?))       
    (field-types (list-of type?))       
    (methods static-method-environment?))) 

;;; selectors at end of file

(define statically-elaborate-class-decls!
  (lambda (c-decls)
    (initialize-static-class-env!)
    (for-each statically-elaborate-class-decl! c-decls)))

(define statically-elaborate-class-decl!
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (specifier class-name super-name
                      field-texps field-ids m-decls)
        (let ((field-ids
                (append
                  (if (eqv? super-name 'object) 
                    '()
                    (static-class->field-ids 
                      (statically-lookup-class super-name)))
                  field-ids))
              (field-types
                (append
                  (if (eqv? super-name 'object) 
                    '()
                    (static-class->field-types
                      (statically-lookup-class super-name)))
                  (expand-type-expressions field-texps)))
              (methods
                (statically-roll-up-method-decls
                  (ensure-no-duplicates m-decls class-name)
                  specifier
                  class-name
                  super-name)))
          ;; first set up the class env-- this is needed before
          ;;  checking self in the method bodies.
          (add-to-static-class-env!
            (a-static-class
              class-name
              super-name
              specifier
              field-ids
              field-types
              methods))
          ;; if this is a concrete class, check to see that abstract
          ;; methods have been filled in:
          (check-for-abstract-methods!  
            specifier methods class-name) 
          ;; then check the method bodies:
          (for-each 
            (lambda (m-decl)
              (typecheck-method-decl! m-decl
                class-name super-name field-ids field-types))
            m-decls))))))

;; produces a static-method-env (= list of static-method)
(define statically-roll-up-method-decls
  (lambda (m-decls specifier self-name super-name)
    (statically-merge-methods self-name
      (if (eqv? super-name 'object)
        '()
        (static-class->methods
          (statically-lookup-class super-name)))
      (map
        (lambda (m-decl)
          (method-decl-to-static-method-struct
            m-decl specifier super-name))
        m-decls))))

(define statically-merge-methods
  (lambda (class-name super-methods methods)
    (cond
      ((null? super-methods) methods)
      (else
        (let ((overriding-method
                (statically-lookup-method
                  (static-method->method-name
                    (car super-methods))
                  methods)))
          (if overriding-method
            (if (or
                  (eqv? ;^ initialize must be able to change types
                    'initialize
                    (static-method->method-name
                      (car super-methods)))
                  (equal? 
                    (static-method->type overriding-method)
                    (static-method->type
                      (car super-methods))))
              (cons overriding-method
                (statically-merge-methods
                  class-name
                  (cdr super-methods)
                  (remove-method overriding-method methods)))
              (eopl:error 'statically-merge-methods
                (string-append
                  "Overriding method ~s in class ~s of"
                  "wrong type~% original: ~s~%new:      ~s")
                (static-method->method-name overriding-method)
                class-name
                (type-to-external-form
                  (static-method->type (car super-methods)))
                (type-to-external-form
                  (static-method->type overriding-method))))
            (cons (car super-methods)
              (statically-merge-methods
                class-name
                (cdr super-methods)
                methods))))))))

;; ensure there are no duplicates in m-decls.  Returns m-decls if ok,
;; else reports error.

(define ensure-no-duplicates
  (lambda (m-decls class-name)
    (let check-for-dups
      ((names (map method-decl->name m-decls)))
      (cond
        ((null? names) m-decls)
        ((memv (car names) (cdr names))
         (eopl:error 'statically-merge-methods
           "Duplicate method ~s in class ~s"
           (car names)
           class-name))
        (else (check-for-dups (cdr names)))))))

;; call this a struct because static method means something else.

(define-datatype static-method-struct static-method-struct?
  (a-static-method-struct
    (method-name symbol?)
    (specifier abstraction-specifier?)
    (type type?)
    (super-name symbol?)))

(define method-decl-to-static-method-struct
  (lambda (m-decl specifier super-name)
    (cases method-decl m-decl
      (a-method-decl (result-texp name id-texps ids body)
        (a-static-method-struct
          name
          (concrete-specifier)
          (proc-type
            (expand-type-expressions id-texps)
            (expand-type-expression result-texp))
          super-name))
      (an-abstract-method-decl (result-texp name id-texps ids)
        (a-static-method-struct
          name
          (abstract-specifier)
          (proc-type
            (expand-type-expressions id-texps)
            (expand-type-expression result-texp))
          super-name)))))

(define check-for-abstract-methods!
  (lambda (specifier methods class-name)
    (cases abstraction-specifier specifier
      (abstract-specifier () #t)
      (concrete-specifier ()
        (for-each
          (lambda (method)
            (cases abstraction-specifier
              (static-method->abstraction-specifier method) 
              (concrete-specifier () #t)
              (abstract-specifier ()
                (eopl:error 'check-for-abstract-methods!
                  "Abstract method ~s in concrete class ~s"
                  (static-method->method-name method)
                  class-name))))
          methods)))))

; this sets up the environment exactly like interp.scm

; specifier is not used
; (define typecheck-method-decl!
;   (lambda (m-decl specifier self-name super-name field-ids
;             field-types)
;     (cases method-decl m-decl
;       (a-method-decl (result-texp name id-texps ids body)
;         (let ((id-types (expand-type-expressions id-texps)))
;           (let ((tenv
;                   (extend-tenv
;                     (cons '%super (cons 'self ids))
;                     (cons (class-type super-name)
;                       (cons (class-type self-name)
;                         id-types))
;                     (extend-tenv
;                       field-ids field-types (empty-tenv)))))
;             (let ((body-type (type-of-expression body tenv)))
;               (check-is-subtype!
;                 body-type
;                 (expand-type-expression result-texp)
;                 m-decl)))))
;       (an-abstract-method-decl (result-texp name id-texps ids)
;         #t))))

(define typecheck-method-decl!
  (lambda (m-decl self-name super-name field-ids field-types)
    (cases method-decl m-decl
      (a-method-decl (result-texp name id-texps ids body)
        (let ((id-types (expand-type-expressions id-texps)))
          (let ((tenv
                  (extend-tenv
                    (cons '%super (cons 'self ids))
                    (cons (class-type super-name)
                      (cons (class-type self-name)
                        id-types))
                    (extend-tenv
                      field-ids field-types (empty-tenv)))))
            (let ((body-type (type-of-expression body tenv)))
              (check-is-subtype!
                body-type
                (expand-type-expression result-texp)
                m-decl)))))
      (an-abstract-method-decl (result-texp name id-texps ids)
        #t))))

;;;;;;;;;;;;;;;; types ;;;;;;;;;;;;;;;;

(define-datatype type type?
  (atomic-type
    (name symbol?))
  (list-type    ;\new2
    (value-type type?))
  (class-type   ;\new2
    (name symbol?))
  (proc-type
    (arg-types (list-of type?))
    (result-type type?)))

(define check-equal-type!
  (lambda (t1 t2 exp)
    (if (equal? t1 t2)
      #t
      (eopl:error 'type-of-expression
        "Types didn't match: ~s != ~s in~%~s"
        (type-to-external-form t1)
        (type-to-external-form t2)
        exp))))

(define check-is-subtype!
  (lambda (t1 t2 exp)
    (if (is-subtype? t1 t2)
      #t
      (eopl:error 'check-is-subtype!
        "~s is not a subtype of ~s in ~%~s"
        (type-to-external-form t1)
        (type-to-external-form t2)
        exp))))

;; need this for typing cast expressions
(define is-subtype? 
  (lambda (t1 t2)
    (cases type t1
      (class-type (name1)
        (cases type t2
          (class-type (name2)
            (statically-is-subclass? name1 name2))
          (else #f)))
      (else (equal? t1 t2)))))   

(define statically-is-subclass?
  (lambda (name1 name2)
    (or
      (eqv? name1 name2)
      (if (eqv? name1 'object)
        #f
        (let ((super-name
                (static-class->super-name (statically-lookup-class name1))))
          (statically-is-subclass? super-name name2))))))

(define expand-type-expression
  (lambda (texp)
    (cases type-exp texp
      (int-type-exp () int-type)
      (bool-type-exp () bool-type)
      (void-type-exp () void-type)
      (list-type-exp (texp)       ;\new2
        (list-type (expand-type-expression texp)))
      (class-type-exp (name) (class-type name))    ;\new1
      (proc-type-exp (arg-texps result-texp)
        (proc-type
          (expand-type-expressions arg-texps)
          (expand-type-expression  result-texp))))))

(define expand-type-expressions
  (lambda (texps)
    (map expand-type-expression texps)))

(define int-type (atomic-type 'int))
(define bool-type (atomic-type 'bool))
(define void-type (atomic-type 'void))

;;;;;;;;;;;;;;;; type environments ;;;;;;;;;;;;;;;;

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
    (syms (list-of symbol?))
    (vals (list-of type?))
    (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
        (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (syms vals env)
        (let ((pos (list-find-last-position sym syms)))
          (if (number? pos)
            (list-ref vals pos)
            (apply-tenv env sym)))))))
    

;;;;;;;;;;;;;;;; external form of types ;;;;;;;;;;;;;;;;

(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (atomic-type (name) name)
      (class-type (name) name)
      (list-type (ty) (list 'list (type-to-external-form ty)))
      (proc-type (arg-types result-type)
        (append
          (formal-types-to-external-form arg-types)
          '(->)
          (list (type-to-external-form result-type)))))))

(define formal-types-to-external-form
  (lambda (types)
    (if (null? types)
      '()
      (if (null? (cdr types))
        (list (type-to-external-form (car types)))
        (cons
          (type-to-external-form (car types))
          (cons '*
                (formal-types-to-external-form (cdr types))))))))

;;;;;;;;;;;;;;;; static method environments ;;;;;;;;;;;;;;;;

(define static-method-environment?
  (list-of static-method-struct?))

(define statically-lookup-method 
  (lambda (m-name methods)       
    (cond
      ((null? methods) #f)
      ((eqv? m-name (static-method->method-name (car methods)))
       (car methods))
      (else (statically-lookup-method m-name (cdr methods))))))

;;;;;;;;;;;;;;;; static class environments ;;;;;;;;;;;;;;;;

(define the-static-class-env '())

(define initialize-static-class-env!
  (lambda ()
    (set! the-static-class-env '())))

(define add-to-static-class-env!
  (lambda (class)
    (set! the-static-class-env (cons class the-static-class-env))))

(define statically-lookup-class 
  (lambda (name)
    (let loop ((env the-static-class-env))
      (cond
        ((null? env)
         (eopl:error 'statically-lookup-class
           "Unknown class ~s" name))
        ((eqv? (static-class->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

(define static-class->class-name
  (lambda (sc)
    (cases static-class sc
      (a-static-class
        (class-name super-name specifier  field-ids
          field-types methods)
        class-name))))

(define static-class->super-name
  (lambda (sc)
    (cases static-class sc
      (a-static-class
        (class-name super-name specifier  field-ids
          field-types methods)
        super-name))))

(define static-class->field-ids
  (lambda (sc)
    (cases static-class sc
      (a-static-class
        (class-name super-name specifier  field-ids
          field-types methods)
        field-ids))))

(define static-class->field-types
  (lambda (sc)
    (cases static-class sc
      (a-static-class
        (class-name super-name specifier  field-ids
          field-types methods)
        field-types))))

(define static-class->methods
  (lambda (sc)
    (cases static-class sc
      (a-static-class
        (class-name super-name specifier field-ids
          field-types methods)
        methods))))

(define static-method->method-name
  (lambda (sm)
    (cases static-method-struct sm
      (a-static-method-struct
        (method-name specifier type super-name)
        method-name))))

(define static-method->abstraction-specifier
  (lambda (sm)
    (cases static-method-struct sm
      (a-static-method-struct
        (method-name specifier type super-name)
        specifier))))

(define static-method->type
  (lambda (sm)
    (cases static-method-struct sm
      (a-static-method-struct
        (method-name specifier type super-name)
        type))))

(define static-method->super-name
  (lambda (sm)
    (cases static-method-struct sm
      (a-static-method-struct
        (method-name specifier type super-name)
        super-name))))

(define class-type->name
  (lambda (ty)
    (cases type ty
      (class-type (name) name)
      (else
        (eopl:error 'class-type->name
          "Not a class type: ~s" ty)))))

(define method-decl->name
  (lambda (m-decl)
    (cases method-decl m-decl
      (a-method-decl (result-texp name id-texps ids body) name)
      (an-abstract-method-decl (result-texp name id-texps ids) name))))