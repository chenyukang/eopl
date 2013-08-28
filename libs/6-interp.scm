;;; interp.scm: interpreter for typed OO language

(let ((time-stamp "Time-stamp: <2001-05-11 10:44:00 dfried>"))
  (eopl:printf "six-interp.scm ~a~%" (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

(define eval-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (c-decls body)
        (elaborate-class-decls! c-decls) ; new for ch6
        (eval-expression body (init-env))))))

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (lexvar-exp (depth pos) (apply-env-lexvar env depth pos))
      (true-exp () 1)
      (false-exp () 0)
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
      (proc-exp (type-exps ids body)
        (closure ids body env))
      (app-exp (rator rands)
        (let ((proc (eval-expression rator env))
              (args (eval-rands      rands env)))
          (if (procval? proc)
            (apply-procval proc args)
            (eopl:error 'eval-expression 
              "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (result-texps proc-names type-expss idss bodies
                    letrec-body)
        (eval-expression letrec-body
          (extend-env-recursively proc-names idss bodies env)))
      (varassign-exp (id rhs-exp)
        (begin
          (setref! 
            (apply-env-ref env id)
            (eval-expression rhs-exp env))
          1))

      (begin-exp (exp1 exps)
        (let loop ((acc (eval-expression exp1 env))
                   (exps exps))
          (if (null? exps) acc
            (loop (eval-expression (car exps) env) (cdr exps)))))


      ;; lists 
      (list-exp (exp exps) 
        (let ((the-car (eval-expression exp env))
              (the-cdr (eval-expressions exps env)))
          (cons the-car the-cdr)))
      (cons-exp (car-exp cdr-exp) 
        (cons
          (eval-expression car-exp env)
          (eval-expression cdr-exp env)))
      (car-exp (exp) 
        (car (eval-expression exp env)))
      (cdr-exp (exp) 
        (cdr (eval-expression exp env)))
      (nil-exp (type-exp) '())
      (null?-exp (exp)
        (if (null? (eval-expression exp env))
          1 0))

;;;;;;;;;;;;;;;; new cases for chap 6 ;;;;;;;;;;;;;;;;

      (new-object-exp (class-name rands)
        (let ((args (eval-rands rands env))
              (obj (new-object class-name)))
          (find-method-and-apply 'initialize class-name obj args)
          obj))

      (method-app-exp (obj-exp method-name rands)
        (let ((args (eval-rands rands env))
              (obj (eval-expression obj-exp env)))
          (find-method-and-apply
            method-name (object->class-name obj) obj args)))

      (super-call-exp (method-name rands)
        (let ((args (eval-rands rands env))
              (obj (apply-env env 'self)))
          (find-method-and-apply method-name
            (apply-env env '%super) obj args)))

;;;;;;;;;;;;;;;; new cases for chapter 8 ;;;;;;;;;;;;;;;;

      ;; oo-6: get method at position pos. pos is a number, not an
      ;; expression. 
      (apply-method-indexed-exp (obj-exp pos rands)
        (let ((obj (eval-expression obj-exp env))
              (args (eval-expressions rands env)))
          (let ((class-name (object->class-name obj)))
            (apply-method
              (list-ref
                (class->methods (lookup-class class-name))
                pos)
              class-name
              obj
              (eval-expressions rands env)))))

      (cast-exp (exp name)
        (let ((obj (eval-expression exp env)))
          (if (is-subclass? (object->class-name obj) name)
            obj
            (eopl:error 'eval-expression
              "Can't cast object to type ~s:~%~s"))))

      (instanceof-exp (exp name)
        (let ((obj (eval-expression exp env)))
          (if (is-subclass? (object->class-name obj) name)
            the-true-value
            the-false-value)))

      (else (eopl:error 'eval-expression
              "~%Illegal expression~%~s" exp))

      )))

(define eval-rands
  (lambda (exps env)
    (map
      (lambda (exp) (eval-expression exp env))
      exps)))

(define eval-expressions
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

(define init-env 
  (lambda ()
    (extend-env
      '(i v x)
      '(1 5 10)
      (empty-env))))


;;;;;;;;;;;;;;;; classes ;;;;;;;;;;;;;;;;

(define-datatype class class?
  (a-class
    (class-name symbol?)  
    (super-name symbol?) 
    (field-length integer?)  
    (field-ids (list-of symbol?))
    (methods method-environment?)))

;;;; constructing classes

(define elaborate-class-decls!
  (lambda (c-decls)
    (initialize-class-env!)
    (for-each elaborate-class-decl! c-decls)))

(define elaborate-class-decl!
  (lambda (c-decl)
    (let ((super-name (class-decl->super-name c-decl)))
      (let ((field-ids  (append
                          (class-name->field-ids super-name)
                          (class-decl->field-ids c-decl))))
        (add-to-class-env!
          (a-class
            (class-decl->class-name c-decl)
            super-name
            (length field-ids)
            field-ids
            (roll-up-method-decls c-decl field-ids)))))))

(define roll-up-method-decls            
  (lambda (c-decl field-ids)
    (let ((super-name (class-decl->super-name c-decl)))
      (merge-methods
        (class-name->methods super-name)
        (map
          (lambda (m-decl)
            (a-method m-decl super-name field-ids))
          (class-decl->method-decls c-decl))))))

(define merge-methods                   
  (lambda (super-methods methods)
    (cond
      ((null? super-methods) methods)
      (else
        (let ((overriding-method
                (lookup-method
                  (method->method-name (car super-methods))
                  methods)))
          (if overriding-method
            (cons overriding-method
              (merge-methods (cdr super-methods)
                (remove-method overriding-method methods)))
            (cons (car super-methods)
              (merge-methods (cdr super-methods)
                 methods))))))))

(define remove-method
  (lambda (method methods)
    (remv method methods)))

(define remv
  (lambda (x lst)
    (cond
      ((null? lst) '())
      ((eqv? (car lst) x) (remv x (cdr lst)))
      (else (cons (car lst) (remv x (cdr lst)))))))

(define is-subclass?
  (lambda (name1 name2)
    (or (eqv? name1 name2)
      (and (not (eqv? name1 'object))
        (is-subclass? (class-name->super-name name1) name2)))))

;;;;;;;;;;;;;;;; objects ;;;;;;;;;;;;;;;;

(define-datatype object object? 
  (an-object
    (class-name symbol?)
    (fields vector?)))

(define new-object                      
  (lambda (class-name)
    (an-object
      class-name
      (make-vector (class-name->field-length class-name))))) 

;;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;

(define-datatype method method?
  (a-method
    (method-decl method-decl?)
    (super-name symbol?)
    (field-ids (list-of symbol?))))

(define find-method-and-apply           
  (lambda (m-name host-name self args)  
    (let ((method (lookup-method m-name 
                    (class-name->methods host-name))))
      (if (method? method)
          (apply-method method host-name self args)
          (eopl:error 'find-method-and-apply
            "No method for name ~s" m-name)))))

(define apply-method
  (lambda (method host-name self args)                ;\new5
    (let ((ids (method->ids method))
          (body (method->body method))
          (super-name (method->super-name method))
          (field-ids (method->field-ids method))       
          (fields (object->fields self)))
      (eval-expression body
        (extend-env
          (cons '%super (cons 'self ids))
          (cons super-name (cons self args))
          (extend-env-refs field-ids fields (empty-env)))))))

'(define apply-method                    
  (lambda (method host-name self args)
    (eval-expression (method->body method) 
      (extend-env
        (cons '%super (cons 'self (method->ids method))) 
        (cons
          (method->super-name method) 
          (cons self args))
        (extend-env-refs         
          (method->field-ids method) 
          (object->fields self)
          (empty-env))))))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

(define method-environment? (list-of method?)) 

(define lookup-method                
  (lambda (m-name methods)           
    (cond
      ((null? methods) #f)
      ((eqv? m-name (method->method-name (car methods)))
       (car methods))
      (else (lookup-method m-name (cdr methods))))))

;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;;; we'll just use the list of class-decls.

(define the-class-env '())

(define initialize-class-env!
  (lambda ()
    (set! the-class-env '())))

(define add-to-class-env!
  (lambda (class)
    (set! the-class-env (cons class the-class-env))))

(define lookup-class                 
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env)
         (eopl:error 'lookup-class
           "Unknown class ~s" name))
        ((eqv? (class->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;;;;;;;;;;;;;;;; booleans ;;;;;;;;;;;;;;;;

(define true-value?
  (lambda (x)
    (not (zero? x))))
        
(define the-true-value 1)
(define the-false-value 0)


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

;;;;;;;;;;;;;;;; references ;;;;;;;;;;;;;;;;

(define-datatype reference reference?
  (a-ref
    (position integer?)
    (vec vector?)))

(define deref 
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
        (vector-ref vec pos)))))

(define setref! 
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
        (vector-set! vec pos val)))
    1))

;;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;

;;; environments ala chapter 3

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vec vector?)              ; can use this for anything.
    (env environment?))
  )

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
              (a-ref pos vals)
              (apply-env-ref env sym)))))))

(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-lexvar
  (lambda (env depth pos)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env-lexvar
          (string-append
            "~%No such lexical address:"
            "depth = ~s position = ~s")
          depth pos))
      (extended-env-record (syms vals env)
        (if (zero? depth)
          (vector-ref vals pos)
          (apply-env-lexvar env (- depth 1) pos))))))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))

(define extend-env-refs extended-env-record)

(define rib-find-position               
  (lambda (name symbols)
    (list-find-last-position name symbols)))

(define list-find-last-position
  (lambda (sym los)
    (let loop
      ((los los) (curpos 0) (lastpos #f))
      (cond
        ((null? los) lastpos)
        ((eqv? sym (car los))
         (loop (cdr los) (+ curpos 1) curpos))
        (else (loop (cdr los) (+ curpos 1) lastpos))))))

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

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((memv (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1) (difference (cdr set1) set2))))))

;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

(define class-decl->abstraction-specifier
  (lambda (the-c-decl)
    (cases class-decl the-c-decl
      (a-class-decl
        (abstraction-specifier class-name super-name field-texps
          field-ids m-decls) 
        abstraction-specifier))))

(define class-decl->class-name
  (lambda (the-c-decl)
    (cases class-decl the-c-decl
      (a-class-decl
        (abstraction-specifier class-name super-name field-texps
          field-ids m-decls) 
        class-name))))

(define class-decl->super-name
  (lambda (the-c-decl)
    (cases class-decl the-c-decl
      (a-class-decl
        (abstraction-specifier class-name super-name field-texps
          field-ids m-decls) 
        super-name))))

(define class-decl->field-texps
  (lambda (the-c-decl)
    (cases class-decl the-c-decl
      (a-class-decl
        (abstraction-specifier class-name super-name field-texps
          field-ids m-decls) 
        field-texps))))

(define class-decl->field-ids
  (lambda (the-c-decl)
    (cases class-decl the-c-decl
      (a-class-decl
        (abstraction-specifier class-name super-name field-texps
          field-ids m-decls) 
        field-ids))))

(define class-decl->method-decls
  (lambda (the-c-decl)
    (cases class-decl the-c-decl
      (a-class-decl
        (abstraction-specifier class-name super-name field-texps
          field-ids m-decls) 
        m-decls))))

(define method-decl->result-texp
  (lambda (md)
    (cases method-decl md
      (a-method-decl
        (result-texp name arg-type-exps ids method-body)
        result-texp) 
      (an-abstract-method-decl
        (result-texp name arg-type-exps ids) result-texp))))

(define method-decl->method-name
  (lambda (md)
    (cases method-decl md
      (a-method-decl (result-texp name arg-type-exps ids method-body)
        name) 
      (an-abstract-method-decl (result-texp name arg-type-exps ids) name))))

(define method-decl->arg-type-exps
  (lambda (md)
    (cases method-decl md
      (a-method-decl (result-texp name arg-type-exps ids method-body)
        arg-type-exps)
      (an-abstract-method-decl (result-texp name arg-type-exps ids)
        arg-type-exps))))

(define method-decl->ids
  (lambda (md)
    (cases method-decl md
      (a-method-decl (result-texp name arg-type-exps ids method-body)
        ids)
      (an-abstract-method-decl (result-texp name arg-type-exps ids)
        ids))))

(define method-decl->body
  (lambda (md)
    (cases method-decl md
      (a-method-decl (result-texp name arg-type-exps ids
                       method-body)
        method-body)
      (an-abstract-method-decl (result-texp name
                                 arg-type-exps ids) 
        (eopl:error 'method-decl->body
          "Can't take body of abstract method")))))

(define method-decls->method-names
  (lambda (mds)
    (map method-decl->method-name mds)))

(define class->class-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        class-name))))

(define class->super-name
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        super-name))))

(define class-name->super-name
  (lambda (class-name)
    (class->super-name (lookup-class class-name))))

(define class->field-length
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        field-length))))

(define class->field-ids
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        field-ids))))

(define class->methods
  (lambda (c-struct)
    (cases class c-struct
      (a-class (class-name super-name field-length field-ids methods)
        methods))))

(define object->class-name
  (lambda (obj)
    (cases object obj
      (an-object (class-name fields)
        class-name))))

(define object->fields
  (lambda (obj)
    (cases object obj
      (an-object (class-decl fields)
        fields))))

(define object->class-decl
  (lambda (obj)
    (lookup-class (object->class-name obj))))

(define object->field-ids
  (lambda (object)
    (class->field-ids
      (object->class-decl object))))

(define class-name->super-name
  (lambda (class-name)
    (class->super-name (lookup-class class-name))))

(define class-name->field-ids
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->field-ids (lookup-class class-name)))))

(define class-name->methods
  (lambda (class-name)
    (if (eqv? class-name 'object) '()
      (class->methods (lookup-class class-name)))))

(define class-name->field-length
  (lambda (class-name)
    (if (eqv? class-name 'object)
        0
        (class->field-length (lookup-class class-name)))))

(define method->method-decl
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl super-name field-ids) meth-decl))))

(define method->super-name
  (lambda (meth)
    (cases method meth
      (a-method (meth-decl super-name field-ids) super-name))))

(define method->field-ids
  (lambda (meth)
    (cases method meth
      (a-method (method-decl super-name field-ids) field-ids))))

(define method->method-name
  (lambda (method)
    (method-decl->method-name (method->method-decl method))))

(define method->body
  (lambda (method)
    (method-decl->body (method->method-decl method))))

(define method->ids
  (lambda (method)
    (method-decl->ids (method->method-decl method))))

