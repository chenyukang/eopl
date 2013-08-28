;; This is 5-4-4.scm: flat methods

(let ((time-stamp "Time-stamp: <2001-02-24 10:31:05 dfried>"))
  (eopl:printf "5-4-4.scm - flat methods ~a~%"
    (substring time-stamp 13 29)))


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
            (roll-up-method-decls c-decl super-name field-ids)))))))

;^ 5-4-4  ;;; This may change for second printing.
(define roll-up-method-decls
  (lambda (c-decl super-name field-ids)
    (merge-methods
      (class-name->methods super-name)
      (map
        (lambda (m-decl)
          (a-method m-decl super-name field-ids))
        (class-decl->method-decls c-decl)))))

;^ 5-4-4
(define merge-methods
  (lambda (super-methods methods)
    (cond
      ((null? super-methods) methods)
      (else
        (let ((overriding-method
                (lookup-method
                  (method->method-name (car super-methods))
                  methods)))
          (if (method? overriding-method)
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

;;;;;;;;;;;;;;;; objects ;;;;;;;;;;;;;;;;

;; an object is now just a single part, with a vector representing the
;; managed storage for the all the fields. 

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
    (m-decl method-decl?)
    (s-name symbol?)
    (field-ids (list-of symbol?))))

(define find-method-and-apply           
  (lambda (m-name host-name self args)  ;^ 5-4-4: no more loop
    (let ((method (lookup-method m-name 
                    (class-name->methods host-name))))
      (if (method? method)
        (apply-method method host-name self args)
        (eopl:error 'find-method-and-apply
          "No method for name ~s" m-name)))))

(define apply-method                    
  (lambda (method host-name self args)
    (eval-expression (method->body method) 
      (extend-env
        (cons '%super (cons 'self (method->ids method))) 
        (cons
          (method->super-name method) ;; 5-4-4
          (cons self args))
        (extend-env-refs         
          (method->field-ids method) 
          (object->fields self)
          (empty-env))))))

(define rib-find-position               
  (lambda (name symbols)
    (list-find-last-position name symbols)))

;;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

(define method-environment? (list-of method?)) 

(define lookup-method              ;^ 5-4 method-decl => method
  (lambda (m-name methods)         
    (cond
      ((null? methods) #f)
      ((eqv? m-name (method->method-name (car methods)))
       (car methods))
      (else (lookup-method m-name (cdr methods))))))

;;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;;; we'll use the list of classes (not class decls)

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
        ((null? env) (eopl:error 'lookup-class
                       "Unknown class ~s" name))
        ((eqv? (class->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

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

