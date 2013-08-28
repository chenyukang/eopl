;; This is 5-4-2.scm: flat fields

(let ((time-stamp "Time-stamp: <2000-12-13 16:22:39 wand>"))
  (eopl:printf "5-4-2.scm - flat fields ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; classes ;;;;;;;;;;;;;;;;

;;; classes are represented by their class-decls.

(define class? class-decl?)             ; not used

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
      (make-vector (roll-up-field-length class-name)))))

(define roll-up-field-length            
  (lambda (class-name)
    (if (eqv? class-name 'object)
      0
      (+
        (roll-up-field-length
          (class-name->super-name class-name))
        (length (class-name->field-ids class-name))))))

;;;;;;;;;;;;;;;; methods ;;;;;;;;;;;;;;;;

(define find-method-and-apply
  (lambda (m-name host-name self args)
    (if (eqv? host-name 'object)
      (eopl:error 'find-method-and-apply
        "No method for name ~s" m-name)
      (let ((m-decl (lookup-method-decl m-name
                      (class-name->method-decls host-name))))
        (if (method-decl? m-decl)
            (apply-method m-decl host-name self args)
            (find-method-and-apply m-name 
              (class-name->super-name host-name)
              self args))))))

;^ 5-4-2
(define apply-method
  (lambda (m-decl host-name self args)
    (let ((ids (method-decl->ids m-decl))
          (body (method-decl->body m-decl))
          (super-name (class-name->super-name host-name))
          (field-ids (roll-up-field-ids host-name))   ;\new2
          (fields (object->fields self)))
      (eval-expression body
        (extend-env
          (cons '%super (cons 'self ids))
          (cons super-name (cons self args))
          (extend-env-refs field-ids fields (empty-env))))))) ;\new1

;^ 5-4-2
(define roll-up-field-ids               
  (lambda (class-name)
    (if (eqv? class-name 'object)
      '()
      (append
        (roll-up-field-ids
          (class-name->super-name class-name))
        (class-name->field-ids class-name)))))

;^ 5-4-2
(define rib-find-position               
  (lambda (name symbols)
    (list-find-last-position name symbols)))

;^;;;;;;;;;;;;;;; method environments ;;;;;;;;;;;;;;;;

(define lookup-method-decl 
  (lambda (m-name m-decls)
    (cond
      ((null? m-decls) #f)
      ((eqv? m-name (method-decl->method-name (car m-decls)))
       (car m-decls))
      (else (lookup-method-decl m-name (cdr m-decls))))))

;^;;;;;;;;;;;;;;; class environments ;;;;;;;;;;;;;;;;

;^;; we'll just use the list of class-decls.

(define the-class-env '())

(define elaborate-class-decls!
  (lambda (c-decls)
    (set! the-class-env c-decls)))

(define lookup-class
  (lambda (name)
    (let loop ((env the-class-env))
      (cond
        ((null? env)
         (eopl:error 'lookup-class
           "Unknown class ~s" name))
        ((eqv? (class-decl->class-name (car env)) name) (car env))
        (else (loop (cdr env)))))))

;^;;;;;;;;;;;;;;; selectors ;;;;;;;;;;;;;;;;

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
    (class-decl->field-ids
      (object->class-decl object))))

(define class-name->super-name
  (lambda (class-name)
    (class-decl->super-name (lookup-class class-name))))

(define class-name->field-ids
  (lambda (class-name)
    (class-decl->field-ids (lookup-class class-name))))

(define class-name->field-ids
  (lambda (class-name)
    (class-decl->field-ids (lookup-class class-name))))

(define class-name->method-decls
  (lambda (class-name)
    (class-decl->method-decls (lookup-class class-name))))







