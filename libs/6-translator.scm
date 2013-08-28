(let ((time-stamp "Time-stamp: <2000-12-15 15:44:04 wand>"))
  (eopl:printf "6-translator.scm ~a~%" (substring time-stamp 13 32)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;; The Tranlator ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define translation-of-program
  (lambda (pgm)
    (let ((pgm-type (type-of-program pgm))) ;^ set up static class env
      (cases program pgm
        (a-program (c-decls exp)
          (a-program
            (translation-of-class-decls c-decls)
            (translation-of-expression exp (empty-tenv))))))))

(define translation-of-expression
  (lambda (exp tenv)
    (cases expression exp
      (lit-exp (number) exp)
      (true-exp () exp)
      (false-exp () exp)
      (var-exp (id) exp)  
      (primapp-exp (prim rands)
        (primapp-exp prim
          (translations-of-expressions rands tenv)))
      (if-exp (test-exp true-exp false-exp)
        (if-exp (translation-of-expression test-exp tenv)
          (translation-of-expression true-exp tenv)
          (translation-of-expression false-exp tenv)))
      (app-exp (rator rands) 
        (app-exp
          (translation-of-expression rator tenv)
          (translations-of-expressions rands tenv))) 
      ;;^ these next guys manipulate tenv
      (let-exp (ids rands body)
        (translation-of-let-exp ids rands body tenv))
      (proc-exp (id-texps ids body)
        (translation-of-proc-exp id-texps ids  body tenv))
      (letrec-exp (result-texps proc-names id-texpss idss
                    bodies letrec-body) 
        (translation-of-letrec-exp
          result-texps proc-names id-texpss idss bodies
          letrec-body tenv))
      ;; more dull cases
      (varassign-exp (id rhs)     
        (varassign-exp id (translation-of-expression rhs tenv)))
      (begin-exp (exp1 exps)                
        (begin-exp
           (translation-of-expression exp1 tenv)
           (translations-of-expressions exps tenv)))
      (list-exp (exp1 exps) 
        (list-exp
           (translation-of-expression exp1 tenv)
           (translations-of-expressions exps tenv)))
      (cons-exp (car-exp cdr-exp)
        (cons-exp
          (translation-of-expression car-exp tenv)
          (translation-of-expression cdr-exp tenv)))
      (car-exp (exp1) 
        (car-exp (translation-of-expression exp1 tenv)))
      (cdr-exp (exp1) 
        (cdr-exp (translation-of-expression exp1 tenv)))
      (nil-exp (type-exp) exp)
      (null?-exp (exp1) 
        (null?-exp (translation-of-expression exp1 tenv)))
      (new-object-exp (class-name rands)
        (new-object-exp class-name
          (translations-of-expressions rands tenv)))
      (super-call-exp (msg rands) 
        (super-call-exp msg
          (translations-of-expressions rands tenv)))
      ;; these guys do something interesting:
      (method-app-exp (obj-exp msg rands) 
        (translation-of-method-app-exp
          obj-exp msg rands tenv))
      (instanceof-exp (obj-exp name)
        (translation-of-instanceof-exp obj-exp name tenv))
      (cast-exp (obj-exp name)
        (translation-of-cast-exp obj-exp name tenv))
;&
      (else (eopl:error 'eval-expression
              "Illegal expression~%~s" exp))
      )))

(define translations-of-expressions
  (lambda (exps tenv)
    (map (lambda (exp) (translation-of-expression exp tenv)) exps)))

(define translation-of-proc-exp
  (lambda (id-texps ids body tenv)
    (let ((id-types (expand-type-expressions id-texps)))
      (proc-exp
        id-texps
        ids
        (translation-of-expression body
          (extend-tenv ids id-types tenv))))))

(define translation-of-let-exp
  (lambda (ids rands body tenv)
    (let ((tenv-for-body
            (extend-tenv
              ids
              (types-of-expressions rands tenv)
              tenv)))
      (let-exp
        ids
        (translations-of-expressions rands tenv)
        (translation-of-expression body tenv-for-body)))))

(define translation-of-letrec-exp
  (lambda (result-texps proc-names id-texpss idss bodies
            letrec-body tenv)
    (let ((id-typess (map expand-type-expressions id-texpss))
          (result-types
            (expand-type-expressions result-texps)))
      (let ((the-proc-types
              (map proc-type id-typess result-types)))
        (let ((tenv-for-body ;^ type env for body and all procs
                (extend-tenv proc-names the-proc-types tenv)))
          (letrec-exp result-texps proc-names id-texpss idss
            (map
              (lambda (id-types ids body)
                (translation-of-expression body 
                  (extend-tenv ids id-types tenv-for-body)))
              id-typess idss bodies)
            (translation-of-expression
              letrec-body
              tenv-for-body)))))))

;;;;;;;;;;;;;;;; code for class decls ;;;;;;;;;;;;;;;;

(define translation-of-class-decls
  (lambda (c-decls)
    (map translation-of-class-decl c-decls)))

(define translation-of-class-decl
  (lambda (c-decl)
    (cases class-decl c-decl
      (a-class-decl (specifier class-name super-name
                      local-field-texps local-field-ids
                      m-decls)
        (a-class-decl specifier class-name super-name
          local-field-texps local-field-ids
          (map
            (lambda (method-decl)
              (translation-of-method-decl method-decl
                class-name))
            m-decls))))))

(define translation-of-method-decl
  (lambda (m-decl class-name)
    (let ((class (statically-lookup-class class-name)))
      (let ((super-name (static-class->super-name class))
            (field-ids  (static-class->field-ids class))
            (field-types (static-class->field-types class)))
        (cases method-decl m-decl
          (a-method-decl (result-texp name id-texps ids body)
            (let ((id-types
                    (expand-type-expressions id-texps)))
              (let ((tenv
                      (extend-tenv
                        (cons '%super (cons 'self ids))
                        (cons (class-type super-name)
                          (cons (class-type class-name)
                            id-types))
                        (extend-tenv field-ids field-types
                          (empty-tenv)))))
                (a-method-decl
                  result-texp name id-texps ids
                  (translation-of-expression body tenv)))))
          (an-abstract-method-decl (result-texp name id-texps
                                    ids)
            m-decl))))))


;; here's where we finally do something
(define translation-of-method-app-exp
  (lambda (obj-exp msg rands tenv)
    (let ((obj-type (type-of-expression obj-exp tenv)))
      (cases type obj-type
        (class-type (class-name)
          (let ((class (statically-lookup-class class-name)))
            (let ((pos
                    (list-index
                      (lambda (method)
                        (eqv?
                          msg
                          (static-method->method-name
                            method)))
                      (static-class->methods class))))
              (if (number? pos)
                (apply-method-indexed-exp
                  (translation-of-expression obj-exp tenv)
                  pos
                  (translations-of-expressions rands tenv))
                (eopl:error 'translation-of-method-app-exp
                  (string-append
                    "Shouldn't have gotten here: Class "
                    "~s has no method for ~s in ~%~s")
                  class-name
                  msg
                  (method-app-exp obj-exp msg rands))))))
        (else
          (eopl:error 'translation-of-method-app-exp
            (string-append
              "Shouldn't have gotten here: "
              "Can't send message to non-object"
              "~s in ~%~s")
            obj-type
            (method-app-exp obj-exp msg rands)))))))

(define translation-of-instanceof-exp
  (lambda (obj-exp name tenv)
    (let ((obj-type (type-of-expression obj-exp tenv))
          (obj-code (translation-of-expression obj-exp tenv)))
      (cases type obj-type
        (class-type (obj-class-name)
          (cond
            ((statically-is-subclass? obj-class-name name)
             ;; upcast -- always true
             (begin-exp obj-code (list (true-exp)))) 
            ((statically-is-subclass? name obj-class-name)
             ;; downcast -- generate test
             (instanceof-exp obj-code name)) 
            (else
              ;; incomparable -- so always false
              (begin-exp obj-code (list (false-exp))))))
        (else
          (eopl:error 'translation-of-instanceof-expression
            (string-append
              "Shouldn't have gotten here:"
              " ~s not an object type in ~%~s")
              obj-type
              (instanceof-exp obj-exp name)))))))

(define translation-of-cast-exp
  (lambda (obj-exp name tenv)
    (let ((obj-type (type-of-expression obj-exp tenv))
          (obj-code (translation-of-expression obj-exp tenv)))
      (cases type obj-type
        (class-type (obj-class-name)
          (cond
            ((statically-is-subclass? obj-class-name name)
             ;; upcast -- nothing to do
             obj-code)
            ((statically-is-subclass? name obj-class-name)
             ;; downcast -- generate cast
             (cast-exp obj-code name))
            (else
              (eopl:error 'translation-of-cast-exp
                (string-append
                  "Shouldn't have gotten here: "
                  "~s incomparable with ~s in ~%~s")
                obj-class-name
                name
                (cast-exp obj-exp name)))))
        (else
          (eopl:error 'translation-of-cast-expression
            (string-append
              "Shouldn't have gotten here: "
              "~s not an object type in ~%~s")
              obj-type
              (cast-exp obj-exp name)))))))
