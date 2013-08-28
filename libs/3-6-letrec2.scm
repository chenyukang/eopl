;;; environment structure for letrec, using AST with new rib type

(let ((time-stamp "Time-stamp: <2000-09-07 07:27:43 dfried>"))
  (eopl:printf
    "letrec2.scm -- AST rep of environments with 2 kinds of ribs ~a~%"
    (substring time-stamp 13 29)))

(define-datatype environment environment? 
  (empty-env-record)             
  (extended-env-record
    (syms (list-of symbol?))
    (vals vector?)
    (env environment?))
  (recursively-extended-env-record ;\new5
    (proc-names (list-of symbol?))
    (idss (list-of (list-of symbol?)))
    (bodies (list-of expression?))
    (env environment?)))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
      proc-names idss bodies old-env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
            (vector-ref vals pos)
            (apply-env old-env sym))))
      (recursively-extended-env-record (proc-names idss ;\new9
                                         bodies old-env) 
        (let ((pos (rib-find-position sym proc-names)))
          (if (number? pos)
            (closure
              (list-ref idss pos)
              (list-ref bodies pos)
              env)
            (apply-env old-env sym)))))))

(define rib-find-position
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define close-proc-decl
  (lambda (p-decl env)
    (cases proc-decl p-decl
      (a-proc-decl (name ids body) (closure ids body env)))))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

