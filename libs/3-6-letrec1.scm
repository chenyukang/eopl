;;; environment structure for letrec, using procedural representation

(let ((time-stamp "Time-stamp: <2000-09-07 07:26:52 dfried>"))
  (eopl:printf "letrec1.scm -- procedural rep of environments ~a~%"
    (substring time-stamp 13 29)))

(define environment? procedure?)

(define apply-env
  (lambda (env sym)
    (env sym)))

(define empty-env
  (lambda ()
    (lambda (sym)
      (eopl:error 'empty-env "No binding for: ~s" sym))))

(define extend-env
  (lambda (ids vals env)
    (lambda (sym)
      (let ((pos (rib-find-position sym ids)))
        (if (number? pos)
          (list-ref vals pos)
          (apply-env env sym))))))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (letrec
      ((rec-env
         (lambda (sym)
           (let ((pos (rib-find-position sym proc-names)))
             (if (number? pos)
               (closure
                 (list-ref idss pos)
                 (list-ref bodies pos)
                 rec-env)
               (apply-env old-env sym))))))
      rec-env)))

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

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
