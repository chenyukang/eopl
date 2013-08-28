(let ((time-stamp "Time-stamp: <2000-12-11 16:34:08 wand>"))
  (eopl:printf "2-3-2.scm ~a~%" (substring time-stamp 13 29)))

(define empty-env
  (lambda ()
    (lambda (sym) 
      (eopl:error 'apply-env "No binding for ~s" sym))))

(define extend-env
  (lambda (syms vals env)
    (lambda (sym)
      (let ((pos (list-find-position sym syms)))
        (if (number? pos)
          (list-ref vals pos)
          (apply-env env sym))))))

(define apply-env
  (lambda (env sym) 
    (env sym)))

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

