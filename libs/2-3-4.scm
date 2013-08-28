(let ((time-stamp "Time-stamp: <2001-02-24 11:31:02 dfried>"))
  (eopl:printf "2-3-4.scm ~a~%" (substring time-stamp 13 29)))

(define empty-env
  (lambda ()
    '()))

(define extend-env
  (lambda (syms vals env)
    (cons (list syms vals) env)))

(define apply-env
  (lambda (env sym)
    (if (null? env)
      (eopl:error 'apply-env "No binding for ~s" sym)
      (let ((syms (car (car env)))
            (vals (cadr (car env)))
            (env (cdr env)))
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
            (list-ref vals pos)
            (apply-env env sym)))))))

(define rib-find-position list-find-position)

;;; The definitions below are snippetized (ignore if not author)

(define extend-env
  (lambda (syms vals env)
    (cons (cons syms (list->vector vals)) env)))

(define apply-env
  (lambda (env sym)
    (if (null? env)
      (eopl:error 'apply-env "No binding for ~s" sym)
      (let ((syms (car (car env)))
            (vals (cdr (car env)));\new1
            (env (cdr env)))
        (let ((pos (rib-find-position sym syms)))
          (if (number? pos)
            (vector-ref vals pos);\new1
            (apply-env env sym)))))))

(define extend-env
  (lambda (syms vals env)
    (cons (list->vector vals) env)))

(define apply-env-lexical
  (lambda (env depth position)
    (if (null? env)
      (eopl:error 'apply-env-lexical
        "No binding for depth = ~s position = ~s"
        depth position)
      (if (zero? depth)
        (vector-ref (car env) position)
        (apply-env-lexical (cdr env) (- depth 1) position)))))

