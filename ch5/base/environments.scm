  ;;;;;;;;;;;;;;;; environments ;;;;;;;;;;;;;;;;
;;; This should probably be factored out into a module called
;;; environments.scm, like it is in most of the other interpreters.

(define empty-env
  (lambda ()
    '()))

(define empty-env?
  (lambda (x) (null? x)))

(define extend-env
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define extend-env-rec*
  (lambda (p-names b-vars p-bodies saved-env)
    (cons
     (list p-names b-vars p-bodies)
     saved-env)))

(define apply-env
  (lambda (env search-sym)
    (if (null? env)
        (error 'apply-env "No binding for ~s" search-sym)
        (let* ((binding (car env))
               (saved-env (cdr env)))
          (if (symbol? (car binding))
              ;; ok, this is an extend-env
              (if (eqv? search-sym (car binding))
                  (cadr binding)
                  (apply-env saved-env search-sym))
              ;; no, this is an extend-env-rec
              (let ((pos (locate search-sym (car binding)))
                    (b-vars (cadr binding))
                    (p-bodies (caddr binding)))
                (if pos
                    (newref
                     (proc-val
                      (procedure
                       (list-ref b-vars pos)
                       (list-ref p-bodies pos)
                       env)))
                    (apply-env saved-env search-sym))))))))

;; returns position of sym in los, else #f
(define locate
  (lambda (sym los)
    (let loop ((pos 0) (los los))
      ;; los is at position pos of the original los
      (cond
       ((null? los) #f)
       ((eqv? sym (car los)) pos)
       (else (loop (+ pos 1) (cdr los)))))))

(define init-env
  (lambda ()
    (letrec
        ((make-init-env
          ;; entry ::= (id expval)
          (lambda (entries)
            (if (null? entries)
                (empty-env)
                (extend-env
                 (car (car entries))
                 (newref (cadr (car entries)))
                 (make-init-env (cdr entries)))))))
      (make-init-env
       (list
        (list 'i (num-val 1))
        (list 'v (num-val 5))
        (list 'x (num-val 10)))))))

;; not precise, but will do.
(define environment?
  (list-of
   (lambda (p)
     (and
      (pair? p)
      (or
       (symbol? (car p))
       ((list-of symbol?) (car p)))))))
