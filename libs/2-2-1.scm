(let ((time-stamp "Time-stamp: <2000-12-11 16:29:55 wand>"))
  (eopl:printf "2-2-1.scm ~a~%" (substring time-stamp 13 29)))

(define-datatype bintree bintree? 
  (leaf-node 
    (datum number?))
  (interior-node
    (key symbol?) 
    (left bintree?)
    (right bintree?)))

(define-datatype s-list s-list? 
  (empty-s-list)
  (non-empty-s-list 
    (first symbol-exp?)
    (rest s-list?)))

(define-datatype symbol-exp symbol-exp? 
  (symbol-symbol-exp
    (data symbol?))
  (s-list-symbol-exp
    (data s-list?)))

;;; This definition of s-list is not snippetized (ignore if not author)

(define-datatype s-list s-list? 
  (an-s-list
    (data (list-of symbol-exp?))))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(define leaf-sum 
  (lambda (tree)
    (cases bintree tree
      (leaf-node (datum) datum)
      (interior-node (key left right)
        (+ (leaf-sum left) (leaf-sum right))))))

