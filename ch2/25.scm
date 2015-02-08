(load "../libs/init.scm")

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define sum-tree
  (lambda (bt)
    (cases bintree bt
      (interior-node (key left right)
                     (let* ([sum-left (sum-tree left)]
                            [sum-right (sum-tree right)]
                            [cur-sum (+ (cdar sum-left)
                                        (cdar sum-right))])
                       (cond
                         ((and (null? (caar sum-left))
                               (null? (caar sum-right)))
                          (list (cons key cur-sum)))
                         ((null? (caar sum-left))
                          (list (cons key cur-sum) sum-right))
                         ((null? (caar sum-right))
                          (list (cons key cur-sum) sum-left))
                         (else
                          (list (cons key cur-sum) sum-left sum-right)))))
      (leaf-node (num) (list (cons '() num))))))


(define max-sum-tree
  (lambda (lst)
    (if (null? (cdr lst))
        (car lst)
        (let ([res (max-sum-tree (cdr lst))]
              [fst (car lst)])
          (if (> (cdr fst) (cdr res))
              fst
              res)))))

(define aux-max-interior
  (lambda (sum-bt)
    (let ([cur-val (cdar sum-bt)])
      (cond 
        ((null? (cdr sum-bt)) (car sum-bt))
        ((null? (cddr sum-bt))
         (max-sum-tree (list (car sum-bt) (aux-max-interior (cadr sum-bt)))))
        ((null? (cdddr sum-bt))
         (max-sum-tree (list (car sum-bt)
                             (aux-max-interior (cadr sum-bt))
                             (aux-max-interior (caddr sum-bt)))))))))

(define max-interior
  (lambda (bt)
    (car (aux-max-interior (sum-tree bt)))))

(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))

(equal?? (max-interior tree-2) 'foo)
(equal?? (max-interior tree-3) 'foo)
