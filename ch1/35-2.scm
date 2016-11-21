(load "../libs/init.scm")
(load "31.scm")

(define number-leaves
    (lambda (btree)
      (let ([n -1])
        (define replace-leaf-with
          (lambda (btree)
            (cond ((leaf? btree)
                   (set! n (+ n 1))
                   n)
                  (else
                   (interior-node (contents-of btree)
                                  (replace-leaf-with (lson btree))
                                  (replace-leaf-with (rson btree)))))))
        (replace-leaf-with btree))))

(equal?? (number-leaves
         (interior-node 'foo
                        (interior-node 'bar
                                       (leaf 26)
                                       (leaf 12))
                        (interior-node 'baz
                                       (leaf 11)
                                       (interior-node 'quux
                                                      (leaf 117)
                                                      (leaf 14)))))
        '(foo
          (bar 0 1)
          (baz 2 (quux 3 4))))