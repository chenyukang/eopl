(load "../libs/init.scm")

(define leaf (lambda (n) n))
(define leaf? number?)
(define interior-node list)
(define interior-node? list?)
(define left cadr)
(define right caddr)

(define number-leaves
  (lambda (bt)
    (car (number-leaves-from bt 0))))

(define number-leaves-from
  (lambda (bt n)
    (cond
      ((null? bt) (list bt n))
      ((interior-node? bt)
       (let* ([lt (number-leaves-from (left bt) n)]
              [rt (number-leaves-from (right bt) (cadr lt))])
         (list (interior-node (car bt) (car lt) (car rt)) (cadr rt))))
      ((leaf? bt) (list n (+ n 1))))))

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

