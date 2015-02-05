(load "../libs/init.scm")

(define path
  (lambda (n bst)
    (cond
     ((null? bst) '())
     ((< n (car bst)) (cons 'left (path n (cadr bst))))
     ((> n (car bst)) (cons 'right (path n (caddr bst))))
     ((= n (car bst)) '()))))

(equal?? (path 17 '(14 (7 () (12 () ()))
                       (26 (20 (17 () ()) ())
                           (31 () ()))))
         '(right left left))
