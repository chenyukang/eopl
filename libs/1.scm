(let ((time-stamp "Time-stamp: <2001-05-10 05:30:06 dfried>"))
  (eopl:printf "1.scm: code for chapter 1 ~a~%" (substring time-stamp 13 29)))

(define count-nodes
  (lambda (s)
    (if (number? s)
      1
      (+ (count-nodes (cadr s))
         (count-nodes (caddr s))
         1))))

(define list-of-numbers?
  (lambda (lst)
    (if (null? lst)
      #t 
      (and ;\new3
        (number? (car lst))
        (list-of-numbers? (cdr lst))))))

(define nth-elt
  (lambda (lst n)
    (if (null? lst)
      (eopl:error 'nth-elt 
        "List too short by ~s elements.~%" (+ n 1))
      (if (zero? n)
        (car lst)
        (nth-elt (cdr lst) (- n 1))))))

(define remove
  (lambda (s los)
    (if (null? los)
      '()
      (if (eqv? (car los) s)
        (remove s (cdr los)) ;\new1
        (cons (car los) (remove s (cdr los)))))))

(define subst
  (lambda (new old slist)
    (if (null? slist)
      '()
      (cons ;\new3
        (subst-in-symbol-expression new old (car slist)) 
        (subst new old (cdr slist))))))          

(define subst-in-symbol-expression
  (lambda (new old se)
    (if (symbol? se) ;\new3
      (if (eqv? se old) new se)
      (subst new old se))))

(define notate-depth 
  (lambda (slist)
    (notate-depth-in-s-list slist 0)))

(define notate-depth-in-s-list
  (lambda (slist d)                     
    (if (null? slist) ;\new5
      '()
      (cons
        (notate-depth-in-symbol-expression (car slist) d)  
        (notate-depth-in-s-list (cdr slist) d)))))

(define notate-depth-in-symbol-expression
  (lambda (se d)
    (if (symbol? se)
      (list se d)
      (notate-depth-in-s-list se (+ d 1)))))

(define list-sum
  (lambda (lon)
    (if (null? lon)
      0
      (+ (car lon) 
         (list-sum (cdr lon))))))

(define partial-vector-sum
  (lambda (von n)
    (if (zero? n)
      0
      (+ (vector-ref von (- n 1))
         (partial-vector-sum von (- n 1))))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This one will be in the text.                      ;
;;; (define vector-sum                                 ;
;;;  (lambda (von)                                     ;
;;;    (partial-vector-sum von (vector-length von))))  ;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define vector-sum
  (lambda (von)
    (letrec
      ((partial-sum 
        (lambda (n)
          (if (zero? n)
            0
            (+ (vector-ref von (- n 1))
               (partial-sum (- n 1)))))))
      (partial-sum (vector-length von)))))

(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda) 
       (and (not (eqv? (caadr exp) var))
            (occurs-free? var (caddr exp))))
      (else (or (occurs-free? var  (car exp))
                (occurs-free? var (cadr exp)))))))

(define occurs-bound?
  (lambda (var exp)
    (cond
      ((symbol? exp) #f)
      ((eqv? (car exp) 'lambda)
       (or (occurs-bound? var (caddr exp))
           (and (eqv? (caadr exp) var)
                (occurs-free? var (caddr exp)))))
      (else (or (occurs-bound? var  (car exp))
                (occurs-bound? var (cadr exp)))))))

