(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")


;;remove-fisrt
;;procedure version
(define end-cont
  (lambda()
    (lambda (v)
      (begin
	(printf "procedure end-cont : ~a\n" v)
	v))))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define remove-first
  (lambda (lst)
    (remove-first/k lst (end-cont))))

(define remove-first/k
  (lambda(lst cont)
    (if (null? lst)
	(apply-cont cont lst)
	(apply-cont cont (cdr lst)))))

(remove-first '())
(remove-first '(1 2 3))
(remove-first '(1))

;;remove-fisrt, datatype version
(define-datatype continuation continuation?
  (end-cont))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
	   (end-cont ()
		     (begin
		       (printf "datatype end-cont: ~a\n" val)))
	   (else
	    (error "error-cond")))))

(define remove-first
  (lambda (lst)
    (remove-first/k lst (end-cont))))

(define remove-first/k
  (lambda (lst cont)
    (if (null? lst)
	(apply-cont cont lst)
	(apply-cont cont (cdr lst)))))

(remove-first '())
(remove-first '(1 2 3))
(remove-first '(1))


;;list-sum,
;;procedure version
(define end-cont
  (lambda()
    (lambda (v)
      (begin
	(printf "list-sum-procedure end-cont: ~a\n" v)
	v))))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define list-sum
  (lambda (lst)
    (list-sum/k 0 lst (end-cont))))

(define list-sum/k
  (lambda (sum lst cont)
    (if (null? lst)
	(apply-cont cont sum)
	(list-sum/k (+ sum (car lst))  (cdr lst)
		    cont))))

(list-sum '())
(list-sum '(1 2 3))
(list-sum '(1))


;;list-sum
;;datatype version
(define-datatype continuation continuation?
  (end-cont)
  (sum1-cont
   (saved-sum integer?)
   (cont continuation?)))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
	   (end-cont()
	    (begin
	      (printf "list-sum-datatype end-cont: ~a\n" val)
	      val))
	   (sum1-cont (saved-sum saved-cont)
		      (apply-cont saved-cont (+ saved-sum val))))))

(define list-sum
  (lambda (lst)
    (list-sum/k lst (end-cont))))

(define list-sum/k
  (lambda (lst saved-cont)
    (if (null? lst)
	(apply-cont saved-cont 0)
	(list-sum/k (cdr lst)
		    (sum1-cont (car lst) saved-cont)))))

(list-sum '(1))
(list-sum '(1 2 3))
(list-sum '())


;;subst
;;procedure version
(define subst-cur
  (lambda (new old lst)
    (if (list? lst)
        (if (null? lst)
            '()
            (cons (subst-cur new old (car lst))
                  (subst-cur new old (cdr lst))))
        (if (eq? lst old)
            new
            lst))))

(define end-cont
  (lambda()
    (lambda (v)
      (begin
	(printf "subst-procedure end-cont: ~a\n" v)
	v))))

(define subst-cont
  (lambda (new old cur saved-cont)
    (lambda (v)
      (if (list? cur)
	  (if (null? cur)
	      (apply-cont saved-cont '())
	      (subst/k new old cur saved-cont))
	  (if (eq? cur old)
	      (apply-cont saved-cont (cons new v))
	      (apply-cont saved-cont (cons cur v)))))))


(define apply-cont
  (lambda (cont val)
    (cont val)))

(define subst
  (lambda (new old lst)
    (subst/k new old lst (end-cont))))

(define subst/k
  (lambda (new old lst cont)
    (if (null? lst)
	(apply-cont cont '())
	(subst/k new old (cdr lst)
		 (subst-cont new old (car lst) cont)))))

(subst 'a 'b '(b a c '(b b c)))
