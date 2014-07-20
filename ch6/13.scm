(load-relative "../libs/init.scm")
(load-relative "./base/test.scm")
(load-relative "./base/cps.scm")
(load-relative "./base/data-structures.scm")
(load-relative "./base/cps-lang.scm")
(load-relative "./base/cps-cases.scm")

;; Translate each of these expressions in CPS-IN into continuation-passing
;; style using the CPS recipe on page 200 above. Test your transformed
;; programs by running them using the interpreter of figure 6.6.
;; Be sure that the original and transformed versions give the same answer
;; on each input.


;; finish this need to add some types into the interpreter, such as list,
;; cdr, null? number? etc, this will need too much work.

;; This exercise is one level? really?

;; Let me solve it in scheme as an example.

(define removeall
  (lambda (n lst)
    (if (null? lst)
	lst
	;; this is different with example
	(if (list? (car lst))
	    (cons (removeall n (car lst))
		  (removeall n (cdr lst)))
	    (if (equal? n (car lst))
		(removeall n (cdr lst))
		(cons (car lst)
		      (removeall n (cdr lst))))))))

;; give some testcases

(removeall 1 '(1))
;; ==> ()

(removeall 1 '(1 2 3))
;; ==> (2 3)

(removeall 3 '(1 2 2))
;; ==> (1 2 2)

(removeall 3 '(1 3 3))
;; ==> (1)


;; OK, write a tail form version

(define removeall-tail
  (lambda (n lst)
    (remove-iter n lst (end-cont))))

(define end-cont
  (lambda ()
    (lambda (val)
      (begin
	(print "end-cont return only one time")
	val))))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define cons1-cont
  (lambda (elm lst cont)
    (lambda (val)
      (remove-iter elm lst
		   (cons2-cont val cont)))))

(define cons2-cont
  (lambda (val2 cont)
    (lambda (val1)
      (apply-cont cont
		  (cons val1 val2)))))

(define append-cont
  (lambda (store-val cont)
    (lambda (val)
      (apply-cont cont
		  (append (list store-val)
			  val)))))
(define remove-iter
  (lambda (n lst cont)
    (if (null? lst)
        (apply-cont cont lst)
        (if (list? (car lst))
	    (remove-iter n (car lst)
			 ;; new cont
			 (cons1-cont n (cdr lst) cont))
	    (let ((now (car lst)))
	      (if (equal? n now)
		    (remove-iter n (cdr lst) cont)
		    (remove-iter n (cdr lst)
				 ;; new cont
				 (append-cont now cont))))))))


(removeall-tail 1 '(1))
;; ==> ()

(removeall-tail 1 '(1 2 3))
;; ==> (2 3)

(removeall-tail 3 '(1 2 2))
;; ==> (1 2 2)

(removeall-tail 3 '(1 3 3))
;; ==> (1)

(removeall-tail 0 '(1 2 3 4))
;; ==> (1 2 3 4)

;; Others left unsolved, :)
