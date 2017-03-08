(define apply1
  (lambda (stack v)
    v))
(define apply2
  (lambda (stack v)
    stack))

(define empty-stack
  (lambda ()
    (lambda (f) '())))

;; Apply ::= apply1 | apply2
;; EmptyStack ::= Apply -> Nil
;; Stack ::= EmptyStack | apply2 -> Stack

;;stack-push: Stack x Val -> Stack
(define stack-push
  (lambda (stack val)
    (lambda (apply1or2)
      (apply1or2 stack val))))

;; stack-pop: Stack -> Stack
(define stack-pop
  (lambda (stack)
    (stack apply2)))

;; stack-top Stack -> Val
(define stack-top
  (lambda (stack)
    (stack apply1)))

;; test case
(define zero (empty-stack))
(define one (stack-push zero 1))
(define two (stack-push one 2))
(stack-top one) ;; 1 
(stack-top two) ;; 2
(stack-top (stack-pop two)) ;; 1
(stack-top (stack-push (stack-push (stack-push 1 (empty-stack)) 2) 3)) ;; 3
