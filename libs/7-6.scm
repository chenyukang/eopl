;;; Elementary logic programming system

(let ((time-stamp "Time-stamp: <2001-01-05 18:43:00 dfried>"))
  (eopl:printf "7-6.scm ~a~%"
    (substring time-stamp 13 29)))

;;;;;;;;;;;;;;;; top level ;;;;;;;;;;;;;;;;

(define run-all
  (lambda ()
    (begin
      (test)
      (newline)
      (test1)
      (newline)
      (test2))))

;;;;;;;;;;;;;;;; syntax ;;;;;;;;;;;;;;;;

(define-datatype term term?
  (var-term
    (id symbol?))
  (constant-term
    (datum constant?))
  (app-term
    (terms (list-of term?))))

;;;;;;;;;;;;;;;; substitutions ;;;;;;;;;;;;;;;;

;;; basic interface

;; represent substs as fcns id -> term

(define empty-subst
  (lambda ()
    (lambda (id) (var-term id))))

(define apply-subst
  (lambda (subst id)
    (subst id)))

(define unit-subst
  (lambda (id new-term)
    (lambda (id2)
      (if (eq? id2 id) new-term (var-term id2)))))

(define compose-substs
  (lambda (s1 s2)
    (lambda (id)
      (subst-in-term (apply-subst s1 id) s2))))

;;; next level

(define subst-in-term
  (lambda (t subst)
    (cases term t
      (var-term (id)
        (apply-subst subst id))
      (constant-term (datum)
        (constant-term datum))
      (app-term (ts)
        (app-term (subst-in-terms ts subst))))))

(define subst-in-terms
  (lambda (ts subst)
    (map (lambda (t) (subst-in-term t subst)) ts)))

;;; auxiliaries for unifier:

(define all-ids
  (lambda (t)
    (cases term t
      (var-term (id) (list id))
      (constant-term (datum) '())
      (app-term (ts) (all-ids-terms ts)))))

(define all-ids-terms
  (lambda (ts)
    (map-union all-ids ts)))

;;;;;;;;;;;;;;;; unification ;;;;;;;;;;;;;;;;

(define var-term?
  (lambda (t)
    (cases term t
      (var-term (id) #t)
      (else #f))))

;; the unifier

(define unify-term
  (lambda (t u)
    (cases term t
      (var-term (tid)
        (if (or (var-term? u) (not (memv tid (all-ids u))))
          (unit-subst tid u)
          #f))
      (else
        (cases term u
          (var-term (uid) (unify-term u t))
          (constant-term (udatum) 
            (cases term t
              (constant-term (tdatum)
                (if (equal? tdatum udatum) (empty-subst) #f))
              (else #f)))
          (app-term (us)
            (cases term t
              (app-term (ts) (unify-terms ts us))
              (else #f))))))))

(define unify-terms
  (lambda (ts us)
    (cond
      ((and (null? ts) (null? us)) (empty-subst))
      ((or (null? ts) (null? us)) #f)
      (else
        (let ((subst-car (unify-term (car ts) (car us))))
          (if (not subst-car)
            #f
            (let ((new-ts (subst-in-terms (cdr ts) subst-car))
                  (new-us (subst-in-terms (cdr us) subst-car)))
              (let ((subst-cdr (unify-terms new-ts new-us)))
                (if (not subst-cdr)
                  #f
                  (compose-substs subst-car subst-cdr))))))))))

;;;;;;;;;;;;;;;; auxiliaries for inference engine ;;;;;;;;;;;;;;;;

;; this calls variable, which calls gensym, which doesn't quite do the
; trick.

; (define instantiate
;   (lambda (ts)
;     (letrec
;       ((build-subst
;          (lambda (ids)
;            (cond
;              ((null? ids) (empty-subst))
;              (else
;                (compose-substs
;                  (build-subst (cdr ids))
;                  (unit-subst (car ids) (var-term (variable (car ids))))))))))
;       (subst-in-terms ts
;         (build-subst (all-ids-terms ts))))))


(define instantiate
  (letrec
    ((build-subst
       (lambda (ids binds)
         (cond
           ((null? ids) (empty-subst))
           (else
             (compose-substs
               (build-subst (cdr ids) (cdr binds))
               (unit-subst (car ids) (var-term (car binds)))))))))
    (lambda (ts)
      (let ((ids (all-ids-terms ts)))
        (let ((binds (map (lambda (i)
                            (fresh-id/instantiate ids (symbol->string i)))
                       ids)))
          (subst-in-terms ts (build-subst ids binds)))))))

(define fresh-id/instantiate
  (let ((used-syms '()))
    (lambda (ids s)
      (let ((syms (union used-syms ids)))
        (letrec
          ((loop (lambda (n)
                   (let ((sym (string->symbol
                                (string-append s
                                  (number->string n)))))
                     (if (memv sym syms)
                         (loop (+ n 1))
                         (begin
                           (set! used-syms (cons sym used-syms))
                           sym))))))
          (loop 0))))))


;;;;;;;;;;;;;;;; Inference engine ;;;;;;;;;;;;;;;;

(define solve-terms
  (lambda (goals)
    (match-terms goals (empty-subst)
      (lambda (subst failure-fk)
        (unparse-terms (subst-in-terms goals subst)))
      (lambda () #f))))

;;; This is the solution to an exercise
(define solve-terms  
  (lambda (goals)
    (match-terms goals (empty-subst)
      (lambda (subst failure-fk)
        (write (unparse-terms (subst-in-terms goals subst)))
        (newline)
        (failure-fk))
      (lambda () #f))))

(define match-terms
  (lambda (goals subst sk fk)
    (if (null? goals) 
      (sk subst fk)
      (match-term (car goals) subst 
        (lambda (new-subst new-fk)
          (match-terms (cdr goals) new-subst sk new-fk))
        fk))))
      
(define match-term
  (lambda (goal subst sk fk)
    (let loop ((rules the-rules))
      (if (null? rules)
        (fk)
        (match-term-against-rule goal (car rules) subst sk
          (lambda ()
            (loop (cdr rules))))))))

(define match-term-against-rule
  (lambda (goal rule subst sk fk)
    (let ((instantiated-rule (instantiate rule)))
      (let ((head (rule->head instantiated-rule))
            (subgoals (rule->subgoals instantiated-rule)))
        (let ((new-subst
                (unify-term head (subst-in-term goal subst))))
          (if (not new-subst)
            (fk)
            (match-terms subgoals
              (compose-substs subst new-subst)
              sk fk)))))))

;;;; -------------------- Junk below the line -------------------

(define rule->subgoals cdr)
(define rule->head car)
(define make-rule cons)

(define parse-rule
  (lambda (rule-exp)
    (let ((head-exp (car rule-exp))
          (tail-exp (cddr rule-exp)))
      (make-rule (parse-term head-exp) (map parse-term tail-exp)))))

(define unparse-rule
  (lambda (ts)
    (cons
      (unparse-term (rule->head ts))
      (cons '<--
        (map unparse-term (rule->subgoals ts))))))

(define test
  (lambda ()
    (let ((test-term
            (parse-term
              '("append" x y
                 ("cons" 1
                  ("cons" 2
                   ("cons" 3 
                    ("cons" 4 "empty"))))))))
      (solve-terms (list test-term)))))

(define test1
  (lambda ()
    (let ((test-term
            (parse-term
              '("append" x ("cons" 3 ("cons" 4 "empty"))
                  ("cons" 1 ("cons" 2 ("cons" 3 ("cons" 4 "empty"))))))))
      (solve-terms (list test-term)))))

(define test2
  (lambda ()
    (let ((test-term1
            (parse-term
              '("likes" "mary" x)))
          (test-term2
            (parse-term
              '("likes" "john" x))))
      (solve-terms (list test-term1 test-term2)))))

(define eopl:gensym
  (let ((n 0))
    (lambda (s)
      (set! n (+ n 1))
      (let ((s (if (string? s) s (symbol->string s))))
        (string->symbol
          (string-append s (number->string n)))))))

(define variable (lambda (x) (eopl:gensym "v")))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((memv (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(define map-union
  (lambda (f ls)
    (cond
      ((null? ls) '())
      (else (union (f (car ls)) (map-union f (cdr ls)))))))

(define constant=?
  (lambda (x y)
    (cond
      ((and (boolean? x) (boolean? y)) (eqv? x y))
      ((and (number? x) (number? y)) (= x y))
      ((and (string? x) (string? y)) (string=? x y))
      ((and (null? x) (null? y)) #t)
      (else #f))))

(define parse-term
  (lambda (exp)
    (cond
      ((symbol? exp) (var-term exp))
      ((list? exp) (app-term (map parse-term exp)))
      ((constant? exp) (constant-term exp))
      (else (eopl:error 'parse-term "Invalid term:~s" exp)))))

(define unparse-term
  (lambda (t)
    (cases term t
      (var-term (id) id)
      (constant-term (datum) datum)
      (app-term (ts) (unparse-terms ts)))))

(define unparse-terms
  (lambda (ts)
    (map unparse-term ts)))

(define constant?
  (lambda (x)
    (or (boolean? x) (number? x) (string? x) (null? x))))

(define the-rules
  (let ((base-append-rule
          (parse-rule
            '(("append" "empty" x x) <--)))
        (append-rule
          (parse-rule
            '(("append" ("cons" w x) y ("cons" w z))
              <--
              ("append" x y z))))
        (likes-rule-1
          (parse-rule
            '(("likes" "mary" "food") <--)))
        (likes-rule-2
          (parse-rule
            '(("likes" "mary" "wine") <--)))
        (likes-rule-3
          (parse-rule
            '(("likes" "john" "wine") <--)))
        (likes-rule-4
          (parse-rule
            '(("likes" "john" "mary") <--))))
    (append
      (list base-append-rule append-rule)
      (list likes-rule-1 likes-rule-2 likes-rule-3 likes-rule-4))))

;;;;;

;;;;;
