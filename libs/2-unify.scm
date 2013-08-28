(let ((time-stamp "Time-stamp: <2000-12-11 17:01:55 wand>"))
  (eopl:printf
    "2-unify.scm: terms, substitutions, unification ~a~%"
    (substring time-stamp 13 29)))

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

;;;; -------------------- Junk below the line -------------------

(define variable (lambda (x) (gensym)))

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


