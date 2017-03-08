(load "../libs/init.scm")

(define subst-in-s-exp
  (lambda (new old sexp)
          (if (symbol? sexp)
            (if (eqv? sexp old) new sexp)
            (subst new old sexp))))

(define subst
  (lambda (new old slist)
          (if (null? slist)
            '()
            (map (lambda (item) (subst-in-s-exp new old item)) slist))))

(equal?? (subst 'a 'b '(a b c d e)) '(a a c d e))
(equal?? (subst 'a 'b '(b)) '(a))
(equal?? (subst 'a 'b '(b b b)) '(a a a))
(equal?? (subst 's 'a '((a b) c d s)) '((s b) c d s))
