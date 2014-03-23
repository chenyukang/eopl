;;; defining eopl fcns in R5RS scheme

(define debug? (make-parameter #f))

(define eopl:printf
  (lambda (format . args)
    (let ((len (string-length format)))
      (let loop ((i 0) (args args))
        (let ((output
                (lambda (fn)
                  (fn (car args))
                  (loop  (+ i 2) (cdr args))))
              (outputc
                (lambda (fn)
                  (fn)
                  (loop (+ i 2) args))))
          (if (>= i len) #t
            (let ((c (string-ref format i)))
              (if (char=? c #\~)
                (case (string-ref format (+ i 1))
                  ((#\s) (output write))
                  ((#\a) (output display))
                  ((#\c) (output write-char))
                  ((#\% #\n) (outputc newline))
                  ((#\~) (outputc (lambda () (write-char #\~))))
                  (else
                    (write
                      "error in eopl:printf: unknown format character ")
                    (write-char  (string-ref format (+ i 1)))
                    (write-char #\newline)
                    (eopl:error-stop)))
                (begin
                  (display c)
                  (loop (+ i 1) args))))))))))


(define pretty-print
  (lambda (x)
    (if (debug?)
	(begin
	  (write x)
	  (newline)))))

(define eopl:pretty-print pretty-print)

(define sllgen:pretty-print          eopl:pretty-print)
(define define-datatype:pretty-print eopl:pretty-print)

(define eopl:error
  (lambda (who format . data)
    ;; print the message
    (if (debug?)
	(begin
	  (eopl:printf "Error reported by ~s:~%" who)
	  (apply eopl:printf (cons format data))
	  (newline)
	  (eopl:error-stop))
	(eopl:error-stop))))

(define eopl:error-stop "eopl:error-stop is purposely undefined")

(define error eopl:error)

(define-syntax equal??
  (syntax-rules ()
    ((_ test-exp correct-ans)
     (let ((observed-ans test-exp))
       (if (not (equal? observed-ans correct-ans))
           (printf "~s returned ~s, should have returned ~s~%"
                   'test-exp
                   observed-ans
                   correct-ans))))))
