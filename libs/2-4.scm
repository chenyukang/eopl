(let ((time-stamp "Time-stamp: <2001-05-09 19:28:56 dfried>"))
  (eopl:printf "2-4.scm ~a~%" (substring time-stamp 13 29)))

(define create-queue
  (lambda ()
    (let ((q-in '())
          (q-out '()))
      (letrec 
        ((reset-queue
           (lambda ()
             (set! q-in '())
             (set! q-out '())))
         (empty-queue?
           (lambda ()
             (and (null? q-in)
                  (null? q-out))))
         (enqueue
           (lambda (x)
             (set! q-in (cons x q-in))))
         (dequeue
           (lambda ()
             (if (empty-queue?)
               (eopl:error 'dequeue
                 "Not on an empty queue")
               (begin
                 (if (null? q-out)
                   (begin
                     (set! q-out (reverse q-in))
                     (set! q-in '())))
                   (let ((ans (car q-out)))
                     (set! q-out (cdr q-out))
                     ans))))))
        (vector reset-queue empty-queue? enqueue dequeue)))))

(define queue-get-reset-operation   
  (lambda (q) (vector-ref q 0)))
(define queue-get-empty?-operation  
  (lambda (q) (vector-ref q 1)))
(define queue-get-enqueue-operation 
  (lambda (q) (vector-ref q 2)))
(define queue-get-dequeue-operation 
  (lambda (q) (vector-ref q 3)))








