(define the-ready-queue   'uninitialized)
(define the-final-answer  'uninitialized)

(define the-max-time-slice    'uninitialized)
(define the-time-remaining    'uninitialized)

;; initialize-scheduler! : Int -> Unspecified
(define initialize-scheduler!
  (lambda (ticks)
    (set! the-ready-queue (empty-queue))
    (set! the-final-answer 'uninitialized)
    (set! the-max-time-slice ticks)
    (set! the-time-remaining the-max-time-slice)
    ))

  ;;;;;;;;;;;;;;;; the final answer ;;;;;;;;;;;;;;;;

;; place-on-ready-queue! : Thread -> Unspecified
(define place-on-ready-queue!
  (lambda (th)
    (set! the-ready-queue
	  (enqueue the-ready-queue th))))

;; run-next-thread : () -> FinalAnswer
(define run-next-thread
  (lambda ()
    (if (empty? the-ready-queue)
        the-final-answer
        (dequeue the-ready-queue
		 (lambda (first-ready-thread other-ready-threads)
		   (set! the-ready-queue other-ready-threads)
		   (set! the-time-remaining the-max-time-slice)
		   (first-ready-thread)
		   )))))

;; set-final-answer! : ExpVal -> Unspecified
(define set-final-answer!
  (lambda (val)
    (set! the-final-answer val)))

;; time-expired? : () -> Bool
(define time-expired?
  (lambda ()
    (zero? the-time-remaining)))

;; decrement-timer! : () -> Unspecified
(define decrement-timer!
  (lambda ()
    (set! the-time-remaining (- the-time-remaining 1))))
