;;; Behavioral probe for the documented thread-suspend!/thread-resume! pair.
;;; A worker increments a counter. Suspension must stop progress until resume.

(define lock (make-mutex))
(define count 0)

(define (counter-get)
  (mutex-lock! lock)
  (let ((x count))
    (mutex-unlock! lock)
    x))

(define (counter-inc!)
  (mutex-lock! lock)
  (set! count (+ count 1))
  (mutex-unlock! lock))

(define worker
  (make-thread
   (lambda ()
     (let loop ()
       (counter-inc!)
       (thread-yield!)
       (loop)))))

(thread-start! worker)

(let wait-start ((n 5000))
  (cond ((>= (counter-get) 100) #t)
        ((= n 0)
         (display "FAIL worker did not start\n")
         (exit 1))
        (else
         (thread-sleep! .001)
         (wait-start (- n 1)))))

(thread-suspend! worker)
(thread-sleep! .02)
(define suspended-start (counter-get))
(thread-sleep! .05)
(define suspended-end (counter-get))

(if (> suspended-end (+ suspended-start 2))
    (begin
      (display "FAIL suspend did not stop worker: ")
      (write (list suspended-start suspended-end))
      (newline)
      (exit 2)))

(thread-resume! worker)

(let wait-resume ((n 5000))
  (cond ((>= (counter-get) (+ suspended-end 20)) #t)
        ((= n 0)
         (display "FAIL resume did not restart worker\n")
         (exit 3))
        (else
         (thread-sleep! .001)
         (wait-resume (- n 1)))))

(display "THREAD_SUSPEND_RESUME|PASS|")
(write (list suspended-start suspended-end (counter-get)))
(newline)
(exit 0)
