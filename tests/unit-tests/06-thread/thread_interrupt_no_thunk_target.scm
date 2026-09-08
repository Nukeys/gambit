;;; Verify that thread-interrupt! without an explicit thunk runs the
;;; interrupted thread's user interrupt handler, both for the current
;;; thread and for a distinct target thread.

(define (wait-until pred)
  (let loop ((n 5000))
    (cond ((pred) #t)
          ((= n 0) #f)
          (else
           (thread-sleep! .001)
           (loop (- n 1))))))

(define current-hit? #f)

(current-user-interrupt-handler
 (lambda ()
   (set! current-hit? #t)))

(thread-interrupt! (current-thread))

(if (not current-hit?)
    (begin
      (display "FAIL current handler not invoked\n")
      (exit 1)))

(define worker
  (make-thread
   (lambda ()
     (current-user-interrupt-handler
      (lambda ()
        (thread-specific-set! (current-thread) 'worker-hit)))
     (thread-specific-set! (current-thread) 'worker-ready)
     (let loop ()
       (thread-sleep! .05)
       (loop)))))

(thread-start! worker)

(if (not (wait-until (lambda () (eq? (thread-specific worker) 'worker-ready))))
    (begin
      (display "FAIL worker did not become ready\n")
      (exit 1)))

(set! current-hit? #f)
(thread-interrupt! worker)

(if (not (wait-until (lambda () (eq? (thread-specific worker) 'worker-hit))))
    (begin
      (display "FAIL worker handler not invoked\n")
      (write (list 'worker-specific (thread-specific worker)
                   'main-handler-hit current-hit?))
      (newline)
      (exit 1)))

(if current-hit?
    (begin
      (display "FAIL no-thunk interrupt ran main handler instead of target handler\n")
      (exit 1)))

(display "THREAD_INTERRUPT_NO_THUNK|PASS\n")
(exit 0)
