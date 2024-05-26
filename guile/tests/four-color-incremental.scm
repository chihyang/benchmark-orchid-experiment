(use-modules (minikanren language)
             (minikanren examples four-color))
(use-modules ((srfi srfi-19)
              #:renamer (symbol-prefix-proc 'sfri-19:)))

(define (time->seconds t)
  (+ (sfri-19:time-second t)
     (/ (sfri-19:time-nanosecond t) 1e9)))

(define (timing-test thk)
  (let ((t1 (sfri-19:current-time)))
    (let ((r (thk)))
      (let ((diff (sfri-19:time-difference (sfri-19:current-time) t1)))
        (cons r diff)))))

(define (incremental-test goal total batch)
  (let loop ((n total)
             (next-goal goal)
             (incremental-total-time (sfri-19:make-time sfri-19:time-duration 0 0)))
    (cond
     ((<= n 0) incremental-total-time)
     (else
      (let ((next-goal (timing-test (lambda () (runc batch next-goal)))))
        (loop (- n batch)
              (cdr (car next-goal))
              (sfri-19:add-duration incremental-total-time (cdr next-goal))))))))

(let ((goal (do-america/code))
      (total 1000)
      (batches '(1000 500 200 100 50 20 1)))
  (display "Warming up...")
  (run^ total goal)

  (let loop ((batches batches))
    (cond
     ((null? batches) #f)
     (else
      (display (format #f "Get ~a results incrementally, ~a each time:\t" total (car batches)))
      (format #t "~as" (time->seconds (incremental-test goal total (car batches))))
      (newline)
      (loop (cdr batches)))))

  (display (format #f "Get ~a results all together:\t\t\t" total))
  (format #t "~as" (time->seconds (cdr (timing-test (lambda () (run^ total goal))))))
  (newline))
