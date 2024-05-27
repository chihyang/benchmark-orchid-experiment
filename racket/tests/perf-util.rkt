#lang racket

(require "../mk.rkt")
(provide run-tests)

(define (time->seconds t)
  (/ t 1e3))

(define (timing-test thk)
  (let ((t1 (current-inexact-monotonic-milliseconds)))
    (let ((r (thk)))
      (let ((diff (- (current-inexact-monotonic-milliseconds) t1)))
        (cons r diff)))))

(define (incremental-test/f goal total batch)
  (let loop ((first-time? #t)
             (n total)
             (next-goal goal)
             (incremental-total-time 0.0))
    (cond
      ((<= n 0) incremental-total-time)
      (else
       (let ((next-goal (timing-test (lambda () (if first-time? (runf batch next-goal) (next-goal batch))))))
         (loop #f (- n batch)
               (cdr (car next-goal))
               (+ incremental-total-time (cdr next-goal))))))))

(define (incremental-test goal total batch)
  (let loop ((n total)
             (next-goal goal)
             (incremental-total-time 0.0))
    (cond
      ((<= n 0) incremental-total-time)
      (else
       (let ((next-goal (timing-test (lambda () (runc batch next-goal)))))
         (loop (- n batch)
               (cdr (car next-goal))
               (+ incremental-total-time (cdr next-goal))))))))

(define (run-tests name goal total batches)
  (eprintf "Running performance test for ~a\n" name)
  (define all-together-time 0)
  (define functional-time 0)
  (define incremental-time 0)

  (eprintf "Warming up...\n")
  (runc 1 goal)

  (let loop ((batches batches))
    (cond
      ((null? batches) #f)
      (else
       (eprintf "Get ~a results incrementally with a function, ~a each time:\t" total (car batches))
       (set! functional-time (time->seconds (incremental-test/f goal total (car batches))))
       (eprintf "~a s\n" functional-time)
       (loop (cdr batches)))))

  (let loop ((batches batches))
    (cond
      ((null? batches) #f)
      (else
       (eprintf "Get ~a results incrementally, ~a each time:\t" total (car batches))
       (set! incremental-time (time->seconds (incremental-test goal total (car batches))))
       (eprintf "~a s\n" incremental-time)
       (loop (cdr batches)))))
  (eprintf "")

  (set! all-together-time (time->seconds (cdr (timing-test (lambda () (runc total goal))))))
  (eprintf "Get ~a results all together:\t\t\t" total)
  (eprintf "~a s\n" all-together-time)
  (eprintf "")
  (list all-together-time functional-time incremental-time)
  )
