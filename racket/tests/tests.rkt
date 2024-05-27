#lang racket

(require "perf-util.rkt"
         "four-color.rkt"
         "interp.rkt"
         "lists.rkt"
         "../mk.rkt")

(define batches '(1))

(define (write-cvs f data)
  (with-output-to-file f
    (λ ()
      (printf "Total,All together time,F(n) time,Product-inf(n) time\n")
      (for ((i (range 1 (+ 1 (length data))))
            (d data))
        (printf "~a," i)
        (for ((entry (add-between d ",")))
          (printf "~a" entry))
        (printf "\n")))
    #:exists 'replace))

(define (run-and-log name goal total)
  (let ((data (map (λ (total)
                     (run-tests name goal total batches))
                   (range 1 (+ total 1)))))
    (write-cvs (format "~a.cvs" name) data)))

(run-and-log 'appendo
             (λ (q) (fresh (r s)
                       (== q `(,r ,s))
                       (appendo r s (range 40))))
             40)

(run-and-log 'four-color-canada (do-canada/code) 20)
(run-and-log 'four-color-australia (do-australia/code) 20)
;; #;(run-and-log 'four-color-america (do-america/code) 5)

(run-and-log 'interp-quine quine 20)
(run-and-log 'interp-twine twine 20)
(run-and-log 'interp-thine thine 20)
