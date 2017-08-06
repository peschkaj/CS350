#lang racket

(require "algos/shared.rkt" "algos/monotone.rkt" "algos/quickhull.rkt")

(define max-hull-size 500)

(define (time-hull n stride results-list)
  (define the-list (random-data n))
  (define quickhull-time-ms
    (let ([start-time (current-milliseconds)]
           [sorted-hull (quickhull the-list)])
      (- (current-inexact-milliseconds) start-time)))

  (collect-garbage)

  (define monotone-time-ms
    (let ([start-time (current-milliseconds)]
          [sorted-hull (monotone the-list)])
      (- (current-milliseconds) start-time)))

  (collect-garbage)

  (if (> (max quickhull-time-ms monotone-time-ms)  max-runtime-ms)
      (cons (list n quickhull-time-ms monotone-time-ms) results-list)
      (time-hull (+ n 1) (cons (list n quickhull-time-ms monotone-time-ms) results-list))))


(define (results-to-csv filename results)
  (with-output-to-file filename
    (lambda() (for-each (lambda (triple)
                          (fprintf (current-output-port)
                                   "~a\n"
                                   (string-append* (cdr (append*
                                                         (map
                                                          (lambda (x) (list ", " (number->string x)))
                                                          triple))))))
                        (reverse results)))
    #:exists 'truncate))


;; (module* main #f
;;   (let* ([results (time-hull 20 '())])
;;     (results-to-csv "hull.csv" results)
;;     ))

(module* main #f
  (for/async ([i (in-range 100000)])
             (fprintf (current-output-port) "~a\n" i)
             ))
