#lang racket

(require "algos/shared.rkt" "algos/monotone.rkt" "algos/quickhull.rkt")

(define max-runtime-ms 250)

(define (time-hull n results-list)


  (define the-list (random-data n))

  (define quickhull-time-ms
    (let ([start-time (current-milliseconds)]
           [sorted-hull (quickhull data)])
      (- (current-inexact-milliseconds) start-time)))

  (define monotone-time-ms
    (let ([start-time (current-milliseconds)]
          [sorted-hull (monotone data)])
      (- (current-milliseconds) start-time)))

  (if (eq? (modulo n 100) 0)
      (fprintf (current-output-port)
               "iteration ~a, max time is ~a \n"
               n
               (max quickhull-time-ms monotone-time-ms))
      (collect-garbage))

  (if (> (max quickhull-time-ms monotone-time-ms)  max-runtime-ms)
      (cons (list n quickhull-time-ms monotone-time-ms) results-list)
      (time-hull (+ n 10) (cons (list n quickhull-time-ms monotone-time-ms) results-list))))


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


(module* main #f
  (let* ([results (time-hull 20 '())])
    (results-to-csv "hull.csv" results)
    ))
