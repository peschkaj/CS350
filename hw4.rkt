#lang racket

(require "algos/shared.rkt" "algos/monotone.rkt" "algos/quickhull.rkt")

(define max-runtime-ms 1000)

(define (time-hull n results-list)
  (define the-list (random-data n))
  (define quickhull-time-ms
    (let ([start-time (current-inexact-milliseconds)]
           [sorted-hull (quickhull the-list)])
      (- (current-inexact-milliseconds) start-time)))

  (collect-garbage)

  (define monotone-time-ms
    (let ([start-time (current-inexact-milliseconds)]
          [sorted-hull (monotone the-list)])
      (- (current-inexact-milliseconds) start-time)))

  (if (> max-runtime-ms (max quickhull-time-ms monotone-time-ms))
      (cons (list n quickhull-time-ms monotone-time-ms) results-list)
      (time-hull (+ n 1) (cons (list n quickhull-time-ms monotone-time-ms) results-list))))
