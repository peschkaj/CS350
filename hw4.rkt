#lang racket

(require "algos/shared.rkt" "algos/monotone.rkt" "algos/quickhull.rkt")

(define max-runtime-ms 1000)

(define (time-hull algorithm)
  (let* ([n 10])
    (time-hull-iter algorithm n '())))

(define (time-hull-iter algorithm n results-list)
  (define hull-time-ms
    (let* ([start-time (current-inexact-milliseconds)]
           [sorted-hull algorithm (random-list n)])
      )
    (collect-garbage)

    (if (> max-runtime-ms hull-time-ms)
        (cons (list n (symbol->string algorithm) hull-time-ms)))
  )
