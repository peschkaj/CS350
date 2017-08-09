#lang racket

(require "algos/shared.rkt" "algos/monotone.rkt" "algos/quickhull.rkt")

(define max-runtime-ms 250)
(define max-iterations 100000)
(define radius 20)
(define incr 100)
(define mod-print (* incr 10))

(define (time-circle-hull n)
  (time-hull n '() (curry points-on-a-circle radius)))

(define (time-random-hull n)
  (time-hull n '() random-data)
  )

(define (time-hull n results-list generate-list)
  (collect-garbage)

  (define the-list (generate-list n))
  (define quickhull-time-ms
    (let ([start-time (current-milliseconds)]
          [sorted-hull (quickhull the-list)])
      (- (current-inexact-milliseconds) start-time)))

  (define monotone-time-ms
    (let ([start-time (current-milliseconds)]
          [sorted-hull (monotone the-list)])
      (- (current-milliseconds) start-time)))

  ;(if (eq? (modulo n mod-print) 0)
  (fprintf (current-output-port)
           "iteration ~a, max time is ~a \n"
           n
           (max quickhull-time-ms monotone-time-ms))
      ;; (collect-garbage))
   ;   )

  (if (or (> (max quickhull-time-ms monotone-time-ms)  max-runtime-ms)
          (> n max-iterations))
      (cons (list n quickhull-time-ms monotone-time-ms) results-list)
      (time-hull (+ n incr)
                 (cons (list n quickhull-time-ms monotone-time-ms)
                       results-list)
                 random-data)))


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
  ;; (let* ([results (time-random-hull 100)])
  ;;   (results-to-csv "random-hull.csv" results))
  (fprintf (current-output-port) "CIRCLES\n")
  (let* ([results (time-circle-hull 100)])
    (results-to-csv "circle-hull.csv" results))
  )
