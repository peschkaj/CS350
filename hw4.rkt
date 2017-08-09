#lang racket

(require "algos/shared.rkt" "algos/monotone.rkt" "algos/quickhull.rkt")

(define max-runtime-ms 250)
(define max-iterations 100000)
(define radius 50)
(define incr 1000)
(define mod-print (* incr 10))

;; Times the execution of both monotone chain and quickhull starting with n
;; items present in the data set and all points in the set are members of
;; the hull.
(define (time-circle-hull n)
  (time-hull n '() (curry points-on-a-circle radius)))

;; Times the execution of both monotone chain and quickhull starting with n
;; items present in the data set and points in the set are randomly
;; distributed
(define (time-random-hull n)
  (time-hull n '() random-data)  )

;; Times the execution of each hull mechanism.
;;
;; Returns a list of (set-elements quickhull-duration montone-duration)
(define (time-hull n results-list generate-list)
  (collect-garbage)

  ;; Both timings use the same list
  (define the-list (generate-list n))

  (define quickhull-time-ms
    (let ([start-time (current-milliseconds)]
          [sorted-hull (quickhull the-list)])
      (- (current-inexact-milliseconds) start-time)))

  (collect-garbage)

  (define monotone-time-ms
    (let ([start-time (current-milliseconds)]
          [sorted-hull (monotone the-list)])
      (- (current-milliseconds) start-time)))

  (fprintf (current-output-port)
           "iteration ~a, max time is ~a \n"
           n
           (max quickhull-time-ms monotone-time-ms))

  (if (or (> (max quickhull-time-ms monotone-time-ms)  max-runtime-ms)
          (> n max-iterations))
      (cons (list n quickhull-time-ms monotone-time-ms) results-list)
      (time-hull (+ n incr)
                 (cons (list n quickhull-time-ms monotone-time-ms)
                       results-list)
                 random-data)))

;; Convert a list of triples into a CSV file
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
  (let* ([results (time-random-hull 100)])
    (results-to-csv "random-hull.csv" results))

  (fprintf (current-output-port) "CIRCLES\n")

  (let* ([results (time-circle-hull 100)])
    (results-to-csv "circle-hull.csv" results))
  )
