#lang racket

(define max-int-value 2147483647)
(define max-runtime-ms 1000)
(define large-list-size 25000)


;; Generate a random list
;;  n:  list is length n
;;  mx: maximum value to generate
(define (randomlist n mx)
  (cond
    [(= n 0) empty]
    [else
     (cons (+ 1 (random mx))
           (randomlist (- n 1) mx))]))

;; Insertion Sort implementation
;; from https://gist.github.com/miyukino/5652107
(define (insert L M)
  (if (null? L) M
      (if (null? M) L
          (if (< (car L) (car M))
              (cons (car L) (insert (cdr L) M))
              (cons (car M) (insert (cdr M) L))))))

;; another insert function
;; need to modify the first para in insertionsort function to (car L)
(define (insert2 x L)
  (if (null? L) (list x)
      (let ((y (car L)) (M (cdr L)))
        (if (< x y)
            (cons x L)
            (cons y (insert2 x M))))))

;; Exp. (insertionsort '(4 2 10 3 -1 5)) ==> (-1 2 3 4 5 10)
(define (insertionsort L)
  (if (null? L) '()
      (insert (list (car L)) (insertionsort (cdr L)))))


;; Merge Sort implementation
;; From https://gist.github.com/miyukino/5652105
;; Exp. (merge '(1 3 5 7 8 9 10) '(2 4 6)) ==> (1 2 3 4 5 6 7 8 9 10)
(define (merge L M)
  (if (null? L) M
      (if (null? M) L
          (if (< (car L) (car M))
              (cons (car L) (merge (cdr L) M))
              (cons (car M) (merge (cdr M) L))))))

;; split helper functions
(define (odd L)
  (if (null? L) '()
      (if (null? (cdr L)) (list (car L))
          (cons (car L) (odd (cddr L))))))
(define (even L)
  (if (null? L) '()
      (if (null? (cdr L)) '()
          (cons (cadr L) (even (cddr L))))))

;; Exp. (split '(a b c d e f g h i)) ==> ((a c e g i)(b d f h))
(define (split L)
  (cons (odd L) (cons (even L) `())))


;; Exp. (mergesort '(8 1 3 9 6 5 7 2 4 10)) ==> (1 2 3 4 5 6 7 8 9 10)
(define (mergesort L)
  (if (null? L) L
      (if (null? (cdr L)) L
          (merge
           (mergesort (car (split L)))
           (mergesort (cadr (split L)))))))


;; But what we really need is a CSV that produces list size, merge sort ms, insertion sort ms
(define (time-both-sorts [max-duration-ms max-runtime-ms] [results-list '()] [list-builder randomlist] [direction +] [current-size 100] [increment 100])
  (define merge-sort-ms
    (let* ([random-list (list-builder current-size max-int-value)]
           [start-time (current-inexact-milliseconds)]
           [sorted-list (mergesort random-list)])
      (- (current-inexact-milliseconds) start-time)))
  (define insertion-sort-ms
    (let* ([random-list (list-builder current-size max-int-value)]
           [start-time (current-inexact-milliseconds)]
           [sorted-list (insertionsort random-list)])
      (- (current-inexact-milliseconds) start-time)))

  ;; prevents GC from occurring during timing operations
  (collect-garbage)

  (if (> (max merge-sort-ms insertion-sort-ms) max-duration-ms)
      (cons (list current-size merge-sort-ms insertion-sort-ms) results-list)
      (time-both-sorts max-duration-ms
                       (cons (list current-size merge-sort-ms insertion-sort-ms) results-list)
                       list-builder
                       direction
                       (direction current-size increment)
                       increment)))


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


(fprintf (current-output-port) "Running time-both-sorts\n")

(define results (time-both-sorts max-runtime-ms))

(fprintf (current-output-port) "Writing results to file... ")
(results-to-csv "lol.csv" results)
(fprintf (current-output-port) "done\n")
