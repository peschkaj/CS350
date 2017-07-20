#lang racket

(define max-int-value 2147483647)
(define max-runtime-ms 1000)
(define large-list-size 100000)


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


;; Searches for the first instance of insertion sort that's faster than some reference time.
;;
;; A new list is generated each time where `current` elements at the
;; beginning of the list are sorted and then remaining elements
;; are random.
(define (find-fastest current list-size reference-value)
  (define the-list (take (append (range 1 (- list-size current))
                                 (randomlist list-size max-int-value))
                         list-size))
  (define st (current-inexact-milliseconds))
  (insertionsort the-list)
  (define sort-duration (- (current-inexact-milliseconds) st))

  (if (< sort-duration reference-value)
      (fprintf (current-output-port)
               "insertion sort with ~a elements is slower than merge sort's ~a ms to sort ~a\n"
               current
               reference-value
               large-list-size)
      (find-fastest (- current 1)
                    list-size
                    reference-value)))


;; But what we really need is a CSV that produces list size, merge sort ms, insertion sort ms
(define (time-both-sorts [results-list '()] [sorted-list-size large-list-size] [decrement 100])
  (define merge-sort-ms
    (let* ([random-list (take (append (range 1 sorted-list-size)
                                      (randomlist large-list-size max-int-value))
                              large-list-size)]
           [start-time (current-inexact-milliseconds)]
           [sorted-list (mergesort random-list)])
      (- (current-inexact-milliseconds) start-time)))
  (define insertion-sort-ms
    (let* ([random-list (take (append (range 1 sorted-list-size)
                                      (randomlist large-list-size max-int-value))
                              large-list-size)]
           [start-time (current-inexact-milliseconds)]
           [sorted-list (insertionsort random-list)])
      (- (current-inexact-milliseconds) start-time)))

  (if (< merge-sort-ms insertion-sort-ms)
      (cons (list sorted-list-size merge-sort-ms insertion-sort-ms) results-list)
      (time-both-sorts (cons (list sorted-list-size merge-sort-ms insertion-sort-ms) results-list)
                       (- sorted-list-size decrement)
                       decrement)))


(define (results-to-csv filename results)
  (with-output-to-file filename
    (lambda() (for-each (lambda (triple)
                          (fprintf (current-output-port)
                                   "~a\n"
                                   (string-append* (cdr (append*
                                                         (map
                                                          (lambda (x) (list "\t" (number->string x)))
                                                          triple))))))
                        results))
    #:exists 'truncate))

(define (sorted-range length)
  (range 1 length))

(fprintf (current-output-port) "Running time-both-sorts\n")

(define results (time-both-sorts '()
                                 100000
                                 1000))

(define rr (reverse results))

(fprintf (current-output-port) "Writing results to file... ")
(results-to-csv "hw2-c.csv" rr)
(fprintf (current-output-port) "done\n")
