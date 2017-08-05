#lang typed/racket
(provide monotone)
(require "shared.rkt")
(module+ test (require typed/rackunit))

(: monotone : (Listof Point) -> (Listof Point))
(define (monotone points )
  (define sorted (order points))
  (define lower (build-side sorted))
  (define upper (build-side (reverse sorted)))
  (append (rest lower) (rest upper)))

(: order : (Listof Point) -> (Listof Point))
(define (order points)
  (sort points
        (lambda ([x : Point] [y : Point]) 
          (if (= (real-part x) (real-part y))
              (< (imag-part x) (imag-part y))
              (< (real-part x) (real-part y))))))

(module+ test
  (check-equal? (order (list 2 3 1))
                (list 1 2 3))
  (check-equal? (order (list 2 3 3+2i 1))
                (list 1 2 3 3+2i)))

(: build-side : (Listof Point) -> (Listof Point))
(define (build-side points)
  (let loop ([pts : (Listof Point) points] [results : (Listof Point) null])
    (match pts
      [(list) results]
      [(cons x r)
       (list* x (if (null? results) null (list (first results))))
       (loop (rest pts)
             (cons x (cut x results)))])))

(: cut : Point (Listof Point) -> (Listof Point))
(define (cut orig points)
  (match points
    [(list* a b r)
     (if (counter-clockwise? orig a b)
         points
         (cut orig (cons b r)))]
    [_ points]))

(: counter-clockwise? : Point Point Point -> Boolean)
(define (counter-clockwise? o a b)
  ((cross (- a o) (- b o)) . > . 0))

(: cross : Point Point -> Real)
(define (cross a b)
  (- (* (real-part a) (imag-part b))
     (* (real-part b) (imag-part a))))

