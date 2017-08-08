#lang typed/racket

; Shamelessly borrowed from https://github.com/florence/convex-hulls

(provide Point Huller FrameDrawer
         random-data random-point
         .x .y)
(require math/distributions)
(define-type Point Complex)
(define-type Huller
  (->* ((Listof Point))
       (Listof Point)))
(define-type FrameDrawer
  ((Sequenceof Point) (Sequenceof Point) (Sequenceof Point) * -> Void))

(: random-data : (->* () (Natural) (Listof Point)))
(define (random-data [n 100]) 
  (for*/fold ([r : (Listof Point) (list (random-point))])
             ([_ n]
              [p (in-value (random-point))]
              #:unless (ormap (lambda ([r : Point])
                                ((magnitude (- r p)) . < . 5))
                              r))
    (cons p r)))

(define BOUND 100)
(define d (normal-dist (/ BOUND 2) 25))
(: random-point : (-> Point))
(define (random-point)
  (define xs (sample d))
  (define ys (sample d))
  (make-rectangular xs ys))

(define .x real-part)
(define .y imag-part)

(define (points-on-a-circle radius n)
  (let ([incr (/ pi n)])
    (for/list ([i (in-range (+ n))])
      ; (x0 + r cos theta, y0 + r sin theta)
      (cons (* radius (cos (* incr i)))
            (* radius (sin (* incr i)))))))
