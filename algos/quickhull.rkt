#lang typed/racket

; Shamelessly borrowed from https://github.com/florence/convex-hulls

(provide quickhull)
(require "shared.rkt")
(module+ test (require typed/rackunit))

(: quickhull : (Listof Point) -> (Listof Point))
(define (quickhull points)
  (define leftmost (argmin .x points))
  (define rightmost (argmax .x points))
  (define pts 
    (set-remove
     (set-remove (list->set points) leftmost)
     rightmost))
  (define-values (left right)
    (for/fold ([left : (Listof Point) null] [right : (Listof Point) null])
              ([p pts])
      (if (right? leftmost rightmost p)
          (values left (cons p right))
          (values (cons p left) right))))

  (side-add rightmost leftmost left
            (side-add leftmost rightmost right (list rightmost leftmost))))

(: side-add : Point Point (Listof Point) (Listof Point) -> (Listof Point))
(define (side-add a b source hull)
  (match source
    [(list) hull]
    [(list p) (insert-at hull b p)]
    [else
     (define p (argmax (lambda ([p : Point]) (dist-from-line a b p)) source))
     (define remd (remove p source))
     (define newhull (insert-at hull b p))

     (define-values (ap pb)
       (for/fold ([ap : (Listof Point) null] [pb : (Listof Point) null])
                 ([m remd])
         (values (if (not (right? a p m)) ap (cons m ap))
                 (if (not (right? p b m)) pb (cons m pb)))))
     (side-add a p ap
               (side-add p b pb newhull))]))

(: insert-at : (Listof Point) Point Point -> (Listof Point))
(define (insert-at l loc p)
  (match l
    [(list) (error "should never get here")]
    [(cons f r)
     (if (equal? f loc)
         (list* f p r)
         (cons f (insert-at r loc p)))]))
(module+ test
  (check-equal? (insert-at (list 1 2 4) 2 3)
                (list 1 2 3 4)))

(: dist-from-line : Point Point Point -> Real)
(define (dist-from-line a b p)
  (/ (abs (- (* (- (.x b) (.x a)) (- (.y a) (.y p)))
             (* (- (.y b) (.y a)) (- (.x a) (.x p)))))
     (magnitude (- b a))))

(module+ test
  (check-equal? (dist-from-line 0 2 1+1i) 1)
  (check-equal? (dist-from-line 2 0 1+1i) 1)
  (check-equal? (dist-from-line 2 0 1) 0)
  (check-equal? (dist-from-line 2 0 0+1i) 1))

(: right? : Point Point Point -> Boolean)
(define (right? a b p)
  (< (* (- (.x b) (.x a)) (- (.y p) (.y a)))
     (* (- (.y b) (.y a)) (- (.x p) (.x a)))))

(module+ test
  (check-false (right? 0 3+3i 1+2i))
  (check-true (right? 0 3+3i 1-2i))
  (check-false (right? 10 10+10i 5+5i)))
