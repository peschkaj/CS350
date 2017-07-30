#lang racket


(struct point (x y) #:transparent)

(define (make-negative number)
  (cond [(> 0 number) number]
        [(- 0 number)]))


(define (compare-x p1 p2)
  (- (point-x p1) (point-x p2)))

(define (compare-y p1 p2)
  (- (point-y p1) (point-y p2)))

(define (square x)
  (* x x))


(define (point-distance point1 point2)
  (let* ([xdiff (compare-x point1 point2)]
         [ydiff (compare-y point1 point2)])
    (+ (square xdiff)
       (square ydiff))))
         

; Generates a list of points
; The x of each point will be between negative max-x and max-x
; The y of each point will be between megative max-y and max-y
(define (generate-point-list max-length max-x max-y)
  (let* ([min-x (make-negative max-x)]
         [min-y (make-negative max-y)])
    (cond [(= max-length 0) empty]
          [else (cons
                 (point (random min-x (+ 1 max-x))
                        (random min-y (+ 1 max-y)))
                 (generate-point-list (- max-length 1) max-x max-y))])))


; Following basic recursive algorithm from https://rosettacode.org/wiki/Closest-pair_problem
(define (closest-pair x-list y-list lst-length)
    (let-values ([(xL xR) (split-at sorted-by-x (/ lst-length 2))]
                 [(yL yR) (split-at sorted-by-y (/ lst-length 2))]
                 [x-mid (car (take sorted-by-x (ceil (/ lst-length 2))))]
                 [(dL pairL) (closest-pair xL yL (/ lst-length 2))]
                 [(dR pairR) (closest-pair xR yR (/ lst-length 2))]
                 [(dMin pairMin) (cond [(< dL dR) (cons dL pairL)]
                                       [(cons dR pairR)])]
                 ;; IMPLEMENT THE REST OF THIS
                 )
    (fprintf (current-output-port)
             "~a\n\n\n\n~a\n"
             sorted-by-x
             sorted-by-y)
  )))

(define (closest-pair-in lst)
  (let* ([sorted-by-x (sort lst < #:key point-x)]
         [sorted-by-y (sort lst < #:key point-y)])
    (find-closest-pair sorted-by-x sorted-by-y (length lst))))