;; 2.2
;;

;; Points
(define (make-point x y)
  (cons x y))

(define (x-point pt)
  (car pt))

(define (y-point pt)
  (cdr pt))

;; sqrt( (x1 -x2)^2 + (y1 - y2)^2 )
(define (distance-point pt1 pt2)
  (sqrt
   (+
    (expt (- (x-point pt1) (x-point pt2)) 2)
    (expt (- (y-point pt1) (y-point pt2)) 2))))

;; Line-segments
(define (make-segment start-pt end-pt)
  (cons start-pt end-pt))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (length-segment segment)
  (distance-point (start-segment segment) (end-segment segment))
  )

(define (midpoint-segment segment)
  (define (average x y)
    (/ (+ x y) 2))
  (make-point (average (x-point (start-segment segment))
                       (x-point (end-segment segment)))
              (average (y-point (start-segment segment))
                       (y-point (end-segment segment)))))

;; for testing
(let ((s (make-segment (make-point 2 3) (make-point 4 5))))
  (newline)
  (display (x-point (start-segment s))) ;; should be 2
  (newline)
  (display (x-point (end-segment s)))   ;; should be 4
  (newline)
  (display (y-point (start-segment s))) ;; should be 3
  (newline)
  (display (y-point (end-segment s)))   ;; should be 5
  (newline)
  (display (midpoint-segment s))        ;; should be (3 . 4)
  (newline)
  )

;; uses only the interfaces provided by rect
(define (perimeter rect)
  (reduce + 0 (side-lengths-rect rect))
  )

(define (area rect)
  (* (height-rect rect) (width-rect rect))
  )
