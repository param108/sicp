;; 2.3 second implementation
;;
;; load the points and segments
(load "2_02.sch")

;;rect as a list of 4 segments
;;points here are in clockwise direction
(define (make-rect-sides seg1 seg2 seg3 seg4)
  (list seg1 seg2 seg3 seg4))

;; returns an array of side lengths
(define (side-lengths-rect rect)
  (map length-segment rect))

;; returns the height of the rect
;; by conventions sides with min length are considered height
(define (height-rect rect)
  (let ((lengths (side-lengths-rect rect)))
  (reduce min (car lengths) lengths)))

;; returns the width of the rect
;; by convention sides with max length are considered widths
(define (width-rect rect)
  (let ((lengths (side-lengths-rect rect)))
    (reduce max (car lengths) lengths)))

;; area and perimeter are defined in 2_02.sch
(let ((rect (make-rect-sides
             (make-segment (make-point 4 0) (make-point 4 10))
             (make-segment (make-point 4 10) (make-point 10 10))
             (make-segment (make-point 10 10) (make-point 10 0))
             (make-segment (make-point 10 0) (make-point 4 0)))))
  (newline)
  (display (perimeter rect)) ;; 32
  (newline)
  (display (area rect))) ;; 60
