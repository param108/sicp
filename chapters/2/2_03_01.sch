;; 2.03 first version of rect
;;

;; load the points and segments
(load "2_02.sch")

;;rect as a list of 4 points
;;points here are in clockwise direction
(define (make-rect-pts pt1 pt2 pt3 pt4)
  (list pt1 pt2 pt3 pt4))

;; returns an array of side lengths
(define (side-lengths-rect rect)
  (let ((pt1 (list-ref rect 0))
        (pt2 (list-ref rect 1))
        (pt3 (list-ref rect 2))
        (pt4 (list-ref rect 3))
        )
    (list (distance-point pt1 pt2)
          (distance-point pt2 pt3)
          (distance-point pt3 pt4)
          (distance-point pt4 pt1))))

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
(let ((rect (make-rect-pts
             (make-point 4 0)
             (make-point 4 10)
             (make-point 10 10)
             (make-point 10 0))))
  (newline)
  (display (perimeter rect)) ;; 32
  (newline)
  (display (area rect))) ;; 60
