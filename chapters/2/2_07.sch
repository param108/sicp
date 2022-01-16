;; 2.7
;;

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (make-interval a b) (cons a b))

(define (upper-bound z)
  (car z))

(define (upper-bound z)
  (cdr z))

;; 2.8
;;
;; The minimum value would be (lower-bound x) - (upper-bound y)
;; The maximum value would be (upper-bound x) - (lower-bound y)
(define (sub-interval x y )
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; 2.9
;;
;; let intervals be (a1 b1) , (a2 b2)
;; sum -> (a1+a2 b1+b2)
;; width of sum = ((b1+b2) - (a1+a2))/2
;; = (b1 - a1)/2 + (b2 - a2)/2
;; = width1 + width2  -> related to width of input intervals
;;
;;multiplication -> (a1*a2 b1*b2)
;;width = (b1*b2 - a1*a2)/2 -> not related to width of input intervals
;;
;;subtraction = (a1-b2 b1-a2)
;;width = (b1-a2 - (a1 - b2))/2
;;= (b1 - a1)/2 + (b2 - a2)/2 -> related to wideth of input intervals

;; 2.10
;;
(define (div-interval x y)
  ;; intervals that span over 0 are invalid
  (define (validate x)
    (if (and (>= (upper-bound x) 0) (<= (lower-bound x) 0)) #f #t))
  (if (and (validate x) (validate y))
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))
      -1))

;; write tests for all these.
