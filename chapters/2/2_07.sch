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

;; It is assumed that 'a' is the lower-bound and 'b' is upper-bound
(define (make-interval a b) (cons a b))

;; upper-bound is second
(define (upper-bound z)
  (cdr z))

;; lower-bound is first
(define (lower-bound z)
  (car z))

;; 2.8
;;
;; The minimum value would be (lower-bound x) - (upper-bound y)
;; The maximum value would be (upper-bound x) - (lower-bound y)
(define (sub-interval x y )
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; test
(let ((x (make-interval 2 3)) (y (make-interval 3 4)))
  (newline)
  (display "2.8=====")
  (newline)
  (display (sub-interval x y)) ;; should be (-2 . 0)
  (newline)
  )
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
  ;; intervals that span over 0 are invalid on the denominator
  (define (validate x)
    (if (and (>= (upper-bound x) 0) (<= (lower-bound x) 0)) #f #t))
  ;; only need to validate the denominator
  (if (validate y)
      (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))
      -1))

;; test
(let ((x (make-interval 2 3)) (y (make-interval 3 4))    ;; valid
      (x1 (make-interval 0 3)) (y1 (make-interval 3 4))   ;; valid
      (x2 (make-interval -10 0)) (y2 (make-interval 3 4))   ;; valid
      (x3 (make-interval 2 3)) (y3 (make-interval -3 4))   ;; invalid
      (x4 (make-interval 2 3)) (y4 (make-interval 0 4))   ;; invalid
      (x5 (make-interval 2 3)) (y5 (make-interval -4 0))   ;; invalid
      )
  (newline)
  (display "2.10=====")
  (newline)
  (display (div-interval x y))   ;; (.5 . 1.)
  (newline)
  (display (div-interval x1 y1)) ;; (0 . 1)
  (newline)
  (display (div-interval x2 y2)) ;; (-3.33 . 0)
  (newline)
  (display (div-interval x3 y3)) ;; -1
  (newline)
  (display (div-interval x4 y4)) ;; -1
  (newline)
  (display (div-interval x5 y5)) ;; -1
  (newline)
  )

;; 2.11
;;

;; My thoughts on this: I would NOT do this unless I hit a CPU performance issue and
;; this is the top of the profile
(define (mul-interval-improved x y)
  (cond
   ;; all four bounds are greater than zero then we only need to muliply upper-bounds
   ;; and multiply lower-bounds
   ;; > > > >
   ((and (> (lower-bound x) 0) ( >= (upper-bound x) 0) (>= (lower-bound y) 0) (>= (upper-bound y) 0))
   (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))

   ;; all four bounds are less than zero then same as above.
   ;; < < < <
   ((and (< (lower-bound x) 0) ( < (upper-bound x) 0) (< (lower-bound y) 0)  (< (upper-bound y) 0))
   (make-interval  (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))

   ;; only (lower-bound x) is less than 0; the smallest number is (lower-bound x) * (upper-bound y)
   ;; < > > >
   ((and (< (lower-bound x) 0) (>= (upper-bound x) 0) (>= (lower-bound y) 0)  (>= (upper-bound y) 0))
   (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))

   ;; > > < >
   ((and (>= (lower-bound x) 0) (>= (upper-bound x) 0) (< (lower-bound y) 0)  (>= (upper-bound y) 0))
   (make-interval (* (lower-bound y) (upper-bound x)) (* (upper-bound x) (upper-bound y))))

   ;; < < > >
   ((and (< (lower-bound x) 0) (< (upper-bound x) 0) (>= (lower-bound y) 0)  (>= (upper-bound y) 0))
    (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))

   ;; > > < <
   ((and (>= (lower-bound x) 0) (>= (upper-bound x) 0) (< (lower-bound y) 0)  (< (upper-bound y) 0))
    (make-interval (* (lower-bound y) (upper-bound x)) (* (upper-bound y) (lower-bound x))))

   ;; < > < >
   ((and (< (lower-bound x) 0) (>= (upper-bound x) 0) (< (lower-bound y) 0) (>= (upper-bound y) 0))
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
      (make-interval (min p1 p2 p3 p4)
                     (max p1 p2 p3 p4))))
   ;; < < < >
   ((and (< (lower-bound x) 0) (< (upper-bound x) 0) (< (lower-bound y) 0)  (>= (upper-bound y) 0))
    (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))

   ;; < > < <
   ((and (< (lower-bound x) 0) (>= (upper-bound x) 0) (< (lower-bound y) 0)  (< (upper-bound y) 0))
    (make-interval (* (lower-bound y) (upper-bound x)) (* (lower-bound x) (lower-bound y))))

  ))

;; test
;; compare our function output with the original mul-interval
(let ((x (make-interval 2 3)) (y (make-interval 3 4))       ;; > > > >
      (x1 (make-interval -5 -4)) (y1 (make-interval -7 -2)) ;; < < < <
      (x2 (make-interval -10 0)) (y2 (make-interval 3 4))   ;; < > > >
      (x3 (make-interval 2 3)) (y3 (make-interval -3 4))    ;; > > < >
      (x4 (make-interval -10 -2)) (y4 (make-interval 0 4))  ;; < < > >
      (x5 (make-interval 2 3)) (y5 (make-interval -10 -2))  ;; > > < <
      (x6 (make-interval -2 3)) (y6 (make-interval -4 0))   ;; < > < >
      (x7 (make-interval -10 -3)) (y7 (make-interval -4 0))    ;; < < < >
      (x8 (make-interval -10 3)) (y8 (make-interval -10 -2))    ;; < > < <
      )
  (newline)
  (display "2.11=====")
  (newline)
  (let ((i 0))
    (map (lambda (z)
           (display i)
           (display ". ")
           (display (mul-interval (car z) (cdr z)))
           (newline)
           (display (mul-interval-improved (car z) (cdr z)))
           (newline)
           (set! i (+ i 1))
           ) (list (cons x y) (cons x1 y1) (cons x2 y2) (cons x3 y3) (cons x4 y4) (cons x5 y5)
                   (cons x6 y6) (cons x7 y7) (cons x8 y8)))
    ))
