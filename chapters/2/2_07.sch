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

;; 2.12
;;
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (/ (* c p) 100)))
    (make-center-width c w)))

(define (percent i)
  (* (/ (width i) (center i)) 100))

(display "2.12")
(newline)
(display (make-center-width 20 5))
(newline)
(display (make-center-percent 20 25)) ;; should be the same as above
(newline)
(display (percent (make-center-percent 20 25))) ;; 25


;; 2.13
;;
;; consider two intervals (c1,w1) (c2, w2)
;; the new interval limits on multiplication can be
;; (c1+w1) * (c2+w2) = c1c2 + c1w2 + c2w1 + w2w1
;; as w1 and w2 are very small w1*w2 can be ignored
;;                   = c1c2 + c1w2 + c2w1
;; replacing w2 and w1 with c2p2/100 and c1p1/100
;;                   = c1c2 + c1c2p2/100 + c1c2p1/100
;;                   = c1c2 + c1c2*(p1 + p2)/100
;; so the new percentage tolerance is (p1 + p2)/100
;; (c1+w1) * (c2-w2) = c1c2 - c1w2 + c2w1 -w1w2
;; dropping w1w2 as it will be very small
;;                   = c1c2 -c1c2p2/100 + c1c2p1/100
;;                   = c1c2 + c1c2*(p1 -p2)/100
;; (c1-w1) * (c2+w2)
;; similarly         = c1c2 + c1c2*(p2 - p1)/100
;; (c1-w1) * (c2-w2)
;; similarly         = c1c2 - cic2*(p1 + p2)/100
;;
;; as p1 and p2 are always positive (p1 + p2) > (p1 - p2) and (p1 + p2) > (p2 - p1)
;; Therefore the percentage in the final interval will be (p1 + p2)

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(let ((r1 (make-interval 2.9 3)) (r2 (make-interval 3.9 4)))
  (display "2.14===== experiment 1")
  (newline)
  (display (par1 r1 r2))
  (newline)
  (display (par2 r1 r2))
  (newline))

(let ((r1 (make-interval 2.5 3)) (r2 (make-interval 3.5 4)))
  (display "2.14===== experiment 2")
  (newline)
  (display (par1 r1 r2))
  (newline)
  (display (par2 r1 r2))
  (newline))

;; So smaller the error the closer the two are. The reason for this is that par1 has
;; many interval-interval calculations with uncertain numbers (widths > 0)
;; which increases the width of the final. Where as par2 has interactions
;; with certain numbers (i.e width 0) so the width will be much tighter.
;; par2 is better because less interaction with uncertain numbers means the error width will be
;; the least. Remember both are actually correct. One must be fully enclosed in the other.

;; 2.16
;; The differences are because each interaction between uncertain numbers modifies the center
;; and the width. The operations on intervals are not a [field](https://en.wikipedia.org/wiki/Field_%28mathematics%29?%7B%7B%7Bqs%7D%7D%7D)
