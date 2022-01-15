;; 2.1
;;

;; returns the positive gcd of two numbers
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder (abs a) (abs b)))))

(define (make-rat n d)
  (let ((g (gcd n d)) (numer 0) (denom 0))
    (set! numer (cond
                  ;; if both numerator and denominator are negative
                  ;; then numerator can be positive
                  ((and (< n 0) (< d 0)) (abs n))
                  ;; if only denomiator is negative then numerator can
                  ;; be made negative.
                  ((and (> n 0) (< d 0)) (- 0 n))
                  ;; if numerator is negative and denominator is positive
                  ;; OR if numerator and denominator is positive
                  ;; then the numerator remains unchanged
                  (else n)))
    (set! denom (abs d))
    (cons (/ numer g) (/ denom g))))

(newline)
(display (make-rat -25 -20)) ;; (5 . 4)
(newline)
(display (make-rat -25 20))  ;; (-5 . 4)
(newline)
(display (make-rat 25 20))   ;; (5 . 4)
(newline)
(display (make-rat 25 -20))  ;; (-5 . 4)
(newline)
