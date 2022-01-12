;; 1.35 & 1.36
;;

(define tolerance 0.000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

;; phi = 1 + 1/phi
(define (phi-eqn x)
  (+ 1 (/ 1 x)))

(fixed-point phi-eqn 10.0)
;; using average
;;

(fixed-point (lambda (y) (/ (+ y (phi-eqn y)) 2)) 10.0)

(define (log-eqn x)
  (/ (log 1000) (log x)))

(fixed-point log-eqn 10.0)

;; using average
(fixed-point (lambda (y) (/ (+ y (log-eqn y)) 2)) 10.0)
