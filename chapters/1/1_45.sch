;; 1.45
;;

;; copied from 1.35
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

;; iterative repeated
;; initially set acc to f
;; at each iteration store acc = compose (acc f)
(define (repeated f n)
  (define (compose f g)
    (lambda (x) (f (g x))))
  (define (iter i acc)
    (if (= i n)
        (lambda (x) (acc x))
        (iter (+ i 1) (compose acc f))))
  (iter 1 f))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

;; using the example given and trying out on other n
;; we need to do (n - 1) dampings for fixed point
;; to converge for y = x/y^(n-1)
(define (nth-root x n)
  (define (eqn y)
    (/ x (expt y (- n 1))))
  (fixed-point ((repeated average-damp (- n 1)) eqn) 1.0))

(nth-root 1024 5)
