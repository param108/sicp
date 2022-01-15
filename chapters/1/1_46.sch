;; 1.46
;;

;; copied from 1_45.sch

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


;; 1.46
;; both good-enough? and improve are single parameter functions
;; the parameter is the current guess.
(define (iterative-improve good-enough? improve)
  (lambda (guess)
    (define (iter g)
      (if (good-enough? g)
          g
          (iter (improve g))
          ))
    (iter guess)))

(define tolerance 0.000001)

;; fixed-point returns a function that implements fixed-point function
;; using the iterative-improve generic algorithm above.
(define (fixed-point f)
  ;; entry point
  ;; if its the first guess we should return false in good-enough?
  ;; as we don't have a previous value yet.

  (define first #t)

  ;; prev stores the previous guess for comparing the next guess.
  (define prev 0)

  ;; good-enough
  ;; Instead of passing this as a lambda, I extracted as function because it is so large
  (define (good-enough? x)
    (cond
     (first
      ;; if its first there is no prev, so just set prev and return false.
      ;; also set first to false to allow the code to know the first is complete.
      (set! first #f)
      (set! prev x)
      #f)
     (else
      (let ((ret #f))
        ;; if the difference between prev and x is less than tolerance then the guess
        ;; is good enough
        (if (< (abs (- prev x)) tolerance)
            ;; less than tolerance
            (set! ret #t)
            ;; more than tolerance
            (set! ret #f))
        (set! prev x)
        ret))))

  ;; entry-point
  (iterative-improve
     ;; good-enough?
     good-enough?
     ;; improve
     (lambda (x)
       (f x)))
  )

;; using the example given and trying out on other n
;; we need to do (n - 1) dampings for fixed point
;; to converge for y = x/y^(n-1)
(define (nth-root x n)
  (define (eqn y)
    (/ x (expt y (- n 1))))
  ;; fixed-point returns a function now, so we need to apply it
  ((fixed-point ((repeated average-damp (- n 1)) eqn)) 1.0))

(nth-root 1024 5)
