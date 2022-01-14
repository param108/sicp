;; 1.43
;;

;; iterative repeated
(define (repeated f n)
  (define (iter i acc)
    (if (= i n)
        (lambda (x) (acc x))
        (iter (+ i 1) (compose acc f))))
  (iter 1 f))

(define (smooth f)
  (let ((dx 0.00001))
    ;; (f(x - dx) + f(x) + f(x + dx))/3
    (lambda (x) (/ 3 (+ (f (- x dx)) (f x) (f (+ x dx))))))
  )

(define (n-smooth f n)
  (repeated (smooth f) n))

;; smoothing of the identity function should return the identity function again
((n-smooth (lambda (x) x) 10) 13) ;; should return 13

;; TODO is there a more interesting function that can be smoothed to prove this is correct?
