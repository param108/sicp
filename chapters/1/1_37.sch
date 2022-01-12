;; 1.37
;;
;; recursive
(define (cont-frac n d k)
  (define (iter x)
    (if (> x k)
        0
        (/ (n x) (+ (d x) (iter (+ x 1))))))
  (iter 1))

;; need 14 to get to 1.61803
(newline)
(display "recursive")
(newline)
(display (/ 1 (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0) 14)))
(newline)
;; iterative
;;
(define (cont-frac n d k)
  (define (iter x acc)
    (if (= x 0)
        acc
        (iter (- x 1) (/ (n x) (+ (d x) acc)))))
  (iter k 0))

;; need 14 to get to 1.61803
(display "iterative")
(newline)
(display (/ 1 (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0) 14)))
(newline)

;; 1.38
;;

(define (euler k)
  (define (euler-d k)
    (cond
     ((= k 1) 1)
     ((= k 2) 2)
     ;; values for which (k - 2) is a multiple of 3 are not 1
     ;; they can be calculated by ((((k - 2) / 3 ) * 2) + 2)
     ((= (remainder (- k 2) 3) 0) (+ (* (/ (- k 2) 3) 2) 2))
     ;; other numbers are 1
     (else 1)
     )
    )
  (cont-frac (lambda (i) 1.0) euler-d k)
  )

(display "euler")
(newline)
(display (euler 14))
(newline)
