;; 1.37
;;

(define (cont-frac n d k)
  (define (iter x)
    (if (> x k)
        0
        (/ (n k) (+ (d k) (iter (+ x 1))))))
  (iter 1))

;; need 14 to get to 1.61803
(/ 1 (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0) 14))
