;; 1.30
;; iterative sum
;;

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (sum-cube a b)
  (sum (lambda (x) (* x x x))
       a
       (lambda (x) (+ x 1))
       b))
