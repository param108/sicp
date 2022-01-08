;; 1.31
;;
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1)
)


(define (fact n)
  (product
   (lambda (x) x)
   1
   (lambda (x) (+ 1 x))
   n)
  )

;; pi using wallis product
;; The product is of the form pi/4 = (7/2)*(2*2*4*4*6*6/3*3*5*5*7*7)
;; ie remove the first 2 from the numberator and remove the last odd number at the bottom.
;; the odd number at the end is thus (2n + 1)
;; as we need to multiply by 4 and divide by 2 we just multiply by 2
(define (pi n)
  (* 2 (+ (* 2 n) 1) (/ (product
           (lambda (x) (* 2 x 2 x))
           1
           (lambda (x) (+ 1 x))
           n)
          (product
           (lambda (x) (* (+ (* 2 x) 1) (+ (* 2 x) 1)))
           1
           (lambda (x) (+ 1 x))
           n)))
  )
