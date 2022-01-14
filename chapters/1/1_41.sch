;; 1.41
;;

(define (double f)
  (lambda (x) (f (f x)))
  )

(define (inc x)
  (+ 1 x))

(newline)
(display (((double (double double)) inc) 5))

;; (double double) => (double (double x))
;; (double (double double))  => (double (double (double (double x))))
;; (double (double double)) inc => ( inc ( ...16 times ) 5) = 21
