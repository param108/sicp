;; 1.32

(define (accumulate combiner null-value term a next b)
  (define (iter x acc)
    (if (> x b)
        acc
        (iter (next x) (combiner acc (term x))))
    )
  (iter a null-value)
  )

(define (sum a b)
  (accumulate
   ;; combiner
   (lambda (x y) (+ x y))
   ;; null-value
   0
   ;; term
   (lambda (x) x)
   a
   ;; next
   (lambda (x) (+ x 1))
   b)
  )

(define (product a b)
  (accumulate
   ;; combiner
   (lambda (x y) (* x y))
   ;; null-value
   1
   ;; term
   (lambda (x) x)
   a
   ;; next
   (lambda (x) (+ x 1))
   b)
  )


(newline)
(display "Sum: ")
(display (sum 1 5))
(newline)
(display "Product: ")
(display (product 1 5))
(newline)
