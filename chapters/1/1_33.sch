;; 1.33
;;

;; filter is a predicate which returns the input value if the value is selected
;; and null-value otherwise.
(define (accumulate combiner null-value term a next b filter)
  (define (iter x acc)
    (if (> x b)
        acc
        (iter (next x) (combiner acc (term (filter x)))))
    )
  (iter a null-value)
  )

(define (sum-of-prime-squares a b)
  ;; a hacky prime? function for testing only
  (define (prime? x)
    (case x
      ((2 3 5 7 9 11) #t)
      (else #f))
    )
  (accumulate
   ;; combiner
   (lambda (x y) (+ x y))
   ;; null-value
   0
   ;; term
   (lambda (x) (* x))
   a
   ;; next
   (lambda (x) (+ x 1))
   b
   ;; filter
   (lambda (x) (if (prime? x) x 0))
   )
  )

(sum-of-prime-squares 1 10)

(define (product-relative-primes b)
  ;; hacky gcd function for test only.
  (define (gcd-one x y)
    (case x
      ;; relative primes to 10
      ((3 7 9) #t)
      (else #f)))

  (accumulate
   ;; combiner
   (lambda (x y) (* x y))
   ;; null-value
   1
   ;; term
   (lambda (x) x)
   1
   ;; next
   (lambda (x) (+ x 1))
   b
   ;; filter
   (lambda (x) (if (gcd-one x b) x 1))
   )
  )

(product-relative-primes 10)
