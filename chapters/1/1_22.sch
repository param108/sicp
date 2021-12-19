
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (* 1000 (runtime))))

;; had to modify this to return the value of prime?
(define (start-prime-test n start-time)
  (let ((q (prime? n)))
    (if q (report-prime (- (* 1000 (runtime)) start-time))
    q)))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start)
  ;; found is an accumulator that accumulates primes
  ;; as they are found
  (define (search-iter next found)
    ;; we only want 3 primes
    (if (equal? (length found) 3)
        found
        (if (timed-prime-test next)
            (search-iter (+ 2 next) (append found (list next)))
            (search-iter (+ 2 next) found))))
  ;; if start is even start the procedure on the next number
  (if (eqv? 0 (modulo start 2))
      (search-for-primes (+ 1 start))
      (search-iter start '())))

;; strangely the time taken varies arbitrarily
;; does not grow linearly, this could be because of context switching
;; and the fact that the present computers are blazing fast.
;; e.g.
;; 10000103 *** 10.
;; 100000001
;; 100000003
;; 100000005
;; 100000007 *** 0.
;; so prime check for 100000007 takes no time at all while that of 10000103 took 10 units

(search-for-primes 1000)
(search-for-primes 10000)
(search-for-primes 100000)
(search-for-primes 1000000)
(search-for-primes 10000000)
(search-for-primes 100000000)
