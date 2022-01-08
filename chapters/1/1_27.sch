;; 1.27
;; Carmichael numbers
;;

(define (fermat n)
  ;; (a^n)%n
  (define (expmod a)
    (remainder (expt a n) n))
  ;; (a^n)%n == a%n
  (define (check a)
    (= (expmod a) (remainder a n)))
  ;; call check for all (a > 1 && a < n)
  (define (run a)
    (cond
     ((= a n) #t)
     ((check a) (run (+ 1 a)))
     (else #f)))
  (run 2))

(define (test-fermat n)
  (if (fermat n)
      "prime"
      "non-prime"))

;; main
(newline)
(display (map test-fermat '(561 1105 1729 2465 2821 6601)))

;; (prime prime prime prime prime prime)
