;; 1_28
;;
(define (miller-rabin n)
  ;; (a^(n-1))%n
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder
            (square (expmod base (/ exp 2) m))
            m))
          (else
           (remainder
            (* base (expmod base (- exp 1) m))
            m))))

  ;; checks first a^2%n = 1%n -> no prime
  ;; then check a^(- n 1)%n = 1%n -> probably prime (fermats test)
  (define (check a)
    (cond
     ((= (expmod a 2 n) 1) #f)
     ((= (expmod a (- n 1) n) 1) #t)
     (else #f)))

  (define (next)
    (let ((val (random (- n 1))))
      (if (< val 3) (next) val)))

  (define (run a cnt)
    (cond
     ((< n 2) "not prime")
     ((= n 2) "prime")
     ((= n 3) "prime")
     ((> cnt (/ n 2)) "prime")
     ((check a) (run (next) (+ cnt 1)))
     (else "not prime")))
  (run 2 0))

;; This is not the solution that is requested in the book as that does some
;; hacky modification of expmod. I have coded it a bit
