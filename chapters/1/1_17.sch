;; 1.17 & 1.18

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double a)
  (+ a a))

(define (halve a)
  (/ a 2))

;; recursive
(define (fast-mult-recursive a b)
    (cond ((= b 0) 0)
          ((even? b) (fast-mult-recursive (double a) (halve b)))
          (else (+ a (fast-mult-recursive a (- b 1))))))

  ;; iterative
(define (fast-mult a b)
  (define (fast-mult-iter  ac a b)
    (cond ((= b 0) ac)
          ((even? b) (fast-mult-iter ac (double a) (halve b)))
          (else (fast-mult-iter (+ ac a) a (- b 1)))))
  (fast-mult-iter 0 a b))
