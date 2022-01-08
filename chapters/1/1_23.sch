;; 1.23
;;

(define (smallest-divisor n) (find-divisor n 2))
(define (next n)
  (cond
   ((= n 2) 3)
   (else (+ n 2))))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))

;; reason for not 2 is because the growth is 0(root(n)) so growth will be 0(root(n)/2)
