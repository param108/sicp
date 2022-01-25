;; 2.40
;;

;; copied from 1.22
;;
(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

;; copied code
(define nil ())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval i j)
  (define (iter x output)
    (cond
     ((<= x j) (iter (+ 1 x) (append output (list x))))
     (else output)))
  (iter i nil))

(define (unique-pairs n)
  (flatmap (lambda (x)
             (map (lambda (y) (list y x))
                  (enumerate-interval 1 (- x 1)))) (enumerate-interval 1 n)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(newline)
(display "2.40")
(newline)
(display (prime-sum-pairs 10))
;; ((1 2 3) (2 3 5) (1 4 5) (3 4 7) (2 5 7) (1 6 7)
;;  (5 6 11) (4 7 11) (6 7 13) (3 8 11) (5 8 13)
;;  (2 9 11) (4 9 13) (8 9 17) (1 10 11) (3 10 13)
;;  (7 10 17) (9 10 19))
