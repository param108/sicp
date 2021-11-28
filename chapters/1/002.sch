;; 1.9
(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))

;; (+ 4 5)
;; (inc (+ (dec 4) 5))
;; (inc (inc (inc (+ (dec (dec 4)) b))))
;; (inc (inc (inc (inc (+ (dec(dec(dec 4))) b)))))
;; (inc (inc (inc (inc b))))

(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))

;; (+ 4 5)
;; (+ (dec 4) (inc 5))
;; (+ (dec (dec 4)) (inc (inc 5)))
;; (+ (dec (dec (dec 4))) (inc (inc (inc 5))))
;; (+ (dec (dec (dec (dec 4)))) (inc (inc (inc (inc 5)))))
;; (inc (inc (inc (inc 5))))

;; both are actually linear processes dec and inc can be evaluated at each level
;; independently

;; 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))


(A 1 10) ;; 1024
(A 2 4) ;; 65536
(A 3 3) ;; 65536

(define (f n) (A 0 n)) ;; 2 * n
(define (g n) (A 1 n))
;; (g 0) => 0
;; (g 1) => 2
;; (g 2) => (A 0 (A 1 1)) => (A 0 2) => 4
;; (g 3) => (A 0 (A 1 2)) => (A 0 (A 0 (A 1 1)) => (A 0 (A 0 2)) => 8
;; (g 4) => (A 0 (A 1 3)) => (A 0 (A 0 (A 1 2))) => (A 0 (A 0 (A 0 (A 1 1)))) => 16
;; => 2^n
(define (h n) (A 2 n))
;; (h 0) => 0
;; (h 1) => 2
;; (h 2) => 4
;; (h 3) => 16
;; (h 4) => 65536
;; you should see the next number!!!!
;; => 2^(h (- n 1))
(define (k n) (* 5 n n))
;; ( k 1) => 5
;; (k 2) => 20
;; => 5(n^2)

;; 1.11
;; recursive
(define (recur_1_11 n)
  (cond ((< n 3) n)
        ( else ( + (recur_1_11 (- n 1))
                   (* 2 (recur_1_11 (- n 2)))
                   (* 3 (recur_1_11 (- n 3)))))))
;; iterative

(define (iter_1_11 n)
  ;; x -> f(n - 1)
  ;; y -> f(n - 2)
  ;; z -> f(n - 3)
  (define (sum-iter x y z)
    (+ x (* 2 y) (* 3 z)))
  ;; iterates until i reaches n
  ;; stores f(n - 1), f(n - 2), f(n -3) in x, y, z
  ;; each iteration
  ;; y -> z
  ;; x -> y
  ;; sum-iter -> x
  (define (calc-iter i x y z)
    (cond
     ((= i n) (sum-iter x y z))
     (else (calc-iter (+ i 1) (sum-iter x y z) x y))))

  ;; main
  (cond ((< n 3) n)
          (else (calc-iter 3 2 1 0))))
