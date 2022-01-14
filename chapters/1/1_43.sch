;; 1.43
;;

(define (compose f g)
  (lambda (x) (f (g x))))

;; recursive repeated
;; f^n(x) = f(f^n-1(x))
;; f^1(x) = f(x)
(define (repeated f n)
  (define (iter i)
    (if (= i n)
        ;; terminating condition is the application of the fn f on x
        (lambda (x) (f x))
        ;; otherwise return a lambda that applies f on the lambda returned by (iter (+ 1 i))
        (compose f (iter (+ i 1)))))
  (iter 1)
  )

;; iterative repeated
;; initially set acc to f
;; at each iteration store acc = compose (acc f)
(define (repeated-iter f n)
  (define (iter i acc)
    (if (= i n)
        (lambda (x) (acc x))
        (iter (+ i 1) (compose acc f))))
  (iter 1 f))

(define (inc x)
  (+ x 1))

(define (square x)
  (* x x))

(newline)
(display ((repeated inc 5) 1)) ;; should return 6
(newline)
(display ((repeated square 2) 5)) ;; should return 625
(newline)
(display ((repeated-iter inc 5) 1)) ;; should return 6
(newline)
(display ((repeated-iter square 2) 5)) ;; should return 625
