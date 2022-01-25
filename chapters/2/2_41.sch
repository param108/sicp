;; 2.41
;;

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

(define (unique-triads n)
  (flatmap (lambda (x)
             (map (lambda (y)
                    (append y (list x)))
                  (unique-pairs (- x 1)))) (enumerate-interval 1 n)))

(define (triad-sums n s)
  (filter (lambda (triad)
            (= s (+ (car triad) (cadr triad) (caddr triad))))
          (unique-triads n)))

(newline)
(display "2.41")
(newline)
(display (triad-sums 9 12))
;; ((3 4 5) (2 4 6) (1 5 6) (2 3 7) (1 4 7) (1 3 8) (1 2 9))
