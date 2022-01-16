;; 2.6
;;

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; calling add-1 on zero
(define one
  ;;(lambda (f) (lambda (x) (f ((zero f) x)))))
  ;;(lambda (f) (lambda (x) (f ((lambda(x) x) x)))))
  (lambda (f) (lambda (x) (f x))))

;; calling add-1 on one
(define two
  ;;(lambda (f) (lambda (x) (f ((one f) x)))))
  ;;(lambda (f) (lambda (x) (f ((lambda(x) (f x)) x)))))
  (lambda (f) (lambda (x) (f (f x)))))

;; It seems that numbers are
;; (lambda (f) (lambda (x) (f x)))     -> 1
;; (lambda (f) (lambda (x) (f (f x)))) -> 2
;; i.e the number of times f is applied in lambda increases linearly with the value.
;; So then addition is just applying f the number of times f is applied in 'a' plus
;; the number of times f is applied in 'b'
;; suppose b is 2 and a is 1
;; ((b f) x) -> (f (f x))
;; (a f) -> (lambda (x) (f x))
;; ((a f) ((b f) x)) -> (f (f (f x))) which is 3
(define (+ a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
