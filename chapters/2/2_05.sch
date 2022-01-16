;; 2.5
;;


(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

;; Find the number of times the number can be divided by 2
;; without remainder. That will be the exponent a
(define (car z)
  (define (iter v n)
    (cond
     ((= 0 (remainder v 2)) (iter (/ v 2) (+ 1 n)))
     (else n)))
  (iter z 0))

;; Find the number of times the number can be divided by 3
;; without remainder. That will be the exponent b
(define (cdr z)
  (define (iter v n)
    (cond
     ((= 0 (remainder v 3)) (iter (/ v 3) (+ 1 n)))
     (else n)))
  (iter z 0))

(newline)
(display (car (cons 1 2))) ;; 1
(newline)
(display (cdr (cons 1 2))) ;; 2
