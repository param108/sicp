;; 2.20
;;

(define (same-parity . l)
  (define (iter rest acc predicate?)
    (if (null? rest)
        acc
        (if (predicate? (car rest))
           (iter (cdr rest) (append acc (cons (car rest) ())) predicate?)
           (iter (cdr rest) acc predicate?))))
  (if (even? (car l))
      (iter l () even?)
      (iter l () odd?)))


(newline)
(display "2.20")
(newline)
(display (same-parity 1 2 3 4 5 6))
(newline)
(display (same-parity 2 3 4 5 6 7))
(newline)
