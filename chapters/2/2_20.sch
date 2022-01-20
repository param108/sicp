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

(define (same-parity-rec . l)
  (define (iter rest predicate?)
    (cond ((null? rest) rest)
          ((predicate? (car rest)) (cons (car rest) (iter (cdr rest) predicate?)))
          (else (iter (cdr rest) predicate?))))
  (if (even? (car l))
      (iter l even?)
      (iter l odd?)))


(newline)
(display "2.20")
(newline)
(display (same-parity 1 2 3 4 5 6)) ;; 1 3 5
(newline)
(display (same-parity 2 3 4 5 6 7)) ;; 2 4 6
(newline)
(display (same-parity-rec 1 2 3 4 5 6)) ;; 1 3 5
(newline)
(display (same-parity-rec 2 3 4 5 6 7)) ;; 2 4 6
