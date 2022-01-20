;; 2.18
;;

(define (reverse-iter l)
  (define (iter rest acc)
    (if (null? rest)
        acc
        (iter (cdr rest) (cons (car rest) acc))))
  (iter l ()))

;; This is actually cheating as we have used append
;; which appends two lists.
;; To do it with cons you would need to write an append
;; function using cons.
;; The iterative function is definitely better
(define (reverse l)
  (if (null?  l)
      l
      (append (reverse (cdr l)) (list (car l)))))

(newline)
(display "2.18")
(newline)
(display "iterative")
(newline)
(display (reverse-iter (list 1 2 3 4 5))) ;; (5 4 3 2 1)
(newline)
(display (reverse-iter (list 1))) ;; (1)
(newline)
(display "recursive")
(newline)
(display (reverse (list 1 2 3 4 5))) ;; (5 4 3 2 1)
(newline)
(display (reverse (list 1))) ;; (1)
(newline)
