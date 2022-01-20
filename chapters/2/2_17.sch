;; 2.17
;;

(define (last-pair l)
  (cond ((null? l) l) ;; this is an error, but we return null
        ((null? (cdr l)) l)
        (else (last-pair (cdr l)))))

(newline)
(display "2.17")
(newline)
(display (last-pair (list 23 72 149 34))) ;; (34)
(newline)
(display (last-pair (list 1))) ;; (1)
(newline)
