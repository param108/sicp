;; 2.36
;;

(define nil ())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


;; we accumulate the cars and join the result to the
;; accumulate-n of the cdrs of the sequences.
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs) )
            (accumulate-n op init  (map cdr seqs)))))

(display "2.36")
(newline)
(display (accumulate-n
          +
          0
          (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))) ;; (22 26 30)
