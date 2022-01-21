;; 2.33
;;

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; apply (p x) for each element of sequence
(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y) ) () sequence))

;; walk over seq1, adding it to seq2
;; the initial values is thus seq2
(define (append seq1 seq2)
  (accumulate cons
              seq2 seq1))

(define (length sequence)
  (accumulate
   (lambda (x y)
     (+ 1 y)) 0 sequence))


(newline)
(display "2.33")
(newline)
(display (map (lambda (x) (* x x)) (list 1 2 3 4))) ;; (1 4 9 16)
(newline)
(display (append (list 1 2 3 4) (list 5 6 7 8))) ;; (1 2 3 4 5 6 7 8)
(newline)
(display (length (list 1 2 3 4 5 6 7 8)))
