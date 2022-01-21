;; 2.34
;;

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(newline)
(display "2.34")
(newline)
;; 1 + 2x + 3 x^2; if x = 5 => 86
(display (horner-eval 5 (list 1 2 3)))
