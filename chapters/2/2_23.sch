;; 2.23
;;

(define (for-each fn l)
  (if (null? l)
      #t
      (begin
        (fn (car l))
        (for-each fn (cdr l)))))

(newline)
(display "2.23")
(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))
