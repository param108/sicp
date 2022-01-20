;; 2.27
;;

(define (deep-reverse l)
  (if
   (null? l) l
   (append
    (deep-reverse (cdr l))
    (list (if (pair? (car l))
              (deep-reverse (car l))
              (car l))))))

(display "2.27")
(newline)
(display (deep-reverse (list 1 2 3 4 5))) ;; (5 4 3 2 1)
(newline)
(display (deep-reverse (list 1 (list 1 2) 2))) ;; (2 (2 1) 1)
(newline)
(display (deep-reverse (list (list 1 2) (list 3 4)))) ;; ((4 3) (2 1))
