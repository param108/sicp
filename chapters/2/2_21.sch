;; 2.21
;;


(define (square-list items)
  (if (null? items)
      ()
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map (lambda (x) (* x x)) items))

(newline)
(display "2.21")
(newline)
(display (square-list (list 1 2 3 4 5)))
(newline)
(display (square-list-map (list 1 2 3 4 5)))
(newline)
