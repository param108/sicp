;; 2.35
;;

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; count-leaves returns the number of leaves in a tree
(define (count-leaves t)
  (accumulate
   ;; the map function used to generate the sequence
   ;; replaces each leaf with 1 and each sub-list with
   ;; the number of leafs in that sub-list
   ;; for example (a (b c)) => (1 2)
   ;; The op then should just add these values
   (lambda (x y)
     (+ x y))
   0
   (map (lambda (x)
          (if (pair? x)
              (count-leaves x)
              1))
        t)))

(newline)
(display "2.35")
(newline)
(display (count-leaves (list 1 (list 2 3) 4 (list 5 (list 6 7))))) ;; 7
(newline)
(display (count-leaves (cons (list 1 2) (list 3 4)))) ;; 4
