;; 2.31
;;

(define (tree-map fn tree)
  (map (lambda (leaf)
         (if (pair? leaf)
             (tree-map fn leaf)
             (fn leaf))) tree))

(newline)
(display "2.31")
(newline)
(display (tree-map square (list 1
                                (list 2 (list 3 4) 5)
                                (list 6 7))))
