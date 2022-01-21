;; 2.30
;;

(define (square-tree x)
  (cond
   ((null? x) x)
   ((pair? (car x))
    (append (list (square-tree (car x))) (square-tree (cdr x))))
   (else
    (cons (square (car x)) (square-tree (cdr x))))))

(define (square-tree-map tree)
  (map (lambda (x)
         (if (pair? x)
             (square-tree-map x)
             (square x))) tree))

(newline)
(display "2.30")
(newline)
(display (square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))) ;; (1 (4 (9 16) 25) (36 49))
(newline)
(display (square-tree-map
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))) ;; (1 (4 (9 16) 25) (36 49))
