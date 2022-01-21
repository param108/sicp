;; 2.28
;;

(define (fringe x)
  (cond
   ((null? x) x) ;; end of list
   ((pair? (car x)) ;; if x is a list recurse
    (append (fringe (car x)) (fringe (cdr x))))
   (else
    (cons (car x) (fringe (cdr x))))))

(define x (list (list 1 2) (list 3 4)))
(newline)
(display "2.28")
(newline)
(display (fringe x))
(newline)
(display (fringe (list x x)))
