;; 2.32
;;

;; basically subsets are all subsets with the first element +
;; the subsets without the first element
(define (subsets s)
  (if (null? s)
      (list ())
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (x)
                        (cons (car s) x)) rest)))))

(display "2.32")
(newline)
(display (subsets (list 1 2 3))) ;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
