;; 2.39
;;

(define nil ())

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (reverse-right sequence)
  (fold-right (lambda (x y)
                (append y (list x))) nil sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y)
               (append (list y) x) ) nil sequence))

(newline)
(display "2.39")
(newline)
(display (reverse-right (list 1 2 3 4))) ;; ( 4 3 2 1 )
(newline)
(display (reverse-left (list 1 2 3 4))) ;; (4 3 2 1 )
