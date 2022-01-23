;; 2.37
;;

(define nil ())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


;; we accumulate the cars and join the result to the
;; accumulate-n of the cdrs of the sequences.
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs) )
            (accumulate-n op init  (map cdr seqs)))))

;; ri = vj*wj (i here is the row number, j is the column number )
;; both v and w are one-dimensional matrices
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

;; m is  ixj matrix and v is j size matrix
;; e.g. (1 2 3) <- v
;;     ((1 2 3)
;;      (4 5 6)) <- m
(define (matrix-*-vector m v)
  (map (lambda (x)
         (dot-product x v)) m))

;; create lists of the cars and join them.
(define (transpose mat)
  (accumulate-n cons nil mat))

;; first transpose the second matrix so that
;; the previous columns are now the rows
;; then we dot-product each row of this new matrix
;; with the rows of the first matrix forming a new matrix
;; as the result
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x)
           (matrix-*-vector cols x))
         m)))

(newline)
(display "2.37")
(newline)
(display (matrix-*-vector
          (list
           (list 1 2 3 4)
           (list 5 6 7 8)
           (list 9 10 11 12))
          (list 1 1 1 1))) ;; ( 10 26 42 )
(newline)
(display (transpose
          (list
           (list 1 2 3 4)
           (list 5 6 7 8)
           (list 9 10 11 12))));; ( ( 1 5 9 ) (2 6 10) (3 7 11) (4 8 12) )
(newline)
(display (matrix-*-matrix
          (list
           (list 1 2 3 4)
           (list 5 6 7 8)
           (list 9 10 11 12))
          (list
           (list 1 1 1)
           (list 1 1 1)
           (list 1 1 1)
           (list 1 1 1))))
