;; 2.29(d)
;;

(load "2_29.sch")

;; overwriting the definitions as mentioned in (d) of the question
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define balanced (make-mobile (make-branch 4 (make-mobile (make-branch 6 1) (make-branch 6 1)))
                        (make-branch 4 (make-mobile (make-branch 6 1) (make-branch 6 1)))))
(define unbalanced (make-mobile (make-branch 4 (make-mobile (make-branch 6 1) (make-branch 6 1)))
                        (make-branch 4 (make-mobile (make-branch 6 0) (make-branch 6 2)))))

(newline)
(display "2.29(d)")
(newline)
(display (total-weight balanced)) ;; 4
(newline)
(display (total-weight unbalanced)) ;; 4
(newline)
(display (balanced? balanced)) ;; #t
(newline)
(display (balanced? unbalanced)) ;; #f


;; no change in total-weight or balanced?
