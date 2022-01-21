;; 2.29
;;

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (total-weight mobile)
  (define (descend-branch branch)
    (if (pair? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))
  (cond
   ((null? mobile) 0)
   (else (+ (descend-branch (left-branch mobile))
      (descend-branch (right-branch mobile))))))

(define (balanced? mobile)
  ;; length * weight
  (define (torque branch)
    (if (pair? (branch-structure branch))
        ;; if the structure is a mobile multiply length with its total-weight
        (* (branch-length branch) (total-weight (branch-structure branch)))
        ;; else just multiply the length with the structure itself
        (* (branch-length branch) (branch-structure branch))))
  ;; check if a branch is balanced
  (define (balanced-branch branch)
    (if (pair? (branch-structure branch))
        (balanced? (branch-structure branch)) ;; if structure is a mobile check if it is balanced
        #t)) ;; if it is a weight then it is always balanced
  ;; main: a mobile is balanced if left branch torque = right branch torque and
  ;; both branches are also balanced
  (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
       (balanced-branch (left-branch mobile))
       (balanced-branch (right-branch mobile))))

(define balanced (make-mobile (make-branch 4 (make-mobile (make-branch 6 1) (make-branch 6 1)))
                        (make-branch 4 (make-mobile (make-branch 6 1) (make-branch 6 1)))))
(define unbalanced (make-mobile (make-branch 4 (make-mobile (make-branch 6 1) (make-branch 6 1)))
                        (make-branch 4 (make-mobile (make-branch 6 0) (make-branch 6 2)))))

(newline)
(display "2.29")
(newline)
(display (total-weight balanced)) ;; 4
(newline)
(display (total-weight unbalanced)) ;; 4
(newline)
(display (balanced? balanced)) ;; #t
(newline)
(display (balanced? unbalanced)) ;; #f
