;; 2.42
;;

(define nil ())

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval i j)
  (define (iter x output)
    (cond
     ((<= x j) (iter (+ 1 x) (append output (list x))))
     (else output)))
  (iter i nil))


(define (queens board-size)
  ;; The board will just be a list of queen positions found so far.
  (define empty-board ())

  ;; Each queen position is just a list of (row col)
  (define (make-queen row col)
    (list row col))

  ;; selectors
  (define (queen-row q)
    (car q))

  (define (queen-col q)
    (cadr q))

  ;; adjoin-position: just add a queen to a board
  (define (adjoin-position row col board)
    (append (list (make-queen row col)) board))

  (define (safe? col board)
    ;; the way we do adjoin-position means (car board)
    ;; is always the new queen postion
    (define (new-row)
      (queen-row (car board)))

    (define (new-col)
      (queen-col (car board)))

    (define (any pred)
      (> (accumulate
          +
          0
          (map pred
               (cdr board)))
         0))

    ;; if another queen shares the row number
    ;; then the new queen cannot be placed here.
    ;; this function will return true if this is the case
    (define (same-row q)
      (if (= (new-row) (queen-row q)) 1 0))

    ;; if another queen shares the column number
    ;; then the new queen cannot be placed here.
    ;; this function will return true if this is the case
    (define (same-col q)
      (if (= (new-col) (queen-col q)) 1 0))

    ;; if another queen is equidistant in both row and column number
    ;; then the new queen cannot be placed here
    ;; this function will return true if this is the case
    (define (same-diag q)
      (if (=
           (abs (- (new-col) (queen-col q)))
           (abs (- (new-row) (queen-row q))))
                      1 0))
    ;; safe? main
    ;; not same-row or same-col or same-diag
    (if (null? board)
        #t ;; if the board is nil, position is safe.
        (not (boolean/or (any same-row) (any same-col) (any same-diag))))
    );; safe?
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))

  ;; queens:main
  (queen-cols board-size)) ;; queens
;;

(newline)
(display "2.42")
(newline)
(display (queens 1))
(newline)
(display (queens 2))
(newline)
(display (queens 3))
(newline)
(display (queens 4))
(newline)
(display (queens 5))
(newline)
(display (queens 8))
