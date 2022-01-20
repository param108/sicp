;; 2.19
;;

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


(define (except-first-denomination values)
  (cdr values))

(define (first-denomination values)
  (car values))

(define (no-more? values)
  (null? values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(newline)
(display "2.19")
(newline)
(display (cc 100 us-coins)) ;; 292
(newline)
(display (cc 100 uk-coins)) ;; 104561
(newline)

;; The ordering of the numbers doesnt matter because
;; The tree covers all possible combinations of the coin values.
;; All combinations can be considered as either including a coin or not including the coin.
;; The tree thus considers all combinations
