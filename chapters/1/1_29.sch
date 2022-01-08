;; 1.29
;;

(define (simpsons f a b n)
  ;; integral f over a-b in n steps
  ;; coeff returns the coefficient in simpsons rule
  ;; y0+4y1+2y2+4y3+2y4....4y(n - 1)+yn
  (define (coeff idx)
    (cond
     ((= idx 0) 1)
     ((= idx n) 1)
     ((= (remainder idx 2) 0) 4)
     (else 2)))

  ;; value returns coeff(idx)*y(idx)
  ;; y(idx) = f(a + idx*h)
  ;; e.g 1y0, 4y1 etc
  (define (value idx)
    (* (coeff idx) (f (+ a (* idx (hval))))))

  ;;sum sum of simpsons
  (define (sum idx val)
    (cond
     ((> idx n) val)
     (else (sum (+ 1 idx) (+ val (value idx))))))

  ;; hval calculate the h value
  (define (hval)
    (/ (- b a) n))

  ;; main (h/3)*(simpsons sum)
  (* (/ (hval) 3) (sum 0 0))
)

(define (cube x) (* x x x))

;; main
(newline)
(display (exact->inexact (simpsons cube 0 1 100))) ;;.24671666666666667
(newline)
(display (exact->inexact (simpsons cube 0 1 1000)));;.24966716666666666

;; exact value is 24 (integral is x^4/4)
