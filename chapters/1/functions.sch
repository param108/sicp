(define (square x) (* x x))

;; 1.2


(define (onedottwo) (/ (+ 5
      (+ 4
         (- 2
            (- 3
               (+ 6
                  (/ 4 5))))))
   (* 3 (* (- 6 2) (- 2 7)))))

(define (>= x y)
  (or (> x y) (= x y)))

(define (sumsquare x y)
  (+ (square x) (square y)))

(define (max x y)
  (if (> x y)
      x y))

(define (min x y)
  (if (< x y)
      x y))

(define (max_of_3 x y z)
  (max x (max y z)))

(define (min_of_3 x y z)
  (min x (min y z)))

(define (max2 x y z)
  (let ((maxo3 (max_of_3 x y z))
        (mino3 (min_of_3 x y z)))
    (list
     maxo3
     (- (+ x y z) maxo3 mino3))))

(define (onedotthree x y z)
  (let ((m3 (max2 x y z)))
    (sumsquare (car m3) (cadr m3))))

(define (abs x)
  (if (< x 0) (- 0 x) x))

(define (good-enough? g1 g2)
  (< (abs (- g1 g2)) 0.00001))

(define (avg x y)
  (/ (+ x y) 2))

(define (improve x g1)
  (avg g1 (/ x g1)))

(define (sqrt-impl x g1 g2)
  (cond ((= x 0) 0)
        ((< x 0) -1)
        (else (if (good-enough? g1 g2)
                  g2 (sqrt-impl x g2 (improve x g2))))))

(define (sqrt x)
  (sqrt-impl x (/ x 2) (improve x (/ x 2))))

(define (onedotseven x)
  (sqrt x))

(define (improvecube x g1)
  (/ (+ (/ x (* g1 g1)) (* 2 g1)) 3))

(define (cubert-impl x g1 g2)
  (cond ((= x 0) 0)
        (else (if (good-enough? g1 g2)
                  g2 (cubert-impl x g2 (improvecube x g2))))))

(define (cubert x)
  (cubert-impl x (/ x 2) (improvecube x (/ x 2))))

(define (onedoteight x)
  (cubert x))
