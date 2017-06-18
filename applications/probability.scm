(define (factorial n)
  (if (zero? n) 1
    (* n (factorial (- n 1)))))
(define (permutations n k)
  (if (zero? k) 1
    (* n (permutations (- n 1) (- k 1)))))
(define (choose n k)
  (quotient (permutations n k) (factorial k)))
(define (binomial n k p)
  (* (choose n k) (expt p k) (expt (- 1 p) (- n k))))
(define (binomial-range n ks ke p)
  (if (> ks ke) 0
    (+ (binomial n ks p)
       (binomial-range n (+ 1 ks) ke p))))
(define (fz z)
  (/ (expt e (* -1/2 z z)) (sqrt (* 2 pi))))
