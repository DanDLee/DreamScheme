;(define seed 1985127)
(define (random m)
  (random32)
  (remainder seed m))
(define random32 #f)
(let
  ((MM 2147483647)
   (AA 48271)
   (QQ 44488)
   (RR 3399))
  (set! random32
    (lambda ()
      (set! seed
        (- (* AA (modulo seed QQ))
           (* RR (quotient seed QQ))))
      (if (negative? seed)
        (set! seed (+ seed MM))))))
