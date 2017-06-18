(define (add-poly a b)
  (cond
    ((> (length a) (length b))
     (add-poly a (cons 0 b)))
    ((< (length a) (length b))
     (add-poly (cons 0 a) b))
    (else (map + a b))))
(define (mult-add-poly a b r)
  (if (null? a) r
    (let ((s (map (lambda (c) 0) (cdr a))))
      (for-each
        (lambda (c)
          (set! s (cons (* (car a) c) s)))
        (reverse b))
      (mult-add-poly (cdr a) b (add-poly r s)))))
(define (mult-poly a b)
  (cond
    ((null? a) b)
    ((null? b) a)
    (else (mult-add-poly a b '()))))
(define (omit-column m c)
  (map
    (lambda (row)
      (if (zero? c) (cdr row)
        (let* ((r (map (lambda (c) c) row))
               (x (list-tail r (- c 1))))
          (set-cdr! x (cddr x))
          r)))
    m))
(define (determinant m)
  (if (= (length m) 1) (caar m)
    (let ((neg #f) (column 0) (result '()))
      (for-each
        (lambda (p)
          (if neg (set! p (map - p)))
          (set! result
            (add-poly result
              (mult-poly p
                (determinant
                  (omit-column (cdr m) column)))))
          (set! neg (not neg))
          (set! column (+ 1 column)))
        (car m))
      result)))
(define (eval-poly-loop p x)
  (if (null? p) 0
    (+ (car p) (* x (eval-poly-loop (cdr p) x)))))
(define (eval-poly p x)
  (eval-poly-loop (reverse p) x))
(define (resultant-matrix p1 p2)
  (let ((r '())
        (z1r '())
        (z2r '())
        (z1l (map (lambda (x) '(0)) (cdr p2)))
        (z2l (map (lambda (x) '(0)) (cdr p1))))
    (for-each
      (lambda (x)
        (set! r (cons (append (cdr z2l) p2 z2r) r))
        (set! z2r (cons (car z2l) z2r))
        (set! z2l (cdr z2l)))
      (cdr p1))
    (for-each
      (lambda (x)
        (set! r (cons (append (cdr z1l) p1 z1r) r))
        (set! z1r (cons (car z1l) z1r))
        (set! z1l (cdr z1l)))
      (cdr p2))
    r))
