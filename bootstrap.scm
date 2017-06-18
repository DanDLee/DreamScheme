;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This file is meant to be loaded only once!
;;;;;;;;;;;;;;;;;;;;
;;;Library Syntax;;;
;;;;;;;;;;;;;;;;;;;;
((lambda ()
  (define %define define)
  (define %let #f)
  (define %letrec #f)
  (define %sym% (string->symbol ""))
  (set! quasiquote
    (macro (x)
      (%define quasiquote-nested
        (lambda (n x)
          (if (pair? x)
            (if (eq? (car x) 'quasiquote)
              (list list '(quote quasiquote)
                (quasiquote-nested (+ n 1) (cadr x)))
              (if (eq? (car x) 'unquote)
                (if (zero? n)
                  (cadr x)
                  (list list '(quote unquote)
                    (quasiquote-nested (- n 1) (cadr x))))
                (if (pair? (car x))
                  (if (eq? (caar x) 'unquote-splicing)
                    (if (zero? n)
                      (list append
                        (cadar x)
                        (quasiquote-nested n (cdr x)))
                      (list cons
                        (quasiquote-nested (- n 1) (car x))
                        (quasiquote-nested n (cdr x))))
                    (list cons
                      (quasiquote-nested n (car x))
                      (quasiquote-nested n (cdr x))))
                  (list cons
                    (quasiquote-nested n (car x))
                    (quasiquote-nested n (cdr x))))))
            (if (vector? x)
              (list list->vector
                (quasiquote-nested n (vector->list x)))
              (list quote x)))))
      (quasiquote-nested 0 x)))
  (set! %let
    (macro x
     `((,lambda
         ,(map car (car x))
         ,@(cdr x))
       ,@(map cadr (car x)))))
  (set! %letrec
    (macro x
     `((,lambda ()
         ,@(map
             (lambda (b) (cons %define b))
             (car x))
         ,@(cdr x)))))
  (set! define
    (macro (f . b)
      (if (pair? f)
        `(,%define ,(car f)
           (,lambda ,(cdr f)
             ,@b))
        `(,%define ,f ,@b))))
  (set! letrec %letrec)
  (set! let
    (macro x
      (if (symbol? (car x))
       `(,%letrec
          ((,(car x)
             (,lambda ,(map car (cadr x))
                ,@(cddr x))))
          (,(car x) ,@(map cadr (cadr x))))
       `(,%let ,@x))))
  (set! let*
    (macro x
      (if (null? (car x))
       `(,%let ,@x)
       `(,%let (,(caar x))
          (,let*
           ,(cdar x)
           ,@(cdr x))))))
  (set! or
    (macro x
      (if (null? x)
        #f
        (let ((temp %sym%))
         `(,%let
            ((,temp ,(car x)))
            (,if ,temp ,temp
              (,or ,@(cdr x))))))))
  (set! and
    (macro x
      (if (null? x)
        #t
        (if (null? (cdr x))
          (car x)
         `(,if ,(car x) (,and ,@(cdr x)))))))
  (set! cond
    (macro x
      (if (null? x)
        #f
        (if (eq? (caar x) 'else)
          (if (null? (cdar x))
            #f
            (if (null? (cddar x))
              (cadar x)
             `(begin ,@(cdar x))))
          (if (null? (cdar x))
           `(,%let
              ((temp ,(caar x)))
              (,if temp temp
                (,cond ,@(cdr x))))
            (if (eq? (cadar x) '=>)
             `(,%let
                ((temp ,(caar x)))
                (,if temp
                  (,(caddar x) temp)
                  (,cond ,@(cdr x))))
             `(,if
               ,(caar x)
               ,(if (null? (cdar x))
                  #f
                  (if (null? (cddar x))
                    (cadar x)
                   `(,begin ,@(cdar x))))
                (,cond ,@(cdr x)))))))))
  (set! case
    (macro x
      (if (pair? (car x))
       `(,%let
          ((,%sym% ,(car x)))
          (,case
            ,%sym%
            ,@(cdr x)))
        (if (null? (cdr x))
          #f
          (if (eq? 'else (caadr x))
           `(,begin ,@(cdadr x))
           `(,if
              (,memv ,(car x)
                (,quote ,(caadr x)))
              (,begin ,@(cdadr x))
              (,case ,(car x)
                ,@(cddr x))))))))
  (set! do
    (macro (v t . c)
     `(,%letrec
        ((,%sym%
           (,lambda ,(map car v)
             (,if ,(car t)
               (,begin ,@(cdr t))
               (,begin
                 ,@c
                 (,%sym%
                   ,@(map
                       (lambda (e)
                         (%let ((s (cddr e)))
                           (if (null? s) (car e) (car s))))
                       v)))))))
        (,%sym% ,@(map cadr v)))))))

;;;;;;;;;;;;;;;;;;;;;
;;; Numeric Tower ;;;
;;;;;;;;;;;;;;;;;;;;;
(define *R* (cons (/ -1 0) (/ 1 0)))
(set! <
  (macro x
    (cons > (reverse x))))
(set! <=
  (macro x
    (cons >= (reverse x))))
(set! lcm
  (lambda x
    (if (null? x) 1
      (let* ((a (apply lcm (cdr x)))
             (b (* (car x) a)))
        (if (zero? b) 0
          (abs (quotient b (gcd (car x) a))))))))
(vector-set! (tower-procedures) 0 ; +
  (lambda (a b)
    (cond
      ((and (rational? a) (rational? b))
       (let ((d (lcm (denominator a) (denominator b))))
         (let ((n (+ (* (numerator a) (quotient d (denominator a)))
                     (* (numerator b) (quotient d (denominator b))))))
           (/ n d))))
      ((and (rational? a) (real? b))
       (make-real
         (lambda (k)
           (let ((r (b k)))
             (cons
               (+ a (car r))
               (+ a (cdr r)))))))
      ((and (real? a) (rational? b))
       (make-real
         (lambda (k)
           (let ((r (a k)))
             (cons
               (+ (car r) b)
               (+ (cdr r) b))))))
      ((and (real? a) (real? b))
       (make-real
         (lambda (k)
           (let ((ra (a k))
                 (rb (b k)))
             (cons
               (+ (car ra) (car rb))
               (+ (cdr ra) (cdr rb)))))))
      (else 
       (make-rectangular
         (+ (real-part a) (real-part b))
         (+ (imag-part a) (imag-part b)))))))
(let ((prim/ /))
  (set! /
    (lambda (a . b)
      (cond
        ((pair? b)
         (* a (/ (apply * b))))
        ((rational? a)
         (prim/ (denominator a) (numerator a)))
        ((real? a)
         (make-real
           (lambda (k)
             (let ((r (a k)))
               (cond
                 ((or
                    (and (positive? (car r)) (positive? (cdr r)))
                    (and (negative? (car r)) (negative? (cdr r))))
                  (cons (/ (cdr r)) (/ (car r))))
                 (else *R*))))))
        (else
         (let ((x (real-part a))
               (y (imag-part a)))
           (let ((d (+ (* x x) (* y y))))
             (make-rectangular (/ x d) (- (/ y d)))))))))
  (vector-set! (tower-procedures) 1 ; -
    (lambda (a)
      (cond
        ((rational? a)
         (prim/ (- (numerator a))
                (denominator a)))
        ((real? a)
         (make-real
           (lambda (k)
             (let ((r (a k)))
               (cons
                 (- (cdr r))
                 (- (car r)))))))
        (else
         (make-rectangular
           (- (real-part a))
           (- (imag-part a)))))))
  (vector-set! (tower-procedures) 2 ; *
    (lambda (a b)
      (cond
        ((and (rational? a) (rational? b))
         (let ((ga (gcd (numerator a) (denominator b)))
               (gb (gcd (denominator a) (numerator b))))
           (let ((n (* (quotient (numerator a) ga)
                       (quotient (numerator b) gb)))
                 (d (* (quotient (denominator a) gb)
                       (quotient (denominator b) ga))))
             (prim/ n d))))
        ((and (rational? a) (real? b))
         (make-real
           (lambda (k)
             (let* ((r (b k))
                    (rat (* a (car r)))
                    (rau (* a (cdr r))))
               (if (<= rat rau)
                 (cons rat rau)
                 (cons rau rat))))))
        ((and (real? a) (rational? b))
         (make-real
           (lambda (k)
             (let* ((r (a k))
                    (rar (* (car r) b))
                    (ras (* (cdr r) b)))
               (if (<= rar ras)
                 (cons rar ras)
                 (cons ras rar))))))
        ((and (real? a) (real? b))
         (make-real
           (lambda (k)
             (let* ((ra (a k))
                    (rb (b k))
                    (rrt (* (car ra) (car rb)))
                    (rru (* (car ra) (cdr rb)))
                    (rst (* (cdr ra) (car rb)))
                    (rsu (* (cdr ra) (cdr rb))))
               (cons
                 (min rrt rru rst rsu)
                 (max rrt rru rst rsu))))))
        (else
         (make-rectangular
           (- (* (real-part a) (real-part b))
              (* (imag-part a) (imag-part b)))
           (+ (* (real-part a) (imag-part b))
              (* (imag-part a) (real-part b)))))))))
(define *epsilon* 1/1000000000)
(letrec
  ((rational->real
     (lambda (r)
       (make-real
         (lambda (k) (cons r r)))))
   (epsilon=
     (lambda (a b k)
       (let ((aa (a k))
             (bb (b k)))
         (cond
           ((or (> (car aa) (cdr bb))
                (> (car bb) (cdr aa)))
            #f)
           ((or (> (- (cdr aa) (car aa)) *epsilon*)
                (> (- (cdr bb) (car bb)) *epsilon*))
            (epsilon= a b (+ k 1)))
           (else #t)))))
   (epsilon>
     (lambda (a b k d)
       (let ((aa (a k))
             (bb (b k)))
         (cond
           ((> (car aa) (cdr bb)) #t)
           ((> (car bb) (cdr aa)) #f)
           ((or (> (- (cdr aa) (car aa)) *epsilon*)
                (> (- (cdr bb) (car bb)) *epsilon*))
            (epsilon> a b (+ k 1) d))
           (else d))))))
  (vector-set! (tower-procedures) 3 ; =
    (lambda (a b)
      (cond
        ((and (rational? a) (rational? b))
         (and (= (numerator a) (numerator b))
              (= (denominator a) (denominator b))))
        ((and (rational? a) (real? b))
         (epsilon= (rational->real a) b 1))
        ((and (real? a) (rational? b))
         (epsilon= a (rational->real b) 1))
        ((and (real? a) (real? b))
         (epsilon= a b 1))
        (else (and (= (real-part a) (real-part b))
                   (= (imag-part a) (imag-part b)))))))
  (vector-set! (tower-procedures) 4 ; >
    (lambda (a b)
      (cond
        ((and (rational? a) (rational? b))
         (let ((cd (lcm (denominator a) (denominator b))))
           (> (* (numerator a) (quotient cd (denominator a)))
              (* (numerator b) (quotient cd (denominator b))))))
        ((and (rational? a) (real? b))
         (epsilon> (rational->real a) b 1 #f))
        ((and (real? a) (rational? b))
         (epsilon> a (rational->real b) 1 #f))
        ((and (real? a) (real? b))
         (epsilon> a b 1 #f))
        (else (error "Complex numbers not yet supported.")))))
  (vector-set! (tower-procedures) 5 ; >=
    (lambda (a b)
      (cond
        ((and (rational? a) (rational? b))
         (let ((cd (lcm (denominator a) (denominator b))))
           (>= (* (numerator a) (quotient cd (denominator a)))
               (* (numerator b) (quotient cd (denominator b))))))
        ((and (rational? a) (real? b))
         (epsilon> (rational->real a) b 1 #t))
        ((and (real? a) (rational? b))
         (epsilon> a (rational->real b) 1 #t))
        ((and (real? a) (real? b))
         (epsilon> a b 1 #t))
        (else (error "Complex numbers not yet supported."))))))
(letrec
  ((sync-endpoints
     (lambda (thunk r k)
       (let* ((x (r k))
              (a (thunk (car x)))
              (b (thunk (cdr x))))
         (if (= a b) a (sync-endpoints thunk r (+ k 1)))))))
  (vector-set! (tower-procedures) 6 ; round
    (lambda (a)
      (cond
        ((rational? a)
         (let ((d (denominator a))
                (n (* 2 (abs (remainder (numerator a) (denominator a))))))
           (let ((q (quotient (numerator a) d)))
             (if (negative? (numerator a))
               (cond
                 ((> n d) (- q 1))
                 ((= n d) (if (even? q) q (- q 1)))
                 (else q))
               (cond
                 ((> n d) (+ q 1))
                 ((= n d) (if (even? q) q (+ q 1)))
                 (else q))))))
        ((real? a) (sync-endpoints round a 1))
        (else (error "Complex numbers not supported.")))))
  (vector-set! (tower-procedures) 7 ; truncate
    (lambda (a)
      (cond
        ((rational? a)
         (quotient (numerator a) (denominator a)))
        ((real? a) (sync-endpoints truncate a 1))
        (else (error "Complex numbers not supported.")))))
  (vector-set! (tower-procedures) 8 ; floor
    (lambda (a)
      (cond
        ((rational? a)
         (if (negative? (numerator a))
           (- (quotient (numerator a) (denominator a)) 1)
           (quotient (numerator a) (denominator a))))
        ((real? a) (sync-endpoints floor a 1))
        (else (error "Complex numbers not supported."))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; R5RS LIBRARY AND OPTIONAL PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define force
  (lambda (object)
    (object)))
(define delay #f)
(letrec
  ((make-promise
     (lambda (proc)
       (let ((result-ready? #f)
             (result #f))
         (lambda ()
           (if result-ready?
             result
             (let ((x (proc)))
               (if result-ready?
                 result
                 (begin
                   (set! result-ready? #t)
                   (set! result x)
                   result)))))))))
  (set! delay
    (macro (expr)
      `(,make-promise (,lambda () ,expr)))))
(define (expt x n)
  (letrec
    ((power-loop
       (lambda (n y z)
         (cond
           ((even? n)
            (power-loop (quotient n 2) y (* z z)))
           ((= n 1)
            (* y z))
           (else
            (power-loop (quotient n 2) (* y z) (* z z))))))
     (approx-root
       (lambda (r x y n iters)
         (if (zero? iters) (cons x y)
           (let ((m (/ (+ x y) 2)))
             (if (< (power-loop r 1 m) n)
               (approx-root r m y n (- iters 1))
               (approx-root r x m n (- iters 1)))))))
     (root-loop
       (lambda (r a b n)
         (let* ((m (quotient (+ a b) 2))
                (g (power-loop r 1 m)))
           (if (= g n) m
             (cond
               ((= m a)
                (let ((x (exact->inexact a))
                      (y (exact->inexact b)))
                  (make-real
                    (lambda (iters)
                      (approx-root r x y n iters)))))
               ((< g n) (root-loop r m b n))
               (else (root-loop r a m n))))))))
    (cond
      ((negative? n)
       (/ 1 (expt x (abs n))))
      ((zero? n) 1)
      ((zero? x) 0)
      ((integer? n)
       (power-loop n 1 x))
      ((rational? n)
       (cond
         ((integer? x)
          (let ((y (power-loop (numerator n) 1 x)))
            (root-loop (denominator n) 1 y y)))
         ((rational? x) (/ (expt (numerator x) n) (expt (denominator x) n)))
         (else (error "Complex or Real numbers not yet supported."))))
      (else
       (error "Complex numbers not yet supported.")))))
(define (sqrt x) (expt x 1/2))
(define (sin x)
  (define (loop n)
    (if (zero? n)
      (cons 0 x)
      (let ((r (loop (- n 1))))
        (cons
          (+ (car r) (cdr r))
          (/ (- (* (cdr r) x x)) (+ n n) (+ n n 1))))))
  (cond
    ((zero? x) 0)
    ((rational? x)
     (make-real
       (lambda (n)
         (let ((r (loop n)))
           (if (negative? (cdr r))
             (cons (+ (car r) (cdr r)) (car r))
             (cons (car r) (+ (car r) (cdr r))))))))
    (else (error "Complex or Real number not yet supported."))))
(define (cos x)
  (define (loop n)
    (if (zero? n)
      (cons 0 1)
      (let ((r (loop (- n 1))))
        (cons
          (+ (car r) (cdr r))
          (/ (- (* (cdr r) x x)) (+ n n) (+ n n -1))))))
  (cond
    ((zero? x) 1)
    ((rational? x)
     (make-real
       (lambda (n)
         (let ((r (loop n)))
           (if (negative? (cdr r))
             (cons (+ (car r) (cdr r)) (car r))
             (cons (car r) (+ (car r) (cdr r))))))))
    (else (error "Complex or Real number not yet supported."))))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; EXTRA PROCEDURES ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
(define (rational->string f base precision)
  (let* ((q (quotient (numerator f) (denominator f)))
         (df (round (* (expt base precision) (- (abs f) (- (abs q) 1)))))
         (o (number->string df base)))
    (string-set! o 0 #\.)
    (string-append (number->string q base) o)))
(define *required* '())
(define (require f)
  (if (member f *required*) #f
    (begin
      (set! *required* (cons f *required*))
      (load f))))
