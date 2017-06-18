(define (cursor thunk table result)
  (if (null? table) result
    (let ((d (apply thunk (car table))))
      (cursor thunk (cdr table)
              (if d (cons d result) result)))))
(define (cursor! thunk table count)
  (if (null? table) count
    (let ((d (apply thunk (car table))))
      (cond
        (d (set-car! table d)
           (cursor! thunk (cdr table) (+ 1 count)))
        (else (cursor! thunk (cdr table) count))))))
(define (table name) (assv name tables))
(define fields cadr)
(define defaults caddr)
(define select
  (macro (t w . s)
    (let ((columns (fields (table t))))
      `(cursor
         (lambda ,columns
           (if ,w (list ,@(if (null? s) columns s))))
         ,t '()))))
(define update
  (macro (t w . s)
    (let ((columns (fields (table t))))
      `(cursor!
         (lambda ,columns
           (cond
             (,w ,@s (list ,@columns))
             (else #f)))
         ,t 0))))
(define delete
  (macro (t w)
    (let ((columns (fields (table t))))
      `(begin
         (set! ,t
           (cursor
             (lambda ,columns
               (if (not ,w) (list ,@columns)))
             ,t '()))
         #t))))
(define insert
  (macro (t f . s)
    (let* ((tab (table t))
           (c (fields tab))
           (v (defaults tab)))
      `(set! ,t
         (cons
           ((lambda ,c
              ((lambda ,f (list ,@c))
               ,@s))
            ,@v)
           ,t)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tables
  '((tables
      (name columns defaults)
      (#f #f #f))
    (places
      (city state)
      (#f #f))))
(define places
  '(("Bedford" "Texas")
    ("Denton" "Texas")
    ("Boston" "Massachusetts")))
