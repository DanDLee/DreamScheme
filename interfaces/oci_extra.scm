(define %lister-widths '())
(define %lister '())
(define lister-service #f)
(define lister-current '())
(define (display-field w d)
  (cond
    ((not d) (display-field w "#f"))
    ((>= (string-length d) w)
     (display (substring d 0 w))
     (write-char #\space))
    (else
     (display d)
     (display (make-string (- w (string-length d)) #\space))
     (write-char #\space))))
(define (lister-fields w d)
  (if (and (pair? w) (pair? d))
    (begin
      (display-field (car w) (car d))
      (lister-fields (cdr w) (cdr d)))))
(define (lister-row)
  (if (pair? lister-current)
    (begin
      (newline)
      (lister-fields %lister-widths (car lister-current)))))
(define (lister-first)
  (set! lister-current %lister))
(define (lister-next)
  (set! lister-current (cdr lister-current)))
(define (lister)
  (if (null? lister-current)
    (begin (newline) #t)
    (begin
      (lister-row)
      (set! lister-current (cdr lister-current))
      (lister))))
(define (%lister-set-widths! data widths)
  (if (pair? data)
    (begin
      (if (car data)
        (let ((w (string-length (car data))))
          (if (> w (car widths))
            (set-car! widths w))))
      (%lister-set-widths! (cdr data) (cdr widths)))))
(define (lister-set-widths!)
  (set! %lister-widths (map (lambda (x) 2) (car %lister)))
  (for-each
    (lambda (data)
      (%lister-set-widths! data %lister-widths))
    %lister))
(define (lister-set! results)
  (if (pair? results)
    (begin
      (set! %lister results)
      (lister-set-widths!))
    (set! %lister '()))
  (lister-first)
  (lister))
(define (s text . sizes)
  (lister-set! (apply select lister-service text sizes)))

(define (all_tables owner)
  (select lister-service
    (string-append
      "select owner, table_name
       from all_tables
       where owner='" owner "' order by table_name desc")
    50 50))

(define (all_views owner)
  (select lister-service
    (string-append
      "select owner, view_name
       from all_views
       where owner='" owner "'")
    50 50))

(define (all_ind_columns owner table_name)
  (select lister-service
    (string-append "select i.index_name, column_name
                    from all_indexes i
                    inner join all_ind_columns c on i.index_name=c.index_name
                    where i.owner='" owner "' and i.table_name='" table_name "'")
    50 50))

(define (all_tab_columns owner table)
  (select lister-service
    (string-append
      "select owner, table_name, column_name, data_type, data_length, data_precision, data_scale, nullable
       from all_tab_columns
       where owner='" owner "'
       and table_name='" table "' order by column_name desc")
    50 50 50 50 50 50 50 50))

(define (all_procedures owner)
  (select lister-service
    (string-append
      "select owner, object_name, procedure_name
       from all_procedures
       where owner='" owner "'")
    50 50 50))

(define (all-tables owner)
  (lister-set! (all_tables owner)))

(define (all-views owner)
  (lister-set! (all_views owner)))

(define (all-tab-columns owner table)
  (lister-set! (all_tab_columns owner table)))

(define (all-ind-columns owner table)
  (lister-set! (all_ind_columns owner table)))

(define (all-procedures owner)
  (lister-set! (all_procedures owner)))

(define (all-source owner type name file)
  (let
    ((out (open-output-file file))
     (text
       (select lister-service
         (string-append
      "select text
       from all_source
       where owner='" owner "'
       and type='" type "'
       and name='" name "'
       order by line desc") 4000)))
    (for-each (lambda (x) (display (car x) out)) text)
    (close-output-port out)))
