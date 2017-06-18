;Required: (define max-records 1000000)
(define canceled "Query exceeded maximum allowed record count and hence was canceled.")
(define display-progress? #t)
(define (display-progress y)
  (if (and display-progress?
        (zero? (modulo y 500)))
    (begin
      (if (zero? y) (display "<b>Rows Exported:</b> <input readonly=\"1\" id=\"progress\" />"))
      (update-progress y))))
(define (update-progress y)
  (display "<script type=\"text/javascript\">\n")
  (display "document.getElementById('progress').value=")
  (display y)
  (display ";\n</script>\n")
  (fcgx-flush))
(define (display-progress-complete y)
  (if display-progress?
    (begin
      (update-progress y)
      (display "<b>Click to download:</b> "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display HTML table ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (display-xml x out s e)
  (if (< s e)
    (let ((c (string-ref x s)))
      (display
        (case c
          ((#\<) "&lt;")
          ((#\>) "&gt;")
          ((#\&) "&amp;")
          (else c)) out)
      (display-xml x out (+ s 1) e))))
(define (display-th x out)
  (display "<th>" out)
  (display (if x x "&nbsp") out)
  (display "</th>" out))
(define (display-td d out)
  (display "<td>" out)
  (if d
    (if (positive? (string-length d))
      (if (char=? (string-ref d 0) #\<)
        (begin
          (display "<pre>")
          (display-xml d out 0 (string-length d))
          (display "</pre>"))
        (display d out))
      (display "&nbsp" out))
    (display "&nbsp" out))
  (display "</td>" out))
(define (display-row row y out)
  (display
    (if (even? y)
      "<tr class=\"row_even\">\n"
      "<tr class=\"row_odd\">\n")
    out)
  (for-each
    (lambda (x)
      (display-td x out))
    row)
  (display "</tr>\n" out))
(define (display-thead row-headers out)
  (display "<thead>\n<tr>\n" out)
  (for-each
    (lambda (x)
      (display-th x out))
    row-headers)
  (display "</tr>\n</thead>\n" out))
(define (loop-table c y out)
  (let ((row (c (>= y max-records))))
    (if row
      (begin
        (display-row row y out)
        (loop-table c (+ 1 y) out))
      (if (>= y max-records)
        (display canceled)))))
(define (display-tbody c out)
  (display "<tbody>\n" out)
  (loop-table c 0 out)
  (display "</tbody>\n" out))
(define (display-html-table row-headers c out)
  (display "<table cellspacing=\"0\" cellpadding=\"2\" class=\"results\" border=\"0\">\n" out)
  (display-thead row-headers out)
  (display-tbody c out)
  (display "</table>" out))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display CSV table ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(define (display-csv-row row o out)
  (if (null? row)
    (display
      (apply string-append
        (reverse (cons "\n" o)))
      out)
    (display-csv-row (cdr row)
      (if (car row)
        (cons "\"," (cons (car row) (cons "\"" o)))
        (cons "," o))
      out)))
(define (loop-csv c y out)
  (let ((row (c (>= y max-records))))
    (if row
      (begin
        (display-progress y)
        (display-csv-row row '() out)
        (loop-csv c (+ 1 y) out))
      (if (>= y max-records)
        (display canceled)
        (display-progress-complete y)))))
(define (display-csv-table row-headers c out progress?)
  (set! display-progress? progress?)
  (display-csv-row row-headers '() out)
  (loop-csv c 0 out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display Excel table ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (display-xmlss-row row out)
  (display 
    (apply string-append "<Row>"
      (map 
        (lambda (x)
          (if x
            (string-append "<Cell><Data ss:Type=\"String\">" x "</Data></Cell>")
            "<Cell><Data ss:Type=\"String\"></Data></Cell>"))
        row))
    out)
  (display "</Row>\n" out))
(define (loop-xmlss c y count out)
  (if (positive? count)
    (let ((row (c (>= y max-records))))
      (if row
        (begin
          (display-progress y)
          (display-xmlss-row row out)
          (loop-xmlss c (+ 1 y) (- count 1) out))
        (begin
          (if (>= y max-records)
            (display canceled)
            (display-progress-complete y))
          #f)))
    #t))
(define (display-xmlss-worksheets n y row-headers c out)
  (display "<Worksheet ss:Name=\"Sheet" out)
  (display n out)
  (display "\">\n<Table>" out)
  (display-xmlss-row row-headers out)
  (let ((more (loop-xmlss c y 65535 out)))
    (display "</Table>\n</Worksheet>\n" out)
    (if more (display-xmlss-worksheets (+ n 1) (+ y 65535) row-headers c out))))
(define (display-xmlss-workbook row-headers c out progress?)
  (set! display-progress? progress?)
  (display "<?xml version=\"1.0\"?>
<?mso-application progid=\"Excel.Sheet\"?>
<Workbook xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\"
 xmlns:o=\"urn:schemas-microsoft-com:office:office\"
 xmlns:x=\"urn:schemas-microsoft-com:office:excel\"
 xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\"
 xmlns:html=\"http://www.w3.org/TR/REC-html40\">
 <ExcelWorkbook xmlns=\"urn:schemas-microsoft-com:office:excel\" />" out)
  (display-xmlss-worksheets 1 0 row-headers c out)
  (display "</Workbook>" out))
