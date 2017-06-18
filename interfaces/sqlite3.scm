;;;;;;;;;;;;;;;;;;;
;;; Dll Modules ;;;
;;;;;;;;;;;;;;;;;;;

(define *sqlite3* (dlopen "sqlite3"))
(if (zero? *sqlite3*) (error "Sqlite3 library was not found"))

;;;;;;;;;;;;;;;;;
;;; Constants ;;;
;;;;;;;;;;;;;;;;;

(define SQLITE_OK           0)  ;;Successful result
(define SQLITE_ERROR        1)  ;;SQL error or missing database
(define SQLITE_INTERNAL     2)  ;;Internal logic error in SQLite
(define SQLITE_PERM         3)  ;;Access permission denied
(define SQLITE_ABORT        4)  ;;Callback routine requested an abort
(define SQLITE_BUSY         5)  ;;The database file is locked
(define SQLITE_LOCKED       6)  ;;A table in the database is locked
(define SQLITE_NOMEM        7)  ;;A malloc() failed
(define SQLITE_READONLY     8)  ;;Attempt to write a readonly database
(define SQLITE_INTERRUPT    9)  ;;Operation terminated by sqlite3_interrupt()
(define SQLITE_IOERR       10)  ;;Some kind of disk I/O error occurred
(define SQLITE_CORRUPT     11)  ;;The database disk image is malformed
(define SQLITE_NOTFOUND    12)  ;;NOT USED. Table or record not found
(define SQLITE_FULL        13)  ;;Insertion failed because database is full
(define SQLITE_CANTOPEN    14)  ;;Unable to open the database file
(define SQLITE_PROTOCOL    15)  ;;NOT USED. Database lock protocol error
(define SQLITE_EMPTY       16)  ;;Database is empty
(define SQLITE_SCHEMA      17)  ;;The database schema changed
(define SQLITE_TOOBIG      18)  ;;String or BLOB exceeds size limit
(define SQLITE_CONSTRAINT  19)  ;;Abort due to constraint violation
(define SQLITE_MISMATCH    20)  ;;Data type mismatch
(define SQLITE_MISUSE      21)  ;;Library used incorrectly
(define SQLITE_NOLFS       22)  ;;Uses OS features not supported on host
(define SQLITE_AUTH        23)  ;;Authorization denied
(define SQLITE_FORMAT      24)  ;;Auxiliary database format error
(define SQLITE_RANGE       25)  ;;2nd parameter to sqlite3_bind out of range
(define SQLITE_NOTADB      26)  ;;File opened that is not a database file
(define SQLITE_ROW         100) ;;sqlite3_step() has another row ready
(define SQLITE_DONE        101) ;;sqlite3_step() has finished executing


;;;;;;;;;;;;;;;;;
;;; Functions ;;;
;;;;;;;;;;;;;;;;;

(define sqlite3-free
  (let ((f (dlsym *sqlite3* "sqlite3_free")))
    (lambda (address)
      (dlcall f address))))

(define sqlite3-open
  (let ((f (dlsym *sqlite3* "sqlite3_open"))
        (db "    "))
    (lambda (filename)
      (if (zero? (dlcall f db filename))
        (string-ref->tetra db 0)
        #f))))

(define sqlite3-close
  (let ((f (dlsym *sqlite3* "sqlite3_close")))
    (lambda (db)
      (dlcall f db))))

(define sqlite3-finalize
  (let ((f (dlsym *sqlite3* "sqlite3_finalize")))
    (lambda (stmt)
      (dlcall f stmt))))

(define sqlite3-result 0)
(define sqlite3-prepare
  (let ((f (dlsym *sqlite3* "sqlite3_prepare"))
        (stmt-ptr "    ")
        (tail-ptr "    "))
    (lambda (db sql)
      (set! sqlite3-result (dlcall f tail-ptr stmt-ptr (+ 1 (string-length sql)) sql db))
      (if (zero? sqlite3-result)
        (string-ref->tetra stmt-ptr 0)
        #f))))

(define sqlite3-step
  (let ((f (dlsym *sqlite3* "sqlite3_step")))
    (lambda (stmt)
      (dlcall f stmt))))

(define sqlite3-reset
  (let ((f (dlsym *sqlite3* "sqlite3_reset")))
    (lambda (stmt)
      (dlcall f stmt))))

(define sqlite3-errmsg
  (let ((f (dlsym *sqlite3* "sqlite3_errmsg")))
    (lambda (db)
      (let ((a (dlcall f db)))
        (if (zero? a) #f
          (make-immutable-string a))))))

(define sqlite3-column-text
  (let ((f (dlsym *sqlite3* "sqlite3_column_text")))
    (lambda (stmt col)
      (let ((a (dlcall f col stmt)))
        (if (zero? a) #f
          (string-copy (make-immutable-string a)))))))

(define sqlite3-column-count
  (let ((f (dlsym *sqlite3* "sqlite3_column_count")))
    (lambda (stmt)
      (dlcall f stmt))))

;;;;;;;;;;;;;;;
;;; Helpers ;;;
;;;;;;;;;;;;;;;

(define (sqlite-select db sql)
  (let ((stmt (sqlite3-prepare db sql)))
    (if stmt
      (letrec
        ((last-column (- (sqlite3-column-count stmt) 1))
         (loop-rows
           (lambda (rows)
             (case (sqlite3-step stmt)
               ((101) (sqlite3-finalize stmt) (reverse rows))
               ((100)
                (letrec
                  ((row '())
                   (loop-row
                     (lambda (c)
                       (set! row (cons (sqlite3-column-text stmt c) row))
                       (if (zero? c) row (loop-row (- c 1))))))
                  (loop-rows (cons (loop-row last-column) rows))))
               (else (sqlite3-finalize stmt) #f)))))
        (loop-rows '())))))
(define (sqlite-make-select db sql)
  (let ((stmt (sqlite3-prepare db sql)))
    (if stmt
      (letrec
        ((last-column (- (sqlite3-column-count stmt) 1))
         (loop-row
           (lambda (row c)
             (if (negative? c) row
               (loop-row
                 (cons (sqlite3-column-text stmt c) row)
                 (- c 1)))))
         (cleanup
           (lambda ()
             (sqlite3-finalize stmt)
             (set! stmt #f)
             #f)))
        (lambda (cancel)
          (if stmt
            (if cancel
              (cleanup)
              (case (sqlite3-step stmt)
                ((100) (loop-row '() last-column))
                (else (cleanup))))))))))
