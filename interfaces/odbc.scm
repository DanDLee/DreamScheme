;;;;;;;;;;;;;;;;;;;
;;; Dll Modules ;;;
;;;;;;;;;;;;;;;;;;;
;;;Requires (load "windows/heap.scm") or (load "linux/libc.scm")
;(require "linux/libc.scm")

(define *odbc* (dlopen "odbc32"))
(if (zero? *odbc*) (error "Could not open ODBC library"))

;;;;;;;;;;;;;;;
;;; Handles ;;;
;;;;;;;;;;;;;;;
(define *odbc-env* #f)

;;;;;;;;;;;;;;;;;
;;; Constants ;;;
;;;;;;;;;;;;;;;;;
(define SQL_SUCCESS 0)
(define SQL_SUCCESS_WITH_INFO 1)

(define SQL_DRIVER_NOPROMPT 0)
(define SQL_DIAG_MESSAGE_TEXT 6)
(define SQL_ATTR_ODBC_VERSION 200)

(define SQL_NULL_HANDLE 0)
(define SQL_HANDLE_ENV 1)
(define SQL_HANDLE_DBC 2)
(define SQL_HANDLE_STMT 3)

(define SQL_CHAR 1)
(define SQL_C_CHAR 1)
(define SQL_VARCHAR 12)

(define SQL_PARAM_INPUT 1)

(define SQL_NULL_DATA #xffffffff)

;;;;;;;;;;;;;;;;;
;;; Functions ;;;
;;;;;;;;;;;;;;;;;

(define (sql-succeeded? ret)
  (or (= ret SQL_SUCCESS)
      (= ret SQL_SUCCESS_WITH_INFO)))

(define sql-alloc-handle
  (let ((f (dlsym *odbc* "SQLAllocHandle"))
        (handle-ptr "    "))
    (lambda (type context)
      (if (sql-succeeded? (& #xffff (dlcall f handle-ptr context type)))
        (string-ref->tetra handle-ptr 0)
        #f))))

(define sql-set-env-attr
  (let ((f (dlsym *odbc* "SQLSetEnvAttr")))
    (lambda (env attr value)
      (& #xffff (dlcall f (if (string? value) (string-length value) 0) value attr env)))))

(define sql-free-handle
  (let ((f (dlsym *odbc* "SQLFreeHandle")))
    (lambda (type handle)
      (& #xffff (dlcall f handle type)))))

(define sql-driver-connect
  (let ((f (dlsym *odbc* "SQLDriverConnect")))
    (lambda (dbc conn)
      (& #xffff (dlcall f SQL_DRIVER_NOPROMPT 0 0 0 (string-length conn) conn 0 dbc)))))

(define sql-exec
  (let ((f (dlsym *odbc* "SQLExecute")))
    (lambda (stmt)
      (& #xffff (dlcall f stmt)))))

(define sql-prepare
  (let ((f (dlsym *odbc* "SQLPrepare")))
    (lambda (stmt sql)
      (& #xffff (dlcall f (string-length sql) sql stmt)))))

(define sql-bind-parameter
  (let ((f (dlsym *odbc* "SQLBindParameter")))
    (lambda (stmt index io-type value-type param-type buffer ind)
      (& #xffff (dlcall f ind (string-length buffer) buffer 0 (string-length buffer) param-type value-type io-type index stmt)))))

(define sql-close-cursor
  (let ((f (dlsym *odbc* "SQLCloseCursor")))
    (lambda (stmt)
      (& #xffff (dlcall f stmt)))))

(define sql-exec-direct
  (let ((f (dlsym *odbc* "SQLExecDirect")))
    (lambda (stmt sql)
      (& #xffff (dlcall f (string-length sql) sql stmt)))))

(define sql-fetch
  (let ((f (dlsym *odbc* "SQLFetch")))
    (lambda (stmt)
      (& #xffff (dlcall f stmt)))))

(define sql-bind-col
  (let ((f (dlsym *odbc* "SQLBindCol")))
    (lambda (stmt col type buffer ind)
      (& #xffff (dlcall f ind (string-length buffer) buffer type col stmt)))))

(define sql-cancel
  (let ((f (dlsym *odbc* "SQLCancel")))
    (lambda (stmt)
      (& #xffff (dlcall f stmt)))))

(define sql-disconnect
  (let ((f (dlsym *odbc* "SQLDisconnect")))
    (lambda (dbc)
      (& #xffff (dlcall f dbc)))))

(define sql-get-diag-field
  (let ((f (dlsym *odbc* "SQLGetDiagField")))
    (lambda (type handle recnum ident buffer len-ptr)
      (& #xffff (dlcall f len-ptr (string-length buffer) buffer ident recnum handle type)))))

;;;;;;;;;;;;;;;
;;; Helpers ;;;
;;;;;;;;;;;;;;;
(define *sql-error* (make-string 300))
(define (sql-error type handle recnum)
  (let ((len-ptr "  "))
    (if (sql-succeeded?
          (sql-get-diag-field type handle recnum SQL_DIAG_MESSAGE_TEXT *sql-error* len-ptr))
      (substring *sql-error* 0 (string-ref->wyde len-ptr 0)))))

(define *sql-stmt-error* #f)
(define (sql-make-cursor dbc sql . sizes)
  (set! *sql-stmt-error* #f)
  (let* ((stmt (sql-alloc-handle SQL_HANDLE_STMT dbc))
         (values (map (lambda (s) (alloc s)) sizes))
         (indicators (map (lambda (s) (alloc 4)) sizes))
         (cleanup
           (lambda ()
             (sql-cancel stmt)
             (for-each free values)
             (for-each free indicators)
             (sql-free-handle SQL_HANDLE_STMT stmt)
             (set! stmt #f)
             #f))
         (position 0))
    (for-each
      (lambda (v i)
        (set! position (+ position 1))
        (sql-bind-col stmt position SQL_CHAR v i))
      values indicators)
    (if (sql-succeeded? (sql-exec-direct stmt sql))
      (lambda (cancel)
        (if stmt
          (if cancel
            (cleanup)
            (if (= 100 (sql-fetch stmt))
              (cleanup)
              (map
                (lambda (v i)
                  (let ((len (string-ref->tetra i 0)))
                    (case len
                      ((#xffffffff) #f) ;Column is NULL
                      (else
                        (substring v 0
                          (if (>= len (string-length v))
                            (- (string-length v) 1)
                            len))))))
                values indicators)))))
      (begin
        (set! *sql-stmt-error* (sql-error SQL_HANDLE_STMT stmt 1))
        (cleanup)))))

(define (list-cursor l c)
  (let ((row (c #f)))
    (if row
      (list-cursor (cons row l) c)
      l)))

(define (sql-select dbc sql . sizes)
  (let ((c (apply sql-make-cursor dbc sql sizes)))
    (if c
      (list-cursor '() c))))

(define (sql-execute dbc sql)
  (set! *sql-stmt-error* #f)
  (let* ((stmt (sql-alloc-handle SQL_HANDLE_STMT dbc))
         (result (sql-succeeded? (sql-exec-direct stmt sql))))
    (if (not result)
      (set! *sql-stmt-error* (sql-error SQL_HANDLE_STMT stmt 1)))
    (sql-free-handle SQL_HANDLE_STMT stmt)
    result))

(define (sql-make-execute dbc sql . sizes)
  (set! *sql-stmt-error* #f)
  (let ((stmt (sql-alloc-handle SQL_HANDLE_STMT dbc)))
    (if (sql-succeeded? (sql-prepare stmt sql))
      (let* ((buffers (map (lambda (s) (alloc s)) sizes))
             (indicators (map (lambda (s) (alloc 4)) sizes))
             (cleanup
               (lambda ()
                 (for-each free buffers)
                 (for-each free indicators)
                 (sql-free-handle SQL_HANDLE_STMT stmt)
                 (set! stmt #f)
                 #f))
             (position 0))
        (for-each
          (lambda (b i)
            (set! position (+ position 1))
            (sql-bind-parameter stmt position SQL_PARAM_INPUT SQL_C_CHAR SQL_VARCHAR b i))
          buffers indicators)
        (lambda (values)
          (if stmt
            (if values
              (begin
                (for-each
                  (lambda (v b i)
                    (if v
                      (begin
                         (substring-set! b 0 (string-length v) v)
                         (string-set-tetra! i 0 (string-length v)))
                      (string-set-tetra! i 0 SQL_NULL_DATA)))
                  values buffers indicators)
                (if (sql-succeeded? (sql-exec stmt))
                  #t
                  (begin
                    (set! *sql-stmt-error* (sql-error SQL_HANDLE_STMT stmt 1))
                    (cleanup))))
              (cleanup))))))))
