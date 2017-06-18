;;;;;;;;;;;;;;;;;;;
;;; Dll Modules ;;;
;;;;;;;;;;;;;;;;;;;
;;;Requires (load "windows/heap.scm") or (load "linux/libc.scm")
;;;Requires (define *oci-path* "oci")

(define *oci* (dlopen *oci-path*))
(if (zero? *oci*) (error "Could not open OCI library"))

;;;;;;;;;;;;;;;
;;; Handles ;;;
;;;;;;;;;;;;;;;

(define OCI_OBJECT 2)
(define *oci-env*
  (let ((f (dlsym *oci* "OCIEnvCreate"))
        (env "    "))
    (if (positive? (dlcall f 0 0 0 0 0 0 OCI_OBJECT env))
      (error "Could not create OCI Environment"))
    (string-ref->tetra env 0)))

;;;;;;;;;;;;;;;
;;; Structs ;;;
;;;;;;;;;;;;;;;

(define (make-oci-iov buffer)
  (define iov (make-string 8))
  (string-set-tetra! iov 0 (string->address buffer))
  (string-set-tetra! iov 4 (string-length buffer))
  iov)

;;;;;;;;;;;;;;;;;
;;; Constants ;;;
;;;;;;;;;;;;;;;;;

(define OCI_SUCCESS 0)
(define OCI_SUCCESS_WITH_INFO 1)
(define OCI_NO_DATA 100)
(define OCI_ERROR #xffffffff)
(define OCI_INVALID_HANDLE #xfffffffe)

(define OCI_HTYPE_ENV 1)
(define OCI_HTYPE_ERROR 2)
(define OCI_HTYPE_SVCCTX 3)
(define OCI_HTYPE_STMT 4)
(define OCI_HTYPE_BIND 5)
(define OCI_HTYPE_DEFINE 6)
(define OCI_HTYPE_DESCRIBE 7)
(define OCI_HTYPE_SERVER 8)
(define OCI_HTYPE_SESSION 9)

(define OCI_NTV_SYNTAX 1)
(define OCI_V7_SYNTAX 2)
(define OCI_V8_SYNTAX 3)

;;OCISessionBegin credentials
(define OCI_CRED_RDBMS 1)
 
;;OCIStmtExecute mode
(define OCI_BATCH_ERRORS #x80)
(define OCI_COMMIT_ON_SUCCESS #x20)
(define OCI_DEFAULT 0)
(define OCI_DESCRIBE_ONLY #x10)
(define OCI_EXACT_FETCH #x02)
(define OCI_PARSE_ONLY #x100)
(define OCI_STMT_SCROLLABLE_READONLY #x08)

;;OCIStmtFetch orientation
(define OCI_FETCH_CURRENT #x01)
(define OCI_FETCH_NEXT #x02)
(define OCI_FETCH_FIRST #x04)
(define OCI_FETCH_LAST #x08)
(define OCI_FETCH_PRIOR #x10)
(define OCI_FETCH_ABSOLUTE #x20)
(define OCI_FETCH_RELATIVE #x40)

;;Attributes
(define OCI_ATTR_SERVER 6)
(define OCI_ATTR_SESSION 7)
(define OCI_ATTR_USERNAME 22)
(define OCI_ATTR_PASSWORD 23)
(define OCI_ATTR_PREFETCH_ROWS 11)
(define OCI_ATTR_PREFETCH_MEMORY 13)

;;OCIBindByPos mode
(define OCI_IOV #x200)

;;Datatypes
(define TYPE_VARCHAR 1)
(define TYPE_INTEGER 3)

;;;;;;;;;;;;;;;;;
;;; Functions ;;;
;;;;;;;;;;;;;;;;;

(define oci-handle-alloc
  (let ((f (dlsym *oci* "OCIHandleAlloc"))
        (handle-ptr "    "))
    (lambda (type)
      (if (positive? (dlcall f 0 0 type handle-ptr *oci-env*))
        #f
        (string-ref->tetra handle-ptr 0)))))
(define *oci-error* (oci-handle-alloc OCI_HTYPE_ERROR))

(define oci-handle-free
  (let ((f (dlsym *oci* "OCIHandleFree")))
    (lambda (handle type)
      (dlcall f type handle))))

(define oci-attr-set
  (let ((f (dlsym *oci* "OCIAttrSet")))
    (lambda (handle type value size attrtype)
      (dlcall f *oci-error* attrtype size value type handle))))

(define oci-error-get
  (let ((f (dlsym *oci* "OCIErrorGet")))
    (lambda (error recordno code-ptr buffer type)
      (dlcall f type (string-length buffer) buffer code-ptr 0 recordno error))))

(define oci-logoff
  (let ((f (dlsym *oci* "OCILogoff")))
    (lambda (service)
      (dlcall f *oci-error* service))))

(define oci-server-attach
  (let ((f (dlsym *oci* "OCIServerAttach")))
    (lambda (server db mode)
      (dlcall f mode (string-length db) db *oci-error* server))))

(define oci-server-detach
  (let ((f (dlsym *oci* "OCIServerDetach")))
    (lambda (server)
      (dlcall f OCI_DEFAULT *oci-error* server))))

(define oci-session-begin
  (let ((f (dlsym *oci* "OCISessionBegin")))
    (lambda (service session credt mode)
      (dlcall f mode credt session *oci-error* service))))

(define oci-session-end
  (let ((f (dlsym *oci* "OCISessionEnd")))
    (lambda (service session)
      (dlcall f OCI_DEFAULT session *oci-error* service))))

(define oci-stmt-prepare
  (let ((f (dlsym *oci* "OCIStmtPrepare")))
    (lambda (stmt text syntax)
      (dlcall f 0 syntax (string-length text) text *oci-error* stmt)))) 

(define oci-stmt-execute
  (let ((f (dlsym *oci* "OCIStmtExecute")))
    (lambda (service stmt iters rowoff mode)
      (dlcall f mode 0 0 rowoff iters *oci-error* stmt service)))) 

(define oci-define-by-pos
  (let ((f (dlsym *oci* "OCIDefineByPos")))
    (lambda (stmt defn-ptr position value dty ind rlen rcode mode)
      (dlcall f mode rcode rlen ind dty (string-length value) value position *oci-error* defn-ptr stmt))))

(define oci-stmt-fetch2
  (let ((f (dlsym *oci* "OCIStmtFetch2")))
    (lambda (stmt nrows orientation fetch-offset)
      (dlcall f 0 fetch-offset orientation nrows *oci-error* stmt)))) 

(define oci-bind-by-pos
  (let ((f (dlsym *oci* "OCIBindByPos")))
    (lambda (stmt bind-ptr position value value-sz dty ind alen rcode mode)
      (dlcall f mode 0 0 rcode alen ind dty value-sz value position *oci-error* bind-ptr stmt))))

;;;;;;;;;;;;;;;
;;; Helpers ;;;
;;;;;;;;;;;;;;;

(define (quit)
  (oci-handle-free *oci-env* OCI_HTYPE_ENV)
  (exit))

(define (error-message)
  (let ((code "    ")
        (buffer (make-string 100 #\space)))
    (oci-error-get *oci-error* 1 code buffer OCI_HTYPE_ERROR)
    buffer))

(define (attach-server db)
  (define server (oci-handle-alloc OCI_HTYPE_SERVER))
  (cond
    ((not server) #f)
    ((positive? (oci-server-attach server db OCI_DEFAULT))
       (oci-handle-free server OCI_HTYPE_SERVER)
       #f)
    (else server)))

(define (detach-server server)
  (oci-server-detach server)
  (oci-handle-free server OCI_HTYPE_SERVER))

(define (make-service server)
  (define service (oci-handle-alloc OCI_HTYPE_SVCCTX))
  (cond
    ((not service) #f)
    ((positive? (set-server-context! service server))
       (free-service service)
       #f)
    (else service)))

(define (set-server-context! service server)
  (oci-attr-set service OCI_HTYPE_SVCCTX server 0 OCI_ATTR_SERVER))

(define (free-service service)
  (oci-handle-free service OCI_HTYPE_SVCCTX))

(define (begin-session service user password)
  (define session (oci-handle-alloc OCI_HTYPE_SESSION))
  (cond
    ((not session) #f)
    ((or (positive? (oci-attr-set session OCI_HTYPE_SESSION user (string-length user) OCI_ATTR_USERNAME))
         (positive? (oci-attr-set session OCI_HTYPE_SESSION password (string-length password) OCI_ATTR_PASSWORD))
         (positive? (oci-session-begin service session OCI_CRED_RDBMS OCI_DEFAULT))
         (positive? (oci-attr-set service OCI_HTYPE_SVCCTX session 0 OCI_ATTR_SESSION)))
     (oci-handle-free session OCI_HTYPE_SESSION)
     #f)
    (else session)))

(define (end-session service session)
  (oci-session-end service session)
  (oci-handle-free session OCI_HTYPE_SESSION))

(define (make-prepared-statement text)
  (define stmt (oci-handle-alloc OCI_HTYPE_STMT))
  (cond
    ((not stmt) #f)
    ((positive? (oci-stmt-prepare stmt text OCI_NTV_SYNTAX))
       (free-prepared-statement stmt)
       #f)
    (else stmt)))

(define (free-prepared-statement stmt)
  (oci-handle-free stmt OCI_HTYPE_STMT))

(define (make-cursor service stmt . sizes)
  (let* ((numcol (length sizes))
         (rcode (alloc 2))
         (rlens (map (lambda (s) (alloc 2)) sizes))
         (values (map (lambda (s) (alloc s)) sizes))
         (indicators (map (lambda (s) (alloc 2)) sizes))
         (cleanup
           (lambda ()
             (for-each free rlens)
             (for-each free values)
             (for-each free indicators)
             (free-prepared-statement stmt)
             (set! stmt #f)
             #f))
         (position 0))
    (for-each
      (lambda (s r v i)
        (set! position (+ position 1))
        (oci-define-by-pos stmt "    " position v TYPE_VARCHAR i r rcode 0))
      sizes rlens values indicators)
    (if (zero? (oci-stmt-execute service stmt 0 0 OCI_DEFAULT))
      (lambda (cancel)
        (if stmt
          (if cancel
            (begin
              (oci-stmt-fetch2 stmt 0 OCI_FETCH_NEXT 0)
              (cleanup))
            (case (oci-stmt-fetch2 stmt 1 OCI_FETCH_NEXT 0)
              ((0 1) ;SUCCESS or SUCCESS_WITH_INFO
               (map
                 (lambda (r i v)
                   (case (string-ref->wyde i 0)
                     ((#xffff) #f) ;Column is NULL
                     (else (substring v 0 (string-ref->wyde r 0)))))
                 rlens
                 indicators
                 values))
              (else ;Should be OCI_NO_DATA
               (cleanup))))))
      (cleanup))))

(define (write-cursor c)
  (let ((row (c #f)))
    (if row
      (begin
        (display row)
        (newline)
        (write-cursor c)))))

(define (list-cursor l c)
  (let ((row (c #f)))
    (if row
      (list-cursor (cons row l) c)
      l)))

(define (bounded-list-cursor l c count)
  (if (zero? count) l
    (let ((row (c #f)))
      (if row
        (bounded-list-cursor (cons row l) c (- count 1))
        l))))

(define DEFAULT_PREFETCH_ROWS 1000)
(define (make-select service text . sizes)
  (let ((stmt (make-prepared-statement text)))
    (if stmt
      (let ((pre-rows-ptr "    "))
        (string-set-tetra! pre-rows-ptr 0 DEFAULT_PREFETCH_ROWS)
        (oci-attr-set stmt OCI_HTYPE_STMT pre-rows-ptr 4 OCI_ATTR_PREFETCH_ROWS)
        (apply make-cursor service stmt sizes)))))

(define (make-select-prefetch pre-rows service text . sizes)
  (let ((stmt (make-prepared-statement text)))
    (if stmt
      (let ((pre-rows-ptr "    "))
        (string-set-tetra! pre-rows-ptr 0 pre-rows)
        (oci-attr-set stmt OCI_HTYPE_STMT pre-rows-ptr 4 OCI_ATTR_PREFETCH_ROWS)
        (apply make-cursor service stmt sizes)))))

(define (select service text . sizes)
  (let ((c (apply make-select service text sizes)))
    (if c
      (list-cursor '() c))))

(define (array-select service text . sizes)
  (let ((c (apply make-select service text sizes)))
    (if c
      (lambda (count)
        (if (positive? count)
          (bounded-list-cursor '() c count)
          (c #t))))))

(define (execute service text)
  (let* ((stmt (make-prepared-statement text))
         (code (oci-stmt-execute service stmt 1 0 OCI_COMMIT_ON_SUCCESS)))
    (free-prepared-statement stmt)
    code))

(define (array-execute service text array . sizes)
  (let* ((stmt (make-prepared-statement text)))
    (if stmt
      (let* ((numcol (length sizes))
             (numrows (length array))
             (rcodes (map (lambda (s) (alloc (* 2 numrows))) sizes))
             (alens (map (lambda (s) (alloc (* 2 numrows))) sizes))
             (indicators (map (lambda (s) (alloc (* 2 numrows))) sizes))
             (values (map (lambda (s) (alloc (* s numrows))) sizes))
             (cleanup
               (lambda ()
                 (for-each free rcodes)
                 (for-each free alens)
                 (for-each free indicators)
                 (for-each free values)
                 (free-prepared-statement stmt)
                 (set! stmt #f)
                 #f))
             (index 0)
             (position 0))
        (for-each
          (lambda (row)
            (for-each
              (lambda (s a v i r)
                (if r
                  (let ((len (string-length r))
                        (start (* index s)))
                    (substring-set! v start (+ start s) r)
                    (string-set-wyde! a (* index 2) len)
                    (string-set-wyde! i (* index 2) 0))
                  (begin
                    (string-set-wyde! a (* index 2) 0)
                    (string-set-wyde! i (* index 2) #xffff))))
              sizes alens values indicators row)
            (set! index (+ index 1)))
          array)
        (for-each
          (lambda (s a v i r)
            (set! position (+ position 1))
            (if (positive? (oci-bind-by-pos stmt "    " position (make-oci-iov v) s TYPE_VARCHAR i a r OCI_IOV))
              (begin
                (cleanup)
                (error (error-message)))))
          sizes alens values indicators rcodes)
        (let ((rc (oci-stmt-execute service stmt numrows 0 OCI_COMMIT_ON_SUCCESS)))
          (cleanup)
          rc)))))
