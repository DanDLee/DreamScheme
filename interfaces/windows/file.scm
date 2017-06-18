;;;;;;;;;;;;;;;;;;;
;;; Dll Modules ;;;
;;;;;;;;;;;;;;;;;;;

(define *kernel* (get-module-handle "kernel32"))

;;;;;;;;;;;;;;;
;;; Structs ;;;
;;;;;;;;;;;;;;;

;; WIN32_FIND_DATA
(define (make-find-data)
  (define sr (make-string 322))
  sr)
(define (find-data-file-name fd)
  (string-copy (make-immutable-string (+ 44 (string->address fd)))))
(define (find-data-file-size-high fd)
  (string-ref->tetra fd 28))
(define (find-data-file-size-low fd)
  (string-ref->tetra fd 32))
(define (make-file-information)
  (define fi (make-string 52))
  fi)
(define (file-information-file-size-low fi)
  (string-ref->tetra fi 36))

;;;;;;;;;;;;;;;;;
;;; Functions ;;;
;;;;;;;;;;;;;;;;;

(define find-first-file
  (let ((f (dlsym *kernel* "FindFirstFileA")))
    (lambda (path fd)
      (dlcall f fd path))))

(define find-next-file
  (let ((f (dlsym *kernel* "FindNextFileA")))
    (lambda (handle fd)
      (dlcall f fd handle))))

(define find-close
  (let ((f (dlsym *kernel* "FindClose")))
    (lambda (handle)
      (dlcall f handle))))

(define delete-file
  (let ((f (dlsym *kernel* "DeleteFileA")))
    (lambda (path)
      (dlcall f path))))

(define create-file
  (let ((f (dlsym *kernel* "CreateFileA")))
    (lambda (path access creationdisp)
      (dlcall f 0 #x80 creationdisp 0 1 access path))))

(define create-inheritable-file
  (let ((f (dlsym *kernel* "CreateFileA")))
    (lambda (path access creationdisp)
      (let ((sd (make-string 12)))
        (string-set-tetra! sd 0 12)
        (string-set-tetra! sd 8 1)
        (dlcall f 0 #x80 creationdisp sd 3 access path)))))

(define get-file-information-by-handle
  (let ((f (dlsym *kernel* "GetFileInformationByHandle")))
    (lambda (handle fi)
      (dlcall f fi handle))))

(define file-close-handle
  (let ((f (dlsym *kernel* "CloseHandle")))
    (lambda (handle)
      (dlcall f handle))))

;;;;;;;;;;;;;;;
;;; Helpers ;;;
;;;;;;;;;;;;;;;

(define (find-file-names path)
  (let* ((fd (make-find-data))
         (handle (find-first-file path fd))
         (first-name (find-data-file-name fd)))
    (cond
      ((zero? handle) #f)
      ((zero? (string-length first-name)) '())
      (else
       (letrec
        ((loop
           (lambda (names)
             (if (positive? (find-next-file handle fd))
               (loop (cons (find-data-file-name fd) names))
               (begin
                 (find-close handle)
                 names)))))
        (loop (list first-name)))))))

(define (find-files path)
  (let* ((fd (make-find-data))
         (handle (find-first-file path fd))
         (first-name (find-data-file-name fd)))
    (cond
      ((zero? handle) #f)
      ((zero? (string-length first-name)) '())
      (else
       (letrec
        ((loop
           (lambda (files)
             (let ((fd (make-find-data)))
               (if (positive? (find-next-file handle fd))
                 (loop (cons fd files))
                 (begin
                   (find-close handle)
                   files))))))
        (loop (list fd)))))))

(define (find-file path)
  (let* ((fd (make-find-data))
         (handle (find-first-file path fd))
         (first-name (find-data-file-name fd)))
    (cond
      ((or (zero? handle)
           (zero? (string-length first-name)))
       #f)
      (else
       (find-close handle)
       fd))))

(define (get-file-information path)
  (let* ((f (create-file path 0 3)))
    (if (= f #xffffffff) #f
      (let* ((fi (make-file-information))
             (result (get-file-information-by-handle f fi)))
        (file-close-handle f)
        (if (zero? result) #f fi)))))
