;;;;;;;;;;;;;;;;;;;
;;; Dll Modules ;;;
;;;;;;;;;;;;;;;;;;;

(define *libc* (dlopen "libc.so.6"))
(if (zero? *libc*) (error "Cannot open libc"))

;;;;;;;;;;;;;;;;;
;;; Functions ;;;
;;;;;;;;;;;;;;;;;

(define malloc
  (let ((f (dlsym *libc* "malloc")))
    (lambda (bytes)
      (dlcall f bytes))))

(define free
  (let ((f (dlsym *libc* "free")))
    (lambda (lpmem)
      (dlcall f lpmem))))

(define realloc
  (let ((f (dlsym *libc* "realloc")))
    (lambda (lpmem bytes)
      (dlcall f bytes lpmem))))

(define printf
  (let ((f (dlsym *libc* "printf")))
    (lambda x
      (apply dlcall f (reverse x)))))

(define fprintf
  (let ((f (dlsym *libc* "fprintf")))
    (lambda x
      (apply dlcall f (reverse x)))))

(define unlink
  (let ((f (dlsym *libc* "unlink")))
    (lambda (name)
      (dlcall f name))))

(define opendir
  (let ((f (dlsym *libc* "opendir")))
    (lambda (name)
      (dlcall f name))))

(define closedir
  (let ((f (dlsym *libc* "closedir")))
    (lambda (dirstream)
      (dlcall f dirstream))))

(define readdir
  (let ((f (dlsym *libc* "readdir")))
    (lambda (dirstream)
      (dlcall f dirstream))))

(define time
  (let ((f (dlsym *libc* "time")))
    (lambda ()
      (dlcall f 0))))

(define localtime
  (let ((f (dlsym *libc* "localtime")))
    (lambda (t)
      (dlcall f t))))

(define system
  (let ((f (dlsym *libc* "system")))
    (lambda (command)
      (dlcall f command))))

(define getenv
  (let ((f (dlsym *libc* "getenv")))
    (lambda (name)
      (let ((a (dlcall f name)))
        (if (zero? a) #f
          (make-immutable-string a))))))

(define setenv
  (let ((f (dlsym *libc* "setenv")))
    (lambda (name value replace)
      (dlcall f (if replace -1 0) value name))))

(define getpid
  (let ((f (dlsym *libc* "getpid")))
    (lambda ()
      (dlcall f))))

;;;;;;;;;;;;;;;
;;; Helpers ;;;
;;;;;;;;;;;;;;;
(define (alloc b)
  (make-immutable-string
    (malloc b) b))

(define file-exists? #f)
(let ((buf (make-string 88)))
  (set! file-exists?
    (lambda (name)
      (zero? (sys-stat name buf)))))

(define (list-file-names path)
  (let ((d (opendir path)))
    (if (zero? d)
      #f
      (letrec
        ((loop 
           (lambda (names)
             (let ((e (readdir d)))
               (if (zero? e)
                 names
                 (loop (cons (string-copy (make-immutable-string (+ e 11))) names)))))))
        (loop '())))))

(define (local-time)
  (let ((t (make-string 4)))
    (string-set-tetra! t 0 (time))
    (let* ((lt (make-immutable-string (localtime t) 44))
           (offset (string-ref->tetra lt 36)))
      (+ (string-ref->tetra t 0)
        (if (zero? (& #x80000000))
          offset
          (- (+ 1 (~ offset))))))))
