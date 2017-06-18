;;;;;;;;;;;;;;;;;;;
;;; Dll Modules ;;;
;;;;;;;;;;;;;;;;;;;

(define *kernel* (get-module-handle "kernel32"))

;;;;;;;;;;;;;;;;;
;;; Constants ;;;
;;;;;;;;;;;;;;;;;
(define HEAP_NO_SERIALIZE #x01)
(define HEAP_GENERATE_EXCEPTIONS #x04)
(define HEAP_ZERO_MEMORY #x08)
(define HEAP_REALLOC_IN_PLACE_ONLY #x10)

;;;;;;;;;;;;;;;;;
;;; Functions ;;;
;;;;;;;;;;;;;;;;;

(define get-process-heap
  (let ((f (dlsym *kernel* "GetProcessHeap")))
    (lambda () (dlcall f))))

(define heap-alloc
  (let ((f (dlsym *kernel* "HeapAlloc")))
    (lambda (heap flags bytes)
      (dlcall f bytes flags heap))))

(define heap-free
  (let ((f (dlsym *kernel* "HeapFree")))
    (lambda (heap flags lpmem)
      (dlcall f lpmem flags heap))))

(define heap-realloc
  (let ((f (dlsym *kernel* "HeapRealloc")))
    (lambda (heap flags lpmem bytes)
      (dlcall f bytes lpmem flags heap))))

;;;;;;;;;;;;;;;
;;; Helpers ;;;
;;;;;;;;;;;;;;;
(define *heap* (get-process-heap))
(define (alloc b)
  (make-immutable-string
    (heap-alloc *heap* HEAP_ZERO_MEMORY b) b))
(define (free b)
  (heap-free *heap* HEAP_NO_SERIALIZE b))
