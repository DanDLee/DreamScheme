;;;;;;;;;;;;;;;;;;;
;;; Dll Modules ;;;
;;;;;;;;;;;;;;;;;;;

(define *kernel* (get-module-handle "kernel32"))

;;;;;;;;;;;;;;;
;;; Structs ;;;
;;;;;;;;;;;;;;;

(define (make-time)
  (make-string 16))
(define (time-year st)
  (string-ref->wyde st 0))
(define (time-month st)
  (string-ref->wyde st 2))
(define (time-day-of-week st)
  (string-ref->wyde st 4))
(define (time-day st)
  (string-ref->wyde st 6))
(define (time-hour st)
  (string-ref->wyde st 8))
(define (time-minute st)
  (string-ref->wyde st 10))
(define (time-second st)
  (string-ref->wyde st 12))
(define (time-millisecond st)
  (string-ref->wyde st 14))

;;;;;;;;;;;;;;;;;
;;; Functions ;;;
;;;;;;;;;;;;;;;;;

(define get-system-time
  (let ((f (dlsym *kernel* "GetSystemTime")))
    (lambda (time)
      (dlcall f time))))
(define get-local-time
  (let ((f (dlsym *kernel* "GetLocalTime")))
    (lambda (time)
      (dlcall f time))))
