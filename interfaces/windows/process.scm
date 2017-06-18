;;;;;;;;;;;;;;;;;;;
;;; Dll Modules ;;;
;;;;;;;;;;;;;;;;;;;

(define *kernel* (get-module-handle "kernel32"))

;;;;;;;;;;;;;;;;;
;;; Constants ;;;
;;;;;;;;;;;;;;;;;
(define STILL_ACTIVE 259)
(define STARTF_USESTDHANDLES #x100)

;;;;;;;;;;;;;;;
;;; Structs ;;;
;;;;;;;;;;;;;;;

(define (make-startup-info)
  (define si (make-string 68))
  (string-set-tetra! si 0 68)
  si)
(define (startup-info-set-flags! si f)
  (string-set-tetra! si 44 f))
(define (startup-info-set-stdinput! si h)
  (string-set-tetra! si 56 h))
(define (startup-info-set-stdoutput! si h)
  (string-set-tetra! si 60 h))
(define (startup-info-set-stderror! si h)
  (string-set-tetra! si 64 h))
(define (make-process-info)
  (make-string 16))
(define (process-info-hprocess pi)
  (string-ref->tetra pi 0))
(define (process-info-hthread pi)
  (string-ref->tetra pi 4))

;;;;;;;;;;;;;;;;;
;;; Functions ;;;
;;;;;;;;;;;;;;;;;

(define process-create
  (let ((f (dlsym *kernel* "CreateProcessA")))
    (lambda (appname commandline directory startup-info process-info)
      (dlcall f process-info startup-info directory 0 0 1 0 0 commandline appname))))

(define process-terminate
  (let ((f (dlsym *kernel* "TerminateProcess")))
    (lambda (handle exitcode)
      (dlcall f exitcode handle))))

(define process-close-handle
  (let ((f (dlsym *kernel* "CloseHandle")))
    (lambda (handle)
      (dlcall f handle))))

(define process-get-exit-code
  (let ((f (dlsym *kernel* "GetExitCodeProcess")))
    (lambda (handle)
      (let ((code (make-string 4)))
        (if (zero? (dlcall f code handle)) (error "GetExitCodeProcess failed"))
        (string-ref->tetra code 0)))))

(define sleep
  (let ((f (dlsym *kernel* "Sleep")))
    (lambda (milli)
      (dlcall f milli))))

;;;;;;;;;;;;;;;
;;; Helpers ;;;
;;;;;;;;;;;;;;;
