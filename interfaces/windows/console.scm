;;;;;;;;;;;;;;;;;;;
;;; Dll Modules ;;;
;;;;;;;;;;;;;;;;;;;

(define *kernel* (get-module-handle "kernel32"))

;;;;;;;;;;;;;;;
;;; Structs ;;;
;;;;;;;;;;;;;;;

;; coord
(define (make-coord x y)
  (+ x (* y #x10000)))
(define (coord-x c)
  (remainder c #x10000))
(define (coord-y c)
  (quotient c #x10000))

;; small-rect
(define (make-small-rect left top right bottom)
  (define sr (make-string 8))
  (string-set-wyde! sr 0 left)
  (string-set-wyde! sr 2 top)
  (string-set-wyde! sr 4 right)
  (string-set-wyde! sr 6 bottom)
  sr)
(define (small-rect-left sr)
  (string-ref->wyde sr 0))
(define (small-rect-top sr)
  (string-ref->wyde sr 2))
(define (small-rect-right sr)
  (string-ref->wyde sr 4))
(define (small-rect-bottom sr)
  (string-ref->wyde sr 6))

;; console-screen-buffer-info
(define (make-console-screen-buffer-info)
  (make-string 22))
(define (console-screen-buffer-info-size csbi)
  (string-ref->tetra csbi 0))
(define (console-screen-buffer-info-cursor-position csbi)
  (string-ref->tetra csbi 4))
(define (console-screen-buffer-info-attributes csbi)
  (string-ref->wyde csbi 8))
(define (console-screen-buffer-info-window csbi)
  (substring csbi 10 18))
(define (console-screen-buffer-info-window-maximum-size csbi)
  (string-ref->tetra csbi 18))

;; char-info
(define (make-char-info ch attr)
  (define s (make-string 4))
  (string-set-wyde! s 0 (char->integer ch))
  (string-set-wyde! s 2 attr)
  s)

;;;;;;;;;;;;;;;;;
;;; Functions ;;;
;;;;;;;;;;;;;;;;;

(define get-console-screen-buffer-info
  (let ((f (dlsym *kernel* "GetConsoleScreenBufferInfo")))
    (lambda ()
      (define csbi (make-console-screen-buffer-info))
      (dlcall f csbi (current-output-port))
      csbi)))

(define set-console-cursor-position
  (let ((f (dlsym *kernel* "SetConsoleCursorPosition")))
    (lambda (c)
      (dlcall f c (current-output-port)))))

(define set-console-display-mode
  (let ((f (dlsym *kernel* "SetConsoleDisplayMode")))
    (lambda (full-screen c)
      (dlcall f c (if full-screen 1 2) (current-output-port)))))

(define set-console-text-attribute
  (let ((f (dlsym *kernel* "SetConsoleTextAttribute")))
    (lambda (attr)
      (dlcall f attr (current-output-port)))))

(define scroll-console-screen-buffer
  (let ((f (dlsym *kernel* "ScrollConsoleScreenBufferA")))
    (lambda (scroll-rect clip-rect dest-coord fill)
      (dlcall f fill dest-coord clip-rect scroll-rect (current-output-port)))))

(define fill-console-output-attribute
  (let ((f (dlsym *kernel* "FillConsoleOutputAttribute"))
        (out (make-string 4)))
    (lambda (attribute length coord)
      (dlcall f out coord length attribute (current-output-port)))))

(define fill-console-output-character
  (let ((f (dlsym *kernel* "FillConsoleOutputCharacterA"))
        (out (make-string 4)))
    (lambda (character length coord)
      (dlcall f out coord length (char->integer character) (current-output-port)))))
