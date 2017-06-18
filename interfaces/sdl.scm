;;;;;;;;;;;;;;;;;;;
;;; Dll Modules ;;;
;;;;;;;;;;;;;;;;;;;

(define *sdl-path* "libSDL-1.2.so.0") ;"sdl" on Windows
(define *sdl* (dlopen *sdl-path*))
(if (zero? *sdl*) (error "Could not open SDL library"))

;;;;;;;;;;;;;;;
;;; Structs ;;;
;;;;;;;;;;;;;;;

(define (sdl-pixel-format a) (make-immutable-string a 35))
(define (sdl-pixel-format->palette f) (string-ref->tetra f 0))
(define (sdl-pixel-format->bits-per-pixel f) (string-ref->byte f 4))
(define (sdl-pixel-format->bytes-per-pixel f) (string-ref->byte f 5))
(define (sdl-pixel-format->rloss f) (string-ref->byte f 6))
(define (sdl-pixel-format->gloss f) (string-ref->byte f 7))
(define (sdl-pixel-format->bloss f) (string-ref->byte f 8))
(define (sdl-pixel-format->aloss f) (string-ref->byte f 9))
(define (sdl-pixel-format->rshift f) (string-ref->byte f 10))
(define (sdl-pixel-format->gshift f) (string-ref->byte f 11))
(define (sdl-pixel-format->bshift f) (string-ref->byte f 12))
(define (sdl-pixel-format->ashift f) (string-ref->byte f 13))
(define (sdl-pixel-format->rmask f) (string-ref->tetra f 14))
(define (sdl-pixel-format->gmask f) (string-ref->tetra f 18))
(define (sdl-pixel-format->bmask f) (string-ref->tetra f 22))
(define (sdl-pixel-format->amask f) (string-ref->tetra f 26))
(define (sdl-pixel-format->colorkey f) (string-ref->tetra f 30))
(define (sdl-pixel-format->alpha f) (string-ref->byte f 34))

(define (sdl-surface a) (make-immutable-string a 54))
(define (sdl-surface->flags s) (string-ref->tetra s 0))
(define (sdl-surface->format s) (sdl-pixel-format (string-ref->tetra s 4)))
(define (sdl-surface->w s) (string-ref->tetra s 8))
(define (sdl-surface->h s) (string-ref->tetra s 12))
(define (sdl-surface->pitch s) (string-ref->wyde s 16))
(define (sdl-surface->pixels s)
  (make-immutable-string
    (string-ref->tetra s 20)
    (* (sdl-pixel-format->bytes-per-pixel (sdl-surface->format s))
       (sdl-surface->w s)
       (sdl-surface->h s))))
(define (sdl-surface->clip-rect s) (string-ref->tetra s 32))

(define (make-sdl-event) (make-string 4))
(define (sdl-event->type e) (string-ref->byte e 0))
(define SDL_NOEVENT 0)
(define SDL_ACTIVEEVENT 1)
(define SDL_KEYDOWN 2)
(define SDL_KEYUP 3)
(define SDL_MOUSEMOTION 4)
(define SDL_MOUSEBUTTONDOWN 5)
(define SDL_MOUSEBUTTONUP 6)
(define SDL_JOYAXISMOTION 7)
(define SDL_JOYBALLMOTION 8)
(define SDL_JOYHATMOTION 9)
(define SDL_JOYBUTTONDOWN 10)
(define SDL_JOYBUTTONUP 11)
(define SDL_QUIT 12)
(define SDL_SYSWMEVENT 13)
(define SDL_VIDEORESIZE 16)
(define SDL_VIDEOEXPOSE 17)

(define (make-sdl-rects n) (make-string (* 8 n)))
(define (sdl-rects->x r n) (string-ref->wyde r (* 8 n)))
(define (sdl-rects->y r n) (string-ref->wyde r (+ (* 8 n) 2)))
(define (sdl-rects->w r n) (string-ref->wyde r (+ (* 8 n) 4)))
(define (sdl-rects->h r n) (string-ref->wyde r (+ (* 8 n) 6)))
(define (sdl-rects-set-x! r n x) (string-set-wyde! r (* 8 n) x))
(define (sdl-rects-set-y! r n y) (string-set-wyde! r (+ (* 8 n) 2) y))
(define (sdl-rects-set-w! r n w) (string-set-wyde! r (+ (* 8 n) 4) w))
(define (sdl-rects-set-h! r n h) (string-set-wyde! r (+ (* 8 n) 6) h))

(define (sdl-rect->x r) (string-ref->wyde r 0))
(define (sdl-rect->y r) (string-ref->wyde r 2))
(define (sdl-rect->w r) (string-ref->wyde r 4))
(define (sdl-rect->h r) (string-ref->wyde r 6))
(define (sdl-rect-set-x! r x) (string-set-wyde! r 0 x))
(define (sdl-rect-set-y! r y) (string-set-wyde! r 2 y))
(define (sdl-rect-set-w! r w) (string-set-wyde! r 4 w))
(define (sdl-rect-set-h! r h) (string-set-wyde! r 6 h))
(define (make-sdl-rect x y w h)
  (define r (make-string 8))
  (sdl-rect-set-x! r x)
  (sdl-rect-set-y! r y)
  (sdl-rect-set-w! r w)
  (sdl-rect-set-h! r h)
  r)

;;;;;;;;;;;;;;;;;
;;; Functions ;;;
;;;;;;;;;;;;;;;;;

(define SDL_INIT_TIMER       #x00000001)
(define SDL_INIT_AUDIO       #x00000010)
(define SDL_INIT_VIDEO       #x00000020)
(define SDL_INIT_CDROM       #x00000100)
(define SDL_INIT_JOYSTICK    #x00000200)
(define SDL_INIT_NOPARACHUTE #x00100000)
(define SDL_INIT_EVENTTHREAD #x01000000)
(define SDL_INIT_EVERYTHING  #x0000FFFF)
(define SDL_Init 
  (let ((f (dlsym *sdl* "SDL_Init")))
    (lambda (flags) (dlcall f flags))))

(define SDL_Quit
  (let ((f (dlsym *sdl* "SDL_Quit")))
    (lambda () (dlcall f))))

(define SDL_GetError
  (let ((f (dlsym *sdl* "SDL_GetError")))
    (lambda ()
      (define e (dlcall f))
      (if (zero? e) #f
        (make-immutable-string e)))))

;;;;;;;;;;;;;
;;; Video ;;;
;;;;;;;;;;;;;

(define SDL_SWSURFACE  #x00000000)
(define SDL_HWSURFACE  #x00000001)
(define SDL_ASYNCBLIT  #x00000004)
(define SDL_ANYFORMAT  #x10000000)
(define SDL_HWPALETTE  #x20000000)
(define SDL_DOUBLEBUF  #x40000000)
(define SDL_FULLSCREEN #x80000000)
(define SDL_OPENGL     #x00000002)
(define SDL_RESIZABLE  #x00000010)
(define SDL_NOFRAME    #x00000020)
(define SDL_SetVideoMode
  (let ((f (dlsym *sdl* "SDL_SetVideoMode")))
    (lambda (width height depth flags)
      (sdl-surface (dlcall f flags depth height width)))))

(define SDL_LockSurface
  (let ((f (dlsym *sdl* "SDL_LockSurface")))
    (lambda (s)
      (dlcall f s))))
(define SDL_UnlockSurface
  (let ((f (dlsym *sdl* "SDL_UnlockSurface")))
    (lambda (s)
      (dlcall f s))))

(define SDL_UpdateRect
  (let ((f (dlsym *sdl* "SDL_UpdateRect")))
    (lambda (screen x y w h)
      (dlcall f h w y x screen))))

(define SDL_UpdateRects
  (let ((f (dlsym *sdl* "SDL_UpdateRects")))
    (lambda (screen numrects rects)
      (dlcall f rects numrects screen))))

(define SDL_Flip
  (let ((f (dlsym *sdl* "SDL_Flip")))
    (lambda (surface)
      (dlcall f surface))))

(define SDL_RWFromFile
  (let ((f (dlsym *sdl* "SDL_RWFromFile")))
    (lambda (path mode)
      (dlcall f mode path))))

(define SDL_LoadBMP_RW
  (let ((f (dlsym *sdl* "SDL_LoadBMP_RW")))
    (lambda (src freesrc)
      (dlcall f freesrc src))))

(define SDL_UpperBlit
  (let ((f (dlsym *sdl* "SDL_UpperBlit")))
    (lambda (src srcrect dst dstrect)
      (dlcall f dstrect dst srcrect src))))

(define SDL_LowerBlit
  (let ((f (dlsym *sdl* "SDL_LowerBlit")))
    (lambda (src srcrect dst dstrect)
      (dlcall f dstrect dst srcrect src))))

(define SDL_FillRect
  (let ((f (dlsym *sdl* "SDL_FillRect")))
    (lambda (dst dstrect color)
      (dlcall f color dstrect dst))))

(define SDL_CreateRGBSurface
  (let ((f (dlsym *sdl* "SDL_CreateRGBSurface")))
    (lambda (flags width height depth rmask gmask bmask amask)
      (sdl-surface
        (dlcall f amask bmask gmask rmask depth height width flags)))))

(define SDL_FreeSurface
  (let ((f (dlsym *sdl* "SDL_FreeSurface")))
    (lambda (surface)
      (dlcall f surface))))

(define SDL_HWACCEL #x00000100)
(define SDL_SRCCOLORKEY #x00001000)
(define SDL_RLEACCEL #x00004000)
(define SDL_SetColorKey
  (let ((f (dlsym *sdl* "SDL_SetColorKey")))
    (lambda (surface flag key)
      (dlcall f key flag surface))))

(define SDL_DisplayFormat
  (let ((f (dlsym *sdl* "SDL_DisplayFormat")))
    (lambda (surface)
      (sdl-surface
        (dlcall f surface)))))

;;;;;;;;;;;;;;
;;; Events ;;;
;;;;;;;;;;;;;;

(define SDL_Delay
  (let ((f (dlsym *sdl* "SDL_Delay")))
    (lambda (t)
      (dlcall f t))))

(define SDL_PollEvent
  (let ((f (dlsym *sdl* "SDL_PollEvent")))
    (lambda (e)
      (dlcall f e))))

;;;;;;;;;;
;;; GL ;;;
;;;;;;;;;;

(define SDL_GL_LoadLibrary
  (let ((f (dlsym *sdl* "SDL_GL_LoadLibrary")))
    (lambda (path)
      (dlcall f path))))

(define SDL_GL_GetProcAddress
  (let ((f (dlsym *sdl* "SDL_GL_GetProcAddress")))
    (lambda (name)
      (dlcall f name))))

(define SDL_GL_SwapBuffers
  (let ((f (dlsym *sdl* "SDL_GL_SwapBuffers")))
    (lambda ()
      (dlcall f))))

(define SDL_GL_RED_SIZE 0)
(define SDL_GL_GREEN_SIZE 1)
(define SDL_GL_BLUE_SIZE 2)
(define SDL_GL_ALPHA_SIZE 3)
(define SDL_GL_BUFFER_SIZE 4)
(define SDL_GL_DOUBLEBUFFER 5)
(define SDL_GL_DEPTH_SIZE 6)
(define SDL_GL_STENCIL_SIZE 7)
(define SDL_GL_ACCUM_RED_SIZE 8)
(define SDL_GL_ACCUM_GREEN_SIZE 9)
(define SDL_GL_ACCUM_BLUE_SIZE 10)
(define SDL_GL_ACCUM_ALPHA_SIZE 11)
(define SDL_GL_STEREO 12)
(define SDL_GL_MULTISAMPLEBUFFERS 13)
(define SDL_GL_MULTISAMPLESAMPLES 14)
(define SDL_GL_ACCELERATED_VISUAL 15)
(define SDL_GL_SWAP_CONTROL 16)
(define SDL_GL_SetAttribute
  (let ((f (dlsym *sdl* "SDL_GL_SetAttribute")))
    (lambda (attr value)
      (dlcall f value attr))))

(define SDL_GL_GetAttribute
  (let ((f (dlsym *sdl* "SDL_GL_GetAttribute")))
    (lambda (attr)
      (dlcall f attr))))

;;;;;;;;;;;;;
;;; Audio ;;;
;;;;;;;;;;;;;
(define AUDIO_U8 #x0008)
(define AUDIO_S8 #x8008)
(define AUDIO_U16LSB #x0010)
(define AUDIO_S16LSB #x8010)
(define AUDIO_U16MSB #x1010)
(define AUDIO_S16MSB #x9010)
(define AUDIO_U16 AUDIO_U16LSB)
(define AUDIO_S16 AUDIO_S16LSB)

;;;;;;;;;;
;;; CD ;;;
;;;;;;;;;;

(define SDL_CDOpen
  (let ((f (dlsym *sdl* "SDL_CDOpen")))
    (lambda (n)
      (dlcall f n))))

(define SDL_CDEject
  (let ((f (dlsym *sdl* "SDL_CDEject")))
    (lambda (cd)
      (dlcall f cd))))

;;;;;;;;;;;;;;;
;;; Helpers ;;;
;;;;;;;;;;;;;;;

(define (SDL_LoadBMP path)
  (SDL_LoadBMP_RW (SDL_RWFromFile path "rb") 1))

(define SDL_BlitSurface SDL_UpperBlit)

(define *sdl-event* (make-sdl-event))
(define (clear-events)
  (if (zero? (SDL_PollEvent *sdl-event*))
    #t
    (clear-events)))
  
(define (make-plotter s)
  (let ((p (sdl-surface->pixels s))
        (pitch (sdl-surface->pitch s))
        (f (sdl-surface->format s)))
    (case (sdl-pixel-format->bits-per-pixel f)
      ((8)
       (lambda (x y c)
         (string-set-byte! p (+ (* y pitch) x) c)))
      ((16)
       (lambda (x y c)
         (string-set-wyde! p (+ (* y pitch) (* x 2)) c)))
      ((24)
       (lambda (x y c)
         (string-set-triad! p (+ (* y pitch) (* x 3)) c)))
      ((32)
       (lambda (x y c)
         (string-set-tetra! p (+ (* y pitch) (* x 4)) c))))))

(define (make-graph s right up)
  (let ((plot (make-plotter s))
        (width (sdl-surface->w s))
        (height (sdl-surface->h s)))
    (define (loop f x y)
      (cond
        ((>= x width) (loop f 0 (+ 1 y)))
        ((>= y height) (SDL_Flip s))
        (else
          (let ((c (f (- x right) (- y up))))
            (if c (plot x (- height y 1) c)))
          (loop f (+ 1 x) y))))
    (lambda (f) (loop f 0 0))))
