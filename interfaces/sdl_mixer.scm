;;;;;;;;;;;;;;;;;;;
;;; Dll Modules ;;;
;;;;;;;;;;;;;;;;;;;

;(define *mix-path* "sdl_mixer") ;Should be "libSDL_mixer-1.2.so.0" on Linux
(define *mix* (dlopen *mix-path*))
(if (zero? *mix*) (error "Could not open SDL_mixer library"))

(define MIX_DEFAULT_FREQUENCY 22050)

;;;;;;;;;;;;;;;;;
;;; Functions ;;;
;;;;;;;;;;;;;;;;;

(define Mix_OpenAudio
  (let ((f (dlsym *mix* "Mix_OpenAudio")))
    (lambda (rate format channels buffers)
      (dlcall f buffers channels format rate))))

(define Mix_CloseAudio
  (let ((f (dlsym *mix* "Mix_CloseAudio")))
    (lambda ()
      (dlcall f))))

(define Mix_LoadWAV_RW
  (let ((f (dlsym *mix* "Mix_LoadWAV_RW")))
    (lambda (src freesrc)
      (dlcall f freesrc src))))

(define Mix_LoadMUS
  (let ((f (dlsym *mix* "Mix_LoadMUS")))
    (lambda (file)
      (dlcall f file))))

(define Mix_PlayChannelTimed
  (let ((f (dlsym *mix* "Mix_PlayChannelTimed")))
    (lambda (channel chunk loops ticks)
      (dlcall f ticks loops chunk channel))))

(define Mix_PlayMusic
  (let ((f (dlsym *mix* "Mix_PlayMusic")))
    (lambda (music loops)
      (dlcall f loops music))))

(define Mix_HaltChannel
  (let ((f (dlsym *mix* "Mix_HaltChannel")))
    (lambda (channel)
      (dlcall f channel))))

(define Mix_HaltMusic
  (let ((f (dlsym *mix* "Mix_HaltMusic")))
    (lambda ()
      (dlcall f))))

(define Mix_FreeMusic
  (let ((f (dlsym *mix* "Mix_FreeMusic")))
    (lambda (music)
      (dlcall f music))))

(define Mix_Pause
  (let ((f (dlsym *mix* "Mix_Pause")))
    (lambda (channel)
      (dlcall f channel))))

(define Mix_Resume
  (let ((f (dlsym *mix* "Mix_Resume")))
    (lambda (channel)
      (dlcall f channel))))

;;;;;;;;;;;;;;;
;;; Helpers ;;;
;;;;;;;;;;;;;;;
(define (mix-load-wav file)
  (Mix_LoadWav_RW (SDL_RWFromFile file "rb") 1))
(define (mix-play-channel channel chunk loops)
  (Mix_PlayChannelTimed channel chunk loops -1))
