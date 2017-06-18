;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This is a scheme interpreter written in x86 assembly     ;;;
;;;thanks to the description given by Abelson and Sussman in;;;
;;;Structure and Interpretation of Computer Programs.       ;;;
;;;                                                         ;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "x86.scm")
(define WINDOWS #f)
(define LIBFCGI #f) ;(if WINDOWS "libfcgi.dll" "libfcgi.so.0"))
(x86-set-text-start! (if WINDOWS #x400000 #x08048000))
(define program `(begin
  (define WINDOWS ,WINDOWS)
  (define LIBFCGI ,LIBFCGI)
  ;;;Set the following to the size desired in Kb.
  (define MEMSIZE 7000) ;objects
  (define SYMSIZE 128)  ;symbols
  (define STRSIZE 7000) ;strings
  (define DICTSIZE 4096) ;lambdas
  ;;; Hashtable for symbols will be 128*(2<<HASHPOWER) bytes long
  ;;; and will therefore accomodate 32*(2<<HASHPOWER) symbols
  (define HASHPOWER 7)
  ;;; Hashtable for top-level definitions will be a vector
  ;;; 2<<TOPLEVELPOWER elements long
  (define TOPLEVELPOWER 8)
  ;;;Set DEBUG to #t to force continual garbage collection for debugging:
  (define DEBUG #f)
  (define WELCOME "Dream version 2.71, Copyright (C) 2011 David Joseph Stith")
  (define BOOTSTRAP_FILE
    (if WINDOWS "C:\\dream\\bootstrap.scm" "/usr/local/bin/bootstrap.scm"))
  ;;;Set LIBDL to #f to disallow linking with dlopen, dlsym, dlclose.
  (define LIBDL "libdl.so.2")
  (define LIBGMP (if WINDOWS "libgmp-3.dll" "libgmp.so.3"))
  (for-each load
    (list
      (if WINDOWS "pe.scm" "elf.scm") "global.scm"
      (if WINDOWS "windows.scm" "linux.scm") "dream.scm"
      "compiler.scm" "number.scm" "gmp.scm" "pred.scm" "list.scm"
      "char.scm" "string.scm" "vector.scm" "control.scm" "extra.scm"
      "data.scm" "bss.scm"))
  (newline)
  (if (not WINDOWS)
    (begin
      (bss 'dlopen_rel 4)
      (bss 'dlclose_rel 4)
      (bss 'dlsym_rel 4)))))
(let ((code (x86-assemble program))
      (out
        (open-output-file
          (if WINDOWS
            (if LIBFCGI "dream_fcgi.exe" "dream.exe")
            (if LIBFCGI "dream_fcgi" "dream")))))
  (display code out)
  (close-output-port out))
