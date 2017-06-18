;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;; Char Primitives ;;
;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "char->integer")
  (call 'get_last_char_ascii)
  (mov (object INTEGER TEMP) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "integer->char")
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) TEMP)
(: 'return_char) ;return CHAR whose ascii is TEMP
  (and! #xff TEMP)
  (ascii-to-char TEMP VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "char-upcase")
  (call 'get_last_char_ascii)
  (cmpb #\a TEMP)
  (jb 'return_char)
  (cmpb #\z TEMP)
  (ja 'return_char)
  (subb 32 TEMP)
  (jmp 'return_char)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "char-downcase")
  (call 'get_last_char_ascii)
  (cmpb #\A TEMP)
  (jb 'return_char)
  (cmpb #\Z TEMP)
  (ja 'return_char)
  (addb 32 TEMP)
  (jmp 'return_char)
