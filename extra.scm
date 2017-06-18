;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; Bitwise Procedures ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(: 'return_exact_integer)
  (mov (object INTEGER VAL) VAL)
  (clear EXP)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "|")
  (clear VAL)
(: 'bitwise_or_loop)
  (test ARGL ARGL)
  (jz 'return_exact_integer)
  (call 'get_exact_natural)
  (or! (@ 4 TEMP) VAL)
  (jol 'error_overflow)
  (mov (@ 4 ARGL) ARGL)
  (jmp 'bitwise_or_loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "^")
  (clear VAL)
(: 'bitwise_xor_loop)
  (test ARGL ARGL)
  (jz 'return_exact_integer)
  (call 'get_exact_natural)
  (xor (@ 4 TEMP) VAL)
  (jol 'error_overflow)
  (mov (@ 4 ARGL) ARGL)
  (jmp 'bitwise_xor_loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "&")
  (mov -1 VAL)
(: 'bitwise_and_loop)
  (test ARGL ARGL)
  (jz 'return_exact_integer)
  (call 'get_exact_natural)
  (and! (@ 4 TEMP) VAL)
  (jol 'error_overflow)
  (mov (@ 4 ARGL) ARGL)
  (jmp 'bitwise_and_loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "~")
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) VAL)
  (not! VAL)
  (jmp 'return_exact_integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "make-immutable-string")
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov (@ 4 TEMP) VAL)
  (mov (@ 4 ARGL) ARGL)
  (test ARGL ARGL)
  (ifnz
    (begin
      (call 'get_last_exact_natural)
      (mov (@ 4 TEMP) TEMP)
      (cmp (power 2 (- 31 LENGTH_SHIFT)) TEMP)
      (jael 'error_invalid_index))
    (begin
     (: 'make_immutable_string)
      (mov -1 TEMP)
     (: 'make_immutable_string_loop)
      (inc TEMP)
      (cmpb 0 (@ 0 VAL TEMP))
      (jne 'make_immutable_string_loop)))
  (shl LENGTH_SHIFT TEMP)
  (add IMMUTABLE_STRING TEMP)
  (mov (object TEMP VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings as Structs ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "string-ref->byte")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;string argument
  (test EXP EXP)
  (jzl 'error_expected_string)
  (mov (@ 4 EXP) VAL) ;first character of string
  (mov (@ EXP) EXP)
  (cmpb TYPE_STRING EXP)
  (jnel 'error_expected_string)
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) TEMP) ;index
  (shr LENGTH_SHIFT EXP) ;length of string
  (cmp EXP TEMP)
  (jael 'error_invalid_index)
  (clear EXP)
  (add TEMP VAL)
  (movb (@ VAL) VAL)
  (and! #xff VAL)
  (mov (object INTEGER VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "string-ref->wyde")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;string argument
  (test EXP EXP)
  (jzl 'error_expected_string)
  (mov (@ 4 EXP) VAL) ;first character of string
  (mov (@ EXP) EXP)
  (cmpb TYPE_STRING EXP)
  (jnel 'error_expected_string)
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) TEMP) ;index
  (shr LENGTH_SHIFT EXP) ;length of string
  (dec EXP)
  (cmp EXP TEMP)
  (jael 'error_invalid_index)
  (clear EXP)
  (add TEMP VAL)
  (opd-size)(mov (@ VAL) VAL)
  (and! #xffff VAL)
  (mov (object INTEGER VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "string-ref->triad")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;string argument
  (test EXP EXP)
  (jzl 'error_expected_string)
  (mov (@ 4 EXP) VAL) ;first character of string
  (mov (@ EXP) EXP)
  (cmpb TYPE_STRING EXP)
  (jnel 'error_expected_string)
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) TEMP) ;index
  (shr LENGTH_SHIFT EXP) ;length of string
  (sub 2 EXP)
  (cmp EXP TEMP)
  (jael 'error_invalid_index)
  (clear EXP)
  (add TEMP VAL)
  (mov (@ VAL) VAL)
  (and! #xffffff VAL)
  (mov (object INTEGER VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "string-ref->tetra")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;string argument
  (test EXP EXP)
  (jzl 'error_expected_string)
  (mov (@ 4 EXP) VAL) ;first character of string
  (mov (@ EXP) EXP)
  (cmpb TYPE_STRING EXP)
  (jnel 'error_expected_string)
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) TEMP) ;index
  (shr LENGTH_SHIFT EXP) ;length of string
  (sub 3 EXP)
  (cmp EXP TEMP)
  (jael 'error_invalid_index)
  (clear EXP)
  (add TEMP VAL)
  (mov (@ VAL) VAL)
  (mov (object INTEGER VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "string-set-byte!")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;string argument
  (test EXP EXP)
  (jzl 'error_expected_string)
  (mov (@ 4 EXP) VAL) ;first character of string
  (mov (@ EXP) EXP)
  (cmpb TYPE_STRING EXP)
  (jnel 'error_expected_string)
  (mov (@ 4 ARGL) ARGL)
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov (@ 4 TEMP) TEMP) ;index
  (shr LENGTH_SHIFT EXP) ;length of string
  (cmp EXP TEMP)
  (jael 'error_invalid_index)
  (clear EXP)
  (add TEMP VAL)
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) TEMP)
  (movb TEMP (@ VAL))
  (clear VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "string-set-wyde!")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;string argument
  (test EXP EXP)
  (jzl 'error_expected_string)
  (mov (@ 4 EXP) VAL) ;first character of string
  (mov (@ EXP) EXP)
  (cmpb TYPE_STRING EXP)
  (jnel 'error_expected_string)
  (mov (@ 4 ARGL) ARGL)
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov (@ 4 TEMP) TEMP) ;index
  (shr LENGTH_SHIFT EXP) ;length of string
  (dec EXP)
  (cmp EXP TEMP)
  (jael 'error_invalid_index)
  (clear EXP)
  (add TEMP VAL)
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) TEMP)
  (opd-size)(mov TEMP (@ VAL))
  (clear VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "string-set-triad!")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;string argument
  (test EXP EXP)
  (jzl 'error_expected_string)
  (mov (@ 4 EXP) VAL) ;first character of string
  (mov (@ EXP) EXP)
  (cmpb TYPE_STRING EXP)
  (jnel 'error_expected_string)
  (mov (@ 4 ARGL) ARGL)
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov (@ 4 TEMP) TEMP) ;index
  (shr LENGTH_SHIFT EXP) ;length of string
  (sub 2 EXP)
  (cmp EXP TEMP)
  (jael 'error_invalid_index)
  (clear EXP)
  (add TEMP VAL)
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) TEMP)
  (opd-size)(mov TEMP (@ VAL))
  (shr 16 TEMP)
  (movb TEMP (@ 2 VAL))
  (clear VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "string-set-tetra!")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;string argument
  (test EXP EXP)
  (jzl 'error_expected_string)
  (mov (@ 4 EXP) VAL) ;first character of string
  (mov (@ EXP) EXP)
  (cmpb TYPE_STRING EXP)
  (jnel 'error_expected_string)
  (mov (@ 4 ARGL) ARGL)
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov (@ 4 TEMP) TEMP) ;index
  (shr LENGTH_SHIFT EXP) ;length of string
  (sub 3 EXP)
  (cmp EXP TEMP)
  (jael 'error_invalid_index)
  (clear EXP)
  (add TEMP VAL)
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) TEMP)
  (mov TEMP (@ VAL))
  (clear VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "string->address")
  (call 'get_last_string)
  (mov (@ 4 ARGL) VAL)
  (mov (object INTEGER VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "substring-set!")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;string argument
  (test EXP EXP)
  (jzl 'error_expected_string)
  (mov (@ 4 EXP) VAL) ;first character of string
  (mov (@ EXP) EXP)
  (cmpb TYPE_STRING EXP)
  (jnel 'error_expected_string)
  (mov (@ 4 ARGL) ARGL)
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov (@ 4 TEMP) TEMP) ;index
  (shr LENGTH_SHIFT EXP) ;length of string
  (cmp EXP TEMP)
  (jael 'error_invalid_index)
  (push TEMP)
  (add TEMP VAL) ;first character of substring to set
  (mov (@ 4 ARGL) ARGL)
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov (@ 4 TEMP) TEMP)
  (cmp EXP TEMP)
  (jal 'error_invalid_index)
  (pop EXP)
  (sub EXP TEMP) ;TEMP is now length of substring
  (ifnz 
    (begin
      (mov (@ 4 ARGL) ARGL)
      (insure-one-last-arg ARGL)
      (mov (@ ARGL) EXP) ;2nd string argument
      (test EXP EXP)
      (jzl 'error_expected_string)
      (mov (@ 4 EXP) UNEV) ;first character of 2nd string
      (mov (@ EXP) EXP)
      (cmpb TYPE_STRING EXP)
      (jnel 'error_expected_string)
      (shr LENGTH_SHIFT EXP) ;length of 2nd string
      (cmp EXP TEMP)
      (ifa (mov EXP TEMP))
      (mov VAL ARGL) 
      (rep)(movsb)))
  (clear VAL EXP UNEV ARGL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "string-hash")
  (call 'get_last_string)
  (mov (@ 4 ARGL) UNEV)
  (shr LENGTH_SHIFT TEMP)
  (clear ARGL EXP)
  (test TEMP TEMP)
  (jz 'string_hash_end)
(: 'string_hash_loop)
  (clear VAL)
  (lodsb)
  (mul TEMP)
  (add VAL ARGL)
  (loop 'string_hash_loop)
(: 'string_hash_end)
  (mov (object INTEGER ARGL) VAL)
  (clear EXP ARGL UNEV)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "string-ci->symbol")
  (call 'get_last_string)
  (mov (@ 4 ARGL) UNEV)
  (mov (@ 'freesymbol) ARGL)
  (cld)
  (mov SYMBOL (@ ARGL))
  (add 4 ARGL)
(: 'prim_string_ci_to_symbol_loop)
  (lodsb)
  (to_lower VAL)
  (stosb)
  (cmpb 0 (@ -1 UNEV))
  (jne 'prim_string_ci_to_symbol_loop)
  (jmpl 'read_name_end) ;UNEV will get cleared here
