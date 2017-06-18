;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;; String Primitives ;;
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "string-ref")
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
  (ascii-to-char VAL VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "string-set!")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;string argument
  (test EXP EXP)
  (jzl 'error_expected_string)
  (mov (@ 4 EXP) VAL) ;first character of string
  (mov (@ EXP) EXP)
  (cmpb TYPE_STRING EXP)
  (jnel 'error_expected_string)
  (test STRING_IMMUTABILITY EXP)
  (jnzl 'error_expected_mutable_string)
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
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) TEMP) ;char argument
  (fetch-char TEMP TEMP)
  (movb TEMPH (@ VAL))
  (clear VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "string-length")
  (call 'get_last_string)
  (shr LENGTH_SHIFT TEMP) ;TEMP now holds length of string or vector
  (mov (object INTEGER TEMP) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "string->symbol")
  (call 'get_last_string)
  (mov (@ 4 ARGL) UNEV)
  (mov (@ 'freesymbol) ARGL)
  (cld)
  (mov SYMBOL (@ ARGL))
  (add 4 ARGL)
(: 'prim_string_to_symbol_loop)
  (movsb)
  (cmpb 0 (@ -1 UNEV))
  (jne 'prim_string_to_symbol_loop)
  (jmpl 'read_name_end) ;UNEV will get cleared here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "symbol->string")
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) VAL)
  (test VAL VAL)
  (jzl 'error_expected_symbol)
  (mov (@ VAL) TEMP)
  (cmpb SYMBOL TEMP)
  (jnel 'error_expected_symbol)
  (lea (@ 4 VAL) TEMP)
  (mov TEMP (@ 4 FREE))
  (clear VAL)
(: 'prim_symbol_to_string_loop)
  (cmpb 0 (@ TEMP))
  (ifne
    (begin
      (inc TEMP) 
      (inc VAL)
      (jmp 'prim_symbol_to_string_loop)))
  (shl LENGTH_SHIFT VAL)
  (add IMMUTABLE_STRING VAL)
  (mov VAL (@ FREE))
  (mov FREE VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "make-string")
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov (@ 4 ARGL) ARGL)
  (mov TEMP EXP)
  (mov (@ 4 EXP) TEMP) ;length of string-to-be
  (cmp (power 2 (- 31 LENGTH_SHIFT)) TEMP)
  (jael 'error_string_too_large)
  (call 'insure_string_space)
  (cld)
  (test ARGL ARGL)
  (ifz
    (begin
      (mov (@ 'freestring) ARGL)
      (mov (@ 4 EXP) TEMP)
      (add TEMP ARGL))
    (begin
      (insure-one-last-arg ARGL)
      (mov (@ ARGL) TEMP)
      (fetch-char TEMP TEMP)
      (movb TEMPH VALL)
      (mov (@ 'freestring) ARGL)
      (mov (@ 4 EXP) TEMP)
      (rep)
      (stosb)))
  (clear VAL)
  (stosb) ;end string
  (mov (@ 'freestring) VAL)
  (mov ARGL (@ 'freestring))
  (mov (@ 4 EXP) TEMP)
  (shl LENGTH_SHIFT TEMP)
  (add MUTABLE_STRING TEMP)
  (mov TEMP (@ FREE))
  (mov VAL (@ 4 FREE))
  (mov FREE VAL)
  (clear ARGL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'prim_rational_to_string)
  (mov (@ 4 VAL) VAL) ;(numerator . denominator)
  (save VAL)
  (mov (@ 4 VAL) VAL) ;denominator
  (call 'prim_integer_to_string)
  (restore EXP) ;(numerator . denominator)
  (save VAL) ;stringified denominator
  (mov (@ EXP) VAL) ;numerator
  (call 'prim_integer_to_string)
  (restore EXP) ;stringified denominator
  (mov (object EXP 0) ARGL)
  (call 'advance_free) ;ARGL <= (denominator)
  (mov (object 'string_slash ARGL) ARGL)
  (call 'advance_free) ;ARGL <= ("/" denominator)
  (mov (object VAL ARGL) ARGL)
  (call 'advance_free) ;ARGL <= (numerator "/" denominator)
  ;fall through to string-append

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "string-append")
  (clear TEMP)
  (mov ARGL UNEV)
  (jmp 'prim_string_append_length_begin)

(: 'prim_string_append_length_loop)
  (mov (@ UNEV) VAL)
  (test VAL VAL)
  (jzl 'error_expected_string)
  (mov (@ VAL) VAL)
  (cmpb TYPE_STRING VAL)
  (jnel 'error_expected_string)
  (shr LENGTH_SHIFT VAL)
  (add VAL TEMP)
  (mov (@ 4 UNEV) UNEV)
(: 'prim_string_append_length_begin)
  (test UNEV UNEV)
  (jnzl 'prim_string_append_length_loop)
  (clear VAL)
  (call 'insure_string_space)
  (shl LENGTH_SHIFT TEMP)
  (add MUTABLE_STRING TEMP)
  (mov TEMP (@ FREE))
  (mov ARGL VAL)
  (mov (@ 'freestring) ARGL)
  (mov ARGL (@ 4 FREE))
  (jmp 'prim_string_append_concat_begin)

(: 'prim_string_append_concat_loop)
  (mov (@ VAL) UNEV)
  (mov (@ UNEV) TEMP)
  (mov (@ 4 UNEV) UNEV)
  (shr LENGTH_SHIFT TEMP)
  (rep)
  (movsb)
  (mov (@ 4 VAL) VAL)
(: 'prim_string_append_concat_begin)
  (test VAL VAL)
  (jnz 'prim_string_append_concat_loop)
  (movb 0 (@ ARGL))
  (inc ARGL)
  (mov ARGL (@ 'freestring))
  (clear UNEV ARGL)
  (mov FREE VAL)
  (jmpl 'advance_free)

(new-primitive "number->string")
  (insure-more-args ARGL)
  (call 'get_number)
  (mov 10 (@ 'radix))
  (mov (@ 4 ARGL) ARGL)
  (test ARGL ARGL)
  (ifnz
    (begin
      (call 'get_last_exact_natural)
      (mov (@ 4 TEMP) TEMP)
      (mov TEMP (@ 'radix))))
  (mov (@ VAL) TEMP)
  (opd-size)(cmp RATIONAL TEMP)
  (jel 'prim_rational_to_string)
(: 'prim_integer_to_string)
  (call 'stringify_integer)
  (push TEMP) ;pointer to string data
  (mov (@ 'str_len) TEMP) ;now TEMP is length of string
  (push TEMP)
  (call 'insure_string_space)
  (pop TEMP)
  (mov TEMP ARGL)
  (shl LENGTH_SHIFT ARGL)
  (add MUTABLE_STRING ARGL)
  (mov ARGL (@ FREE))
  (mov (@ 'freestring) ARGL)
  (mov ARGL (@ 4 FREE))
  (pop UNEV)
  (inc TEMP)
  (rep)
  (movsb)
  (mov ARGL (@ 'freestring))
  (clear ARGL UNEV)
  (mov FREE VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "string->number")
  (insure-more-args ARGL)
  (mov (@ ARGL) UNEV)
  (test UNEV UNEV)
  (jzl 'error_expected_string)
  (mov (@ UNEV) TEMP)
  (cmpb TYPE_STRING TEMP)
  (jnel 'error_expected_string)
  (mov 10 (@ 'radix)) ;default radix (decimal)
  (mov (@ 4 ARGL) ARGL)
  (test ARGL ARGL)
  (ifnz
    (begin
      (call 'get_last_exact_natural)
      (mov (@ 4 TEMP) TEMP)
      (mov TEMP (@ 'radix))))
  (mov 0 (@ 'input))
  (mov 0 (@ 'bytes_io))
  (mov 'prim_string_to_number_stream (@ 'thunk))
  (call 'prim_string_to_number_stream)
  (mov INTEGER (@ FREE))
  (call 'stream_to_integer)
  (test VAL VAL)
  (jz 'prim_string_to_number_failure)
  (cmpb 0 (@ 'input))
  (jz 'prim_string_to_number_integer)
  (cmpb #\/ (@ 'input))
  (jne 'prim_string_to_number_failure)
  (push VAL)
  (call 'prim_string_to_number_stream)
  (mov INTEGER (@ FREE))
  (call 'stream_to_integer)
  (test VAL VAL)
  (ifnz
    (begin
      (cmpb 0 (@ 'input))
      (jz 'prim_string_to_number_rational)))
  (pop EXP)
(: 'prim_string_to_number_failure)
  (clear EXP)
  (return-false)

(: 'prim_string_to_number_integer)
  (clear UNEV)
  (ret)

(: 'prim_string_to_number_rational)
  (pop EXP)
  (mov (object VAL 0) UNEV)
  (call 'advance_free)
  (mov (object EXP UNEV) UNEV)
  (call 'advance_free)
  (save ENV)
  (mov (symbol-name "/") EXP)
  (mov (@ 'interaction_environment) ENV)
  (call 'ev_variable)
  (call 'apply_procedure)
  (restore ENV)
  (ret)

(: 'prim_string_to_number_stream)
  (mov (@ 4 UNEV) TEMP)
  (add (@ 'bytes_io) TEMP)
  (movb (@ TEMP) TEMP)
  (movb TEMP (@ 'input))
  (inc (@ 'bytes_io))
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "string->list")
  (call 'get_last_string)
  (shr LENGTH_SHIFT TEMP) ;string length
  (mov TEMP (@ 'str_len))
  (clear VAL)
  (jmp 'string_to_list_loop_begin)

(: 'string_to_list_loop)
  (add (@ 4 ARGL) TEMP) ;address of current character
  (movzb (@ TEMP) TEMP)
  (ascii-to-char TEMP TEMP)
  (mov (object TEMP VAL) VAL)
  (call 'advance_free)
(: 'string_to_list_loop_begin)
  (dec (@ 'str_len))
  (mov (@ 'str_len) TEMP)
  (test TEMP TEMP)
  (jns 'string_to_list_loop)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "list->string")
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) ARGL) ;list argument
(new-primitive "string")
  (mov ARGL VAL)
  (clear TEMP)
  (jmp 'list_to_string_length_loop_begin)

(: 'list_to_string_length_loop)
  (insure-object-is-pair VAL)
  (mov (@ 4 VAL) VAL)
  (inc TEMP)
(: 'list_to_string_length_loop_begin)
  (test VAL VAL)
  (jnz 'list_to_string_length_loop)

  (call 'insure_string_space)
  (shl LENGTH_SHIFT TEMP)
  (add MUTABLE_STRING TEMP)
  (mov TEMP (@ FREE))
  (mov (@ 'freestring) TEMP)
  (mov TEMP (@ 4 FREE))
  (jmp 'list_to_string_loop_begin)

(: 'list_to_string_loop)
  (mov (@ ARGL) VAL) ;char
  (fetch-char VAL VAL)
  (movb VALH (@ TEMP))
  (mov (@ 4 ARGL) ARGL)
  (inc TEMP)
(: 'list_to_string_loop_begin)
  (test ARGL ARGL)
  (jnz 'list_to_string_loop)

  (mov 0 (@ TEMP))
  (inc TEMP)
  (mov TEMP (@ 'freestring))
  (mov FREE VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "substring")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;string
  (test EXP EXP)
  (jzl 'error_expected_string)
  (mov (@ EXP) TEMP)
  (cmpb TYPE_STRING TEMP)
  (jnel 'error_expected_string)
  (shr LENGTH_SHIFT TEMP) ;string length
  (mov TEMP (@ 'str_len))
  (mov (@ 4 ARGL) ARGL)
  (insure-more-args ARGL)
  (call 'get_exact_natural) ;TEMP <- start index
  (mov (@ 4 TEMP) TEMP)
  (test TEMP TEMP)
  (jsl 'error_invalid_index)
  (cmp TEMP (@ 'str_len))
  (jll 'error_invalid_index)
  (add (@ 4 EXP) TEMP) ;start address
  (mov TEMP UNEV)
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_exact_natural) ;TEMP <- end index
  (mov (@ 4 TEMP) TEMP)
  (cmp TEMP (@ 'str_len))
  (jll 'error_invalid_index)
  (add (@ 4 EXP) TEMP) ;end address
  (sub UNEV TEMP) ;substring length
  (jsl 'error_invalid_index)
 (: 'substring_unev_temp)
  (push UNEV)
  (clear UNEV)
  (call 'insure_string_space)
  (pop UNEV)
  (mov TEMP VAL)
  (shl LENGTH_SHIFT VAL)
  (add MUTABLE_STRING VAL)
  (mov VAL (@ FREE))
  (mov (@ 'freestring) ARGL)
  (mov ARGL (@ 4 FREE))
  (mov FREE VAL)
  (rep)
  (movsb)
  (movb 0 (@ ARGL))
  (inc ARGL)
  (mov ARGL (@ 'freestring))
  (clear ARGL UNEV)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "string-copy")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;string
  (test EXP EXP)
  (jzl 'error_expected_string)
  (mov (@ EXP) TEMP)
  (cmpb TYPE_STRING TEMP)
  (jnel 'error_expected_string)
  (shr LENGTH_SHIFT TEMP) ;string length
  (mov (@ 4 EXP) UNEV)
  (jmp 'substring_unev_temp)
