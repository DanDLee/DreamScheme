;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Initialize Global Symbols;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'start)
  (mov (@ 'mem) FREE)
  (mov 'obhash ARGL)

(define special-symbols
 '("space" ")" "." "unquote" "unquote-splicing"))

(for-each
  (lambda (s)
    (mov (symbol-name s) VAL)
    (call 'intern_new_symbol))
  special-symbols)

  (call 'initialize_ports)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Initialize Numeric Tower Procedure Vector;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (mov FREE (@ 'tower_procedures))
  (mov (+ SCHEME_VECTOR #x12000) (@ FREE)) ;vector of length 9
  (mov 9 TEMP)
(: 'init_tower_procedures_loop)
  (add 4 FREE)
  (mov (proc-name "expected-integer") (@ FREE))
  (loop 'init_tower_procedures_loop)
  (add 4 FREE)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Initialize Environments;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (mov 'builtins UNEV) ;for lodsl
  (mov FREE EXP)
  ;Make interaction_environment VECTOR
  (mov (+ SCHEME_VECTOR (* TOPLEVEL_SIZE #x10000)) (@ EXP))
  (add (+ 8 (* 4 TOPLEVEL_SIZE)) FREE)

(: 'install_builtin_loop)
  (lods)
  (test VAL VAL)
  (ifnz
    (begin
      (call 'intern_new_symbol)
      (mov VAL (@ FREE))
      (and! TOPLEVEL_HASH_MASK VAL)
      (lea (@ 4 EXP VAL 0) TEMP)
      (mov (@ TEMP) ENV)
      (mov UNEV (@ 4 FREE))
      (mov FREE (@ 8 FREE)) ;((symbol . value) . ENV)
      (add 8 FREE)
      (mov ENV (@ 4 FREE))
      (mov FREE (@ TEMP))
      (add 8 FREE)
      (add 8 UNEV)
     (jmp 'install_builtin_loop)))
  (mov 'memstr1 (@ 'freestring))
  (mov 'dictionary (@ 'freedict))
  (mov 'dictionarylimit (@ 'freedictend))
  (mov (object EXP 0) ENV)
  (add 8 FREE)
  (mov ENV (@ 'interaction_environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Initialize Chararray;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
  (mov 'chararray ARGL)
  (mov CHAR VAL)
  (xorb VALH VALH)
(: 'chararray_loop)
  (stos)
  (incb VALH)
  (jne 'chararray_loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Initialize Scheme Registers;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (clear VAL EXP ARGL UNEV)
  (clc)
  (mov SP (@ 'stackbase))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Load Bootstrap File;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(if BOOTSTRAP_FILE
 (begin
  (mov 'welcome (@ 'error_continuation))
  (mov 'bootstrap_file TEMP)
  (mov (string-length BOOTSTRAP_FILE) (@ 'str_len))
  (call 'prim_load_scratch)))

  (call 'parse_command_line)

;;;;;;;;;;;;;
;;;Welcome;;;
;;;;;;;;;;;;;
(: 'welcome)
  (call 'set_output_port_current)
  (new-message 'msg_welcome WELCOME)
  (init-message 'msg_welcome)
  (call 'puts)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Read-Eval-Print Loop;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

  (new-message 'prompt "\ndream> ")

  (mov 'read_eval_print_loop (@ 'error_continuation))
(: 'read_eval_print_loop)
  (call 'set_output_port_current)
  (mov 0 (@ 'load_filename))
  (init-message 'prompt)
  (call 'puts)
  (call 'set_input_port_current)
  (call 'prim_read_argl)
  (cmp 'eof VAL)
  (jel 'exit_ok)
  (cmp (string->symbol "sym_)") VAL)
  (jel 'error_extra_eol)

  (mov VAL EXP)
  (mov (@ 'interaction_environment) ENV)
  (call 'eval_dispatch)
  (call 'set_output_port_current)
  (call 'write_dispatch)
  (jmp 'read_eval_print_loop)

(: 'exit_ok)
  (exit-with-code 0)
(: 'exit_not_ok)
  (exit-with-code -1)

;;;;;;;;;;;;;;;
;;;Utilities;;;
;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Intern new symbol VAL in hashtable at ARGL
;;VAL *MUST* be a NEW symbol
(: 'intern_new_symbol)
  (movb (@ 4 VAL) TEMP)
  (and! #x7f TEMP)
  (shl HASHPOWER TEMP)
  (add ARGL TEMP)
(: 'intern_new_symbol_loop)
  (cmp 0 (@ TEMP))
  (ifne
    (begin
      (add 4 TEMP)
      (jmp 'intern_new_symbol_loop)))
  (mov VAL (@ TEMP))
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Advance FREE pointer to next available space for pair storage.
(: 'advance_free)
  (add 8 FREE)
  (cmp (@ 'memlimit) FREE)
  ((if DEBUG jmpl jgel) 'gc_sans_strings)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Insure that there are TEMP bytes available for string storage.
(: 'insure_string_space)
  (push TEMP)
  (test TEMP TEMP)
  (jsl 'error_out_of_memory)
  (add (@ 'freestring) TEMP)
  (cmp (@ 'memstrlimit) TEMP)
  (if (not DEBUG) (jl 'insure_string_space_ok))
  (call 'gc_strings)
  (mov (@ SP) TEMP)
  (add (@ 'freestring) TEMP)
  (cmp (@ 'memstrlimit) TEMP)
  (jgl 'error_out_of_memory)
(: 'insure_string_space_ok)
  (pop TEMP)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Return in TEMP the string value of INTEGER VAL using radix
;;Length of string returned in (@ 'str_len)
;;Destroys VAL, ARGL, EXP, and UNEV in the process.
(: 'stringify_integer)
  (test #x00020000 (@ VAL)) ;sign-bit
  (jz 'stringify_integer_abs)
  (call 'stringify_integer_abs)
  (dec TEMP)
  (movb #\- (@ TEMP))
  (inc (@ 'str_len))
  (ret)

(: 'stringify_integer_abs)
  (mov (@ VAL) TEMP)
  (test #xfffc0000 TEMP)
  (jz 'stringify_integer_single)
  (mov (@ 'memstrnew) ARGL)
  (mov (@ 4 VAL) UNEV)
  (shr 18 TEMP)
  (mov TEMP (@ 'str_len))
  (rep)(movs)
  (mov (@ 'memstrlimitnew) ARGL)
  (movb 0 (@ ARGL))
  (mov (@ 'memstrnew) UNEV)
  (mov (@ 'str_len) TEMP)
(: 'stringify_integer_big_loop)
  (dec ARGL)
  (clear EXP)
(: 'stringify_integer_big_divide_loop)
  (dec TEMP)
  (mov (@ 0 UNEV TEMP 2) VAL)
  (div (@ 'radix))
  (mov VAL (@ 0 UNEV TEMP 2))
  (test TEMP TEMP)
  (jnz 'stringify_integer_big_divide_loop)
  (opd-size)(add #\0 EXP)
  (opd-size)(cmp #\9 EXP)
  (jle 'stringify_integer_big_digit)
  (opd-size)(add 39 EXP) ;Advance to letters for digits greater than '9'
(: 'stringify_integer_big_digit)
  (movb EXP (@ ARGL))
  (mov (@ 'str_len) TEMP)
  (cmp 0 (@ -4 UNEV TEMP 2))
  (jne 'stringify_integer_big_loop)
  (dec TEMP)
  (mov TEMP (@ 'str_len))
  (jnz 'stringify_integer_big_loop)
  (clear UNEV)
  (jmp 'stringify_integer_end)

(: 'stringify_integer_single)
  (mov (@ 4 VAL) VAL)
(: 'stringify_integer32)
  (mov (@ 'memstrlimitnew) ARGL)
  (movb 0 (@ ARGL))
(: 'stringify_integer_loop)
  (dec ARGL)
  (clear EXP)
  (div (@ 'radix))
  (opd-size)(add #\0 EXP)
  (opd-size)(cmp #\9 EXP)
  (jle 'stringify_integer_digit)
  (opd-size)(add 39 EXP) ;Advance to letters for digits greater than '9'
(: 'stringify_integer_digit)
  (movb EXP (@ ARGL))
  (test VAL VAL)
  (jnz 'stringify_integer_loop)

(: 'stringify_integer_end)
  (mov ARGL TEMP)
  (sub (@ 'memstrlimitnew) ARGL)
  (neg ARGL)
  (mov ARGL (@ 'str_len))
  (clear VAL ARGL EXP)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Return INTEGER in VAL obtained by converting, according to
;;(@ 'radix), the ascii digits streaming into the byte
;;(@ 'input) from calls to (@ 'thunk).
;;VAL is set to '() on return upon failure.
(: 'stream_to_integer)
  (cmp 0 (@ 'input))
  (jzl 'stream_to_integer_error) ;empty stream
(: 'stream_to_integer_nonempty)
  (cmpb #\- (@ 'input))
  (jne 'stream_to_integer_absolute)
  (xor #x00020000 (@ FREE))
  (calln (@ 'thunk))
  (cmp 0 (@ 'input))
  (jle 'stream_to_integer_error)
(: 'stream_to_integer_absolute)
  (clear VAL)
(: 'stream_to_integer_loop)
  (mov (@ 'input) TEMP)
  (cmp -1 TEMP)
  (je 'stream_to_integer_end)

  (for-each
    (lambda (token)
      (cmpb token TEMP)
      (je 'stream_to_integer_end))
    '(0 #\space #\newline 13 9 #\/ #\( #\) #\"))

  (cmpb #\0 TEMP)
  (jb 'stream_to_integer_error)
  (subb #\0 TEMP)
  (cmpb 9 TEMP)
  (ja 'stream_to_integer_alpha_digit)
(: 'stream_to_integer_digit)
  (cmp (@ 'radix) TEMP)
  (jae 'stream_to_integer_error)
  (test #xfffc0000 (@ FREE))
  (jnz 'stream_to_integer_big)
  (mul (@ 'radix))
  (add TEMP VAL)
  (adc 0 EXP)
  (jnz 'stream_to_integer_promote)
  (calln (@ 'thunk))
  (jmp 'stream_to_integer_loop)

(: 'stream_to_integer_error)
  (clear EXP VAL)
  (ret)

(: 'stream_to_integer_alpha_digit)
  (cmpb 49 TEMP) ;adjusted #\a
  (jae 'stream_to_integer_lower_alpha_digit)
  (cmpb 17 TEMP) ;adjusted #\A
  (jb 'stream_to_integer_error)
  (subb 7 TEMP)
  (jmp 'stream_to_integer_digit)

(: 'stream_to_integer_lower_alpha_digit)
  (subb 39 TEMP)
  (jmp 'stream_to_integer_digit)

(: 'stream_to_integer_end)
  (clear EXP)
  (test VAL VAL)
  (jnz 'stream_to_integer_nonzero)
  (and! (~ #x00020000) (@ FREE)) ;prevent -0
(: 'stream_to_integer_nonzero)
  (mov VAL (@ 4 FREE))
  (mov FREE VAL)
  (mov (@ FREE) TEMP)
  (shr 16 TEMP)
  (and! #xfffc TEMP)
  (add TEMP (@ 'freestring))
  (jmpl 'advance_free)

(: 'stream_to_integer_promote)
  (mov VAL TEMP)
  (mov (@ 'freestring) VAL)
  (mov TEMP (@ VAL))
  (mov EXP (@ 4 VAL))
  (add #x80000 (@ FREE))
  (calln (@ 'thunk))
  (jmpl 'stream_to_integer_loop)

(: 'stream_to_integer_big)
  (mov (@ FREE) EXP)
  (shr 18 EXP)
  (mov EXP (@ 'str_len))
  (push UNEV) ;for thunk
  (mov VAL UNEV) ;integer data
  (push VAL)
(: 'stream_to_integer_big_loop)
  (lods)
  (mul (@ 'radix))
  (add TEMP VAL)
  (adc 0 EXP)
  (mov EXP TEMP)
  (mov VAL (@ -4 UNEV))
  (dec (@ 'str_len))
  (jnz 'stream_to_integer_big_loop)
  (pop VAL)
  (test TEMP TEMP)
  (jz 'stream_to_integer_big_next)
  (add #x40000 (@ FREE))
  (jsl 'error_overflow)
  (mov TEMP (@ UNEV))
  (cmp (@ 'memstrlimit) UNEV)
  (if (not DEBUG) (jl 'stream_to_integer_big_next))
  (pop UNEV) ;need to save for thunk
  (push VAL) ;integer data
  (clear EXP VAL)
  (push (@ FREE)) ;integer header
  (call 'gc_strings)
  (pop TEMP) ;integer header
  (mov TEMP (@ FREE))
  (pop VAL) ;integer data
  (push UNEV)
  (mov VAL UNEV)
  (shr 18 TEMP)
  (mov (@ 'freestring) ARGL)
  (rep)(movs)
  (cmp (@ 'memstrlimit) ARGL)
  (jael 'error_out_of_memory)
  (mov (@ 'freestring) VAL)
  (clear ARGL)
(: 'stream_to_integer_big_next)
  (pop UNEV)
  (calln (@ 'thunk))
  (jmpl 'stream_to_integer_loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Return at VAL list at ARGL reversed 
;;Leaves EXP and UNEV untouched
(: 'reverse)
  (clear VAL)
  (jmp 'reverse_begin)

(: 'reverse_loop)
  (mov (@ ARGL) TEMP)
  (mov (object TEMP VAL) VAL)
  (mov (@ 4 ARGL) ARGL)
  (call 'advance_free)
(: 'reverse_begin)
  (test ARGL ARGL)
  (jnz 'reverse_loop)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Return vector at VAL created from list at VAL
(: 'list_to_vector)
  (mov VAL UNEV) ;we will step through the list using UNEV
  (mov FREE TEMP) ;start of vector
(: 'list_to_vector_loop)
  (test UNEV UNEV)
  (jz 'list_to_vector_end)
  (add 4 FREE)
  (cmp (@ 'memlimit) FREE)
  (jae 'list_to_vector_gc)
  (mov (@ UNEV) EXP)
  (mov EXP (@ FREE))
  (mov (@ 4 UNEV) UNEV)
  (jmp 'list_to_vector_loop)

(: 'list_to_vector_gc)
  (mov TEMP FREE)
  (mov (@ 'memlimit) TEMP)
  (sub FREE TEMP)
  (push TEMP) ;remember how much room we had
  (call 'gc_sans_strings)
  (mov (@ 'memlimit) TEMP)
  (sub FREE TEMP)
  (cmp (@ SP) TEMP) ;do we have more room now?
  (jbel 'error_out_of_memory)
  (add 4 SP)
  (jmp 'list_to_vector)

(: 'list_to_vector_end)
  (mov FREE VAL)
  (sub TEMP VAL)
  (shl (- LENGTH_SHIFT 2) VAL) ;divide by 4 and shift to high word
  (add SCHEME_VECTOR VAL)
  (mov VAL (@ TEMP))
  (mov TEMP VAL)
  (add 4 FREE)
  (test 7 FREE)
  (jz 'list_to_vector_aligned)
  (mov 0 (@ FREE))
  (add 4 FREE)
(: 'list_to_vector_aligned)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Return list at VAL created from vector at EXP
(: 'vector_to_list)
  (clear VAL)
  (shr LENGTH_SHIFT TEMP) ;now TEMP holds vector length
  (jz 'vector_to_list_end)
(: 'vector_to_list_loop)
  (mov TEMP (@ 'backup))
  (mov (@ 0 EXP TEMP 2) TEMP) ;(vector-ref (- TEMP 1) EXP)
  (mov (object TEMP VAL) VAL)
  (call 'advance_free)
  (mov (@ 'backup) TEMP)
  (opd-size)(dec TEMP)
  (jnz 'vector_to_list_loop)
(: 'vector_to_list_end)
  (ret)

(new-additional-primitive "gc-strings")
;;;;;;;;;;;;;;;;;;;;;;;;
;;;Garbage Collection;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;Perform garbage collection on both pair and string storage.
(: 'gc_strings)
  (push (@ 'memstrnew))
  (jmp 'gc)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Perform garbage collection on only pair storage.
(: 'gc_sans_strings)
  (push 0)
(: 'gc)
  (stack_push EXP ENV VAL ARGL UNEV)
  (mov (@ 'interaction_environment) VAL)
  (stack_push VAL)
  (mov (@ 'current_input_port) VAL)
  (stack_push VAL)
  (mov (@ 'current_output_port) VAL)
  (stack_push VAL)
  (mov (@ 'tower_procedures) VAL)
  (stack_push VAL)
  (pop TEMP)
  (mov (@ 'memnew) FREE)
  (mov (@ 'memnew) SCAN)
  (mov (@ 'root) OLD)
  (call 'relocate_old_result_in_new)
  (mov NEW (@ 'root))
  (mov 0 (@ 'lambda_list))
(: 'gc_loop)
  (cmp FREE SCAN)
  (jel 'gc_flip)
  (mov (@ SCAN) OLD) ;OLD <- (car SCAN)
  (test 1 OLD)
  (jnz 'gc_non_pair)
  (test OLD OLD)
  (jz 'gc_relocate_cdr)
  (call 'relocate_old_result_in_new)
  (mov NEW (@ SCAN)) ;(set-car! SCAN NEW)
(: 'gc_relocate_cdr)
  (mov (@ 4 SCAN) OLD) ;OLD <- (cdr SCAN)
  (test OLD OLD)
  (jz 'gc_next)
  (call 'relocate_old_result_in_new)
  (mov NEW (@ 4 SCAN)) ;(set-cdr! SCAN NEW)
(: 'gc_next)
  (add 8 SCAN)
  (jmp 'gc_loop)
(: 'gc_non_pair)
  (test OLD OLD)
  (js 'gc_next) ;Actually this should never happen
  (test #x100 OLD)
  (jnz 'gc_relocate_cdr)
  (jecxz 'gc_next)
  (opd-size)(cmp INTEGER OLD)
  (jne 'gc_relocate_string)
  (test #xfffc0000 OLD)
  (jz 'gc_next) ;Big INTEGERs have non-zero length
  (and! (~ #x00030000) OLD) ;ignore sign and exactness bit
  (shr (- 16 LENGTH_SHIFT) OLD) ;make length as if a string length
  (jmp 'gc_relocate_string_data)
(: 'gc_relocate_string)
  (mov OLD EXP)
  (cmpb TYPE_STRING EXP)
  (jne 'gc_next)
  (test STRING_IMMUTABILITY OLD)
  (jnz 'gc_next)
(: 'gc_relocate_string_data)
  (mov TEMP NEW)
  (mov OLD TEMP)
  (shr LENGTH_SHIFT TEMP)
  (inc TEMP) ;Now TEMP is length of string + terminating null char
  (mov (@ 4 SCAN) OLD) ;OLD now points to original string
  (mov NEW (@ 4 SCAN))
  (cld)
(: 'gc_relocate_string_loop)
  (rep)(movsb)
  (mov NEW TEMP)
  (jmp 'gc_next)

(: 'gc_flip)
  (mov (@ 'mem) OLD)
  (mov (@ 'memnew) NEW)
  (mov NEW (@ 'mem))
  (mov OLD (@ 'memnew)) ;mem <-> memnew
  (mov (@ 'memlimit) OLD)
  (mov (@ 'memlimitnew) NEW)
  (mov NEW (@ 'memlimit))
  (mov OLD (@ 'memlimitnew)) ;memlimit <-> memlimitnew
  (cmp NEW FREE)
  (jgel 'error_out_of_memory)
  (jecxz 'gc_flip_ignore_strings)
  (mov (@ 'memstr) OLD)
  (mov (@ 'memstrnew) NEW)
  (mov NEW (@ 'memstr))
  (mov OLD (@ 'memstrnew)) ;memstr <-> memstrnew
  (mov (@ 'memstrlimit) OLD)
  (mov (@ 'memstrlimitnew) NEW)
  (mov NEW (@ 'memstrlimit))
  (mov OLD (@ 'memstrlimitnew)) ;memstrlimit <-> memstrlimitnew
  (mov TEMP (@ 'freestring))
(: 'gc_flip_ignore_strings)
  (restore VAL)
  (mov VAL (@ 'tower_procedures))
  (restore VAL)
  (mov VAL (@ 'current_output_port))
  (restore VAL)
  (mov VAL (@ 'current_input_port))
  (restore VAL)
  (mov VAL (@ 'interaction_environment))
  (restore UNEV ARGL VAL ENV EXP)
  (ret)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'relocate_old_result_in_new)
  (mov (@ OLD) EXP) ;EXP <- (car OLD)
  (test 1 EXP)
  (jz 'relocate_move)
  (cmp BROKEN_HEART EXP)
  (je 'relocate_already_moved)
  (test EXP EXP)
  (js 'relocate_do_not_move)
  (cmpb TYPE_VECTOR EXP)
  (je 'relocate_vector)
  (cmp ASSEMBLY EXP)
  (jne 'relocate_move)
  (mov (@ 4 OLD) NEW) ;NEW now points to machine language
  (cmp 0 (@ -8 NEW))
  (je 'relocate_move)
  (mov (@ 'lambda_list) ENV)
  (mov ENV (@ -4 NEW))
  (add -8 NEW)
  (mov NEW (@ 'lambda_list))
(: 'relocate_move)
  (mov FREE NEW)
  (add 8 FREE)
  (mov EXP (@ NEW))   ;(set-car! NEW EXP)
  (mov (@ 4 OLD) ENV) ;ENV <- (cdr OLD)
  (mov ENV (@ 4 NEW)) ;(set-cdr! NEW ENV)
  (mov BROKEN_HEART (@ OLD)) ;(set-car! OLD BROKEN_HEART)
  (mov NEW (@ 4 OLD)) ;(set-cdr! OLD NEW)
  (ret)

(: 'relocate_do_not_move)
  (mov OLD NEW)
  (ret)

(: 'relocate_already_moved)
  (mov (@ 4 OLD) NEW)
  (ret)

(: 'relocate_vector)
  (call 'relocate_move)
  (push NEW)
  (mov FREE NEW)
  (add 8 OLD)
  (shr LENGTH_SHIFT EXP) ;Now edx is length of vector
  (test EXP EXP)
  (jz 'relocate_vector_end)
  (test 1 EXP) ;We always store remaining vector elements in pairs
  (jnz 'relocate_vector_odd)
(: 'relocate_vector_loop)
  (movs)
(: 'relocate_vector_odd)
  (dec EXP)
  (ja 'relocate_vector_loop)
(: 'relocate_vector_end)
  (mov NEW FREE)
  (pop NEW)
  (ret)

;;;;;;;;;;;;;;;;
;;;Validation;;;
;;;;;;;;;;;;;;;;

(new-message 'msg_unsupported_rational "Non-integral result unsupported")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'error_io)
  (popa)
(: 'error_scheme_io)
(new-message 'msg_io "IO error.")
  (init-message 'msg_io)
  (jmpl 'error_msg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'error_undefined_var)
(new-message 'msg_undefined_var " is undefined yet evaluated")
(if LIBFCGI (call 'insure_content_type))
  (mov EXP VAL)
  (mov STDERR (@ 'io_file))
  (call 'write_dispatch)
  (init-message 'msg_undefined_var)
  (jmpl 'error_msg_continue)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new-message 'msg_unsupported_environment "Unsupported environment")
(new-message 'msg_immutable_environment "Environment is immutable")
(new-message 'msg_too_many_args "Too many arguments")
(new-message 'msg_too_few_args "Too few arguments")
(new-message 'msg_expected_procedure "Expected procedure")
(new-message 'msg_unknown_syntax "Unknown syntax")
(new-message 'msg_expected_number "Expected number")

(define (make-errors . errs)
  (for-each
    (lambda (s)
      (: (string->symbol (string-append "error_" s)))
      (init-message (string->symbol (string-append "msg_" s)))
      (jmpl 'error_msg))
    errs))

(make-errors "unsupported_environment")
(make-errors "immutable_environment")
(make-errors "too_many_args")
(make-errors "too_few_args")
(make-errors "expected_procedure" "unknown_syntax" "expected_number")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Move first argument in ARGL into VAL and guarantee that it
;;is a NUMBER and that it is the last argument.
;;Return type header in TEMP
(: 'get_last_number)
  (insure-one-last-arg ARGL)
(: 'get_number)
  (mov (@ ARGL) VAL)
  (test VAL VAL)
  (jz 'error_expected_number)
  (mov (@ VAL) TEMP)
  (cmpb NUMBER TEMP)
  (jne 'error_expected_number)
  (ret)

(new-message 'msg_expected_exact_nonnegative_integer "Expected exact non-negative integer")
(make-errors "expected_exact_nonnegative_integer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Move first argument in ARGL into TEMP and guarantee that
;;it is an exact non-negative INTEGER and that it is the last argument.
(: 'get_last_exact_natural)
  (insure-one-last-arg ARGL)
(: 'get_exact_natural)
  (mov (@ ARGL) TEMP)
  (jecxz 'error_expected_exact_nonnegative_integer)
  (cmp INTEGER (@ TEMP))
  (jne 'error_expected_exact_nonnegative_integer)
  (ret)
  
(new-additional-primitive "expected-integer")
(new-message 'msg_expected_integer "Expected integer")
(make-errors "expected_integer")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Move first argument in ARGL into EXP.
;;Store in (@ FREE) the type header, also returned in TEMP,
;;with possibly inherited inexactness.
(: 'get_number_exactness)
  (get-number-exactness FREE)
  (ret)

(: 'get_integer_exactness)
  (get-integer-exactness FREE)
  (ret)

(new-message 'msg_expected_rational "Expected rational")
(new-message 'msg_expected_char "Expected char")
(new-message 'msg_expected_complex "Expected complex")
(make-errors "expected_rational" "expected_char" "expected_complex")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Move first argument in ARGL into VAL and guarantee that it
;;is a CHAR.  Then the ascii is moved into TEMP
(: 'get_last_char_ascii)
  (insure-one-last-arg ARGL)
(: 'get_char_ascii)
  (mov (@ ARGL) VAL)
  (fetch-char VAL VAL)
  (movzb VALH TEMP)
  (ret)

(new-message 'msg_expected_string "Expected string")
(make-errors "expected_string")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Move last argument in ARGL into ARGL and insure that it is a
;;TYPE_STRING.  Place header into TEMP (with length in high byte.)
(: 'get_last_string)
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) ARGL)
  (test ARGL ARGL)
  (jz 'error_expected_string)
  (mov (@ ARGL) TEMP)
  (cmpb TYPE_STRING TEMP)
  (jne 'error_expected_string)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new-message 'msg_expected_mutable_string "Expected mutable string")
(new-message 'msg_expected_vector "Expected vector")
(new-message 'msg_expected_symbol "Expected symbol")
(new-message 'msg_expected_input_port "Expected input port")
(new-message 'msg_expected_pair "Expected pair")
(make-errors "expected_mutable_string")
(make-errors "expected_input_port")
(make-errors "expected_pair" "expected_vector" "expected_symbol")


(: 'insure_pair)
  (test ARGL ARGL)
  (jz 'error_expected_pair)
  (test 1 (@ ARGL))
  (jnz 'error_expected_pair)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Move first argument in ARGL into ARGL and guarantee that
;;it is an INPUT_PORT.  If no argument is given, then use
;;current_input_port.  Set io_file accordingly.
(: 'get_last_input_port)
  (test ARGL ARGL)
  (jz 'set_input_port_current)
  (cmp 0 (@ 4 ARGL))
  (jnel 'error_too_many_args)
  (mov (@ ARGL) ARGL)
  (test ARGL ARGL)
  (jz 'error_expected_input_port)
  (mov (@ ARGL) TEMP)
  (opd-size)(cmp INPUT_PORT TEMP)
  (jne 'error_expected_input_port)
  (jmp 'set_input_port)
(: 'set_input_port_current)
  (mov (@ 'current_input_port) ARGL)
(: 'set_input_port)
  (mov (@ 4 ARGL) TEMP)
  (mov TEMP (@ 'io_file))
  (mov 0 (@ 'input))
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new-message 'msg_expected_output_port "Expected output port")
(make-errors "expected_output_port")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Move first argument in ARGL into ARGL and guarantee that
;;it is an OUTPUT_PORT.  If no argument is given, use
;;current_output_port.  Then set io_file accordingly
(: 'get_last_output_port)
  (test ARGL ARGL)
  (jz 'set_output_port_current)
  (cmp 0 (@ 4 ARGL))
  (jnel 'error_too_many_args)
  (mov (@ ARGL) ARGL)
  (test ARGL ARGL)
  (jz 'error_expected_output_port)
  (cmp OUTPUT_PORT (@ ARGL))
  (jne 'error_expected_output_port)
  (jmp 'set_output_port)
(: 'set_output_port_current)
  (mov (@ 'current_output_port) ARGL)
(: 'set_output_port)
  (mov (@ 4 ARGL) TEMP)
  (mov TEMP (@ 'io_file))
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(new-message 'msg_expected_simple_type "Expected simple type")
(new-message 'msg_expected_paren "Expected closing parenthesis")
(new-message 'msg_extra_eol "Too many closing parentheses")
(new-message 'msg_invalid_pair "Invalid pair")
(new-message 'msg_invalid_index "Invalid index")
(new-message 'msg_string_too_large "String is too large")
(new-message 'msg_vector_too_large "Vector is too large")
(new-message 'msg_overflow "Arithmetic overflow")
(new-message 'msg_no_bignums "Bignums not yet supported")
(new-message 'msg_divide_by_zero "Divide by zero error")
(new-message 'msg_unknown_literal "Unknown literal")
(make-errors "expected_simple_type" "expected_paren" "extra_eol" "invalid_pair"
             "invalid_index" "string_too_large" "vector_too_large" "overflow"
             "no_bignums" "divide_by_zero" "unknown_literal")
(if LIBFCGI
 (begin
  (new-message 'msg_no_fcgi "Could not load FCGI library")
  (make-errors "no_fcgi")))

(: 'error_eof)
(new-message 'msg_eof "End of file encountered")
  (init-message 'msg_eof)

(new-additional-primitive "error")
  (call 'get_last_string)
  (shr LENGTH_SHIFT TEMP) ;TEMP now holds length of string
  (mov TEMP (@ 'str_len))
  (mov (@ 4 ARGL) TEMP)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'error_msg)
(if LIBFCGI (call 'insure_content_type))
(: 'error_msg_continue)
  (mov STDERR (@ 'io_file))
  (call 'puts)
  (cmp 0 (@ 'load_filename))
  (jz 'error_was_from_repl)
  (new-message 'at_line " at line ")
  (init-message 'at_line)
  (call 'puts)
  (mov 10 (@ 'radix))
  (mov (@ 'load_line) VAL)
  (call 'stringify_integer32)
  (call 'puts)
  (new-message 'of_file " of ")
  (init-message 'of_file)
  (call 'puts)
  (mov (@ 'load_filename) UNEV)
  (jmp 'error_filename_loop_begin)

(: 'error_filename_loop)
  (movb VAL (@ 'output))
  (call 'putch)
(: 'error_filename_loop_begin)
  (lodsb)
  (cmpb 0 VAL)
  (jne 'error_filename_loop)

  (mov 0 (@ 'load_filename))

(: 'error_was_from_repl)
  (movb 10 (@ 'output))
  (call 'putch)

(: 'break)
  (mov (@ 'stackbase) SP)
  (mov 0 (@ 'root))
  (clear VAL EXP UNEV ARGL)
  (if LIBFCGI
    (jmpl 'exit_not_ok)
    (jmpn (@ 'error_continuation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'error_out_of_memory)
(new-message 'msg_out_of_memory "Out of memory.")
  (mov STDERR (@ 'io_file))
  (init-message 'msg_out_of_memory)
  (call 'puts)
  (movb 10 (@ 'output))
  (call 'putch)
  (exit-with-code -1)

;;;;;;;;;;;
;;;Forms;;;
;;;;;;;;;;;

(new-primitive "eval")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP)
  (mov (@ 4 ARGL) ENV)
  (test ENV ENV)
  (jzl 'error_too_few_args)
  (cmp 0 (@ 4 ENV))
  (jnzl 'error_too_many_args)
  (mov (@ ENV) ENV)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'eval_dispatch)
  (test EXP EXP)
  (jz 'ev_self_eval)
  (test 1 (@ EXP))
  (jz 'ev_combination)
  (mov (@ EXP) TEMP)
  (cmpb SYMBOL TEMP)
  (je 'ev_variable)
(: 'ev_self_eval)
  (mov EXP VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Return in TEMP the (symbol . value) associated with
;;the symbol in EXP.  Returns in VAL the value.
;;Clobbers ENV in the process.
;;Leaves EXP, UNEV, and ARGL untouched.
;;Only returns if variable is found.
(: 'ev_variable)
  (mov (@ ENV) VAL) ;VAL <- ((symbol . value) ...)
                    ;or #(((symbol . value) ...) ...)
  (cmp 0 (@ 4 ENV)) ;Is this the top-level environment?
  (jnz 'variable_binding_loop)
  (test 1 (@ VAL))
  (jz 'variable_binding_loop)
  (mov EXP TEMP) ;Environment must be a VECTOR
  (and! TOPLEVEL_HASH_MASK TEMP)
  (mov (@ 4 VAL TEMP 0) VAL)
(: 'variable_binding_loop)
  (test VAL VAL)
  (jz 'variable_enclosing_scope)
  (mov (@ VAL) TEMP)  ;TEMP <- (symbol . value)
  (cmp EXP (@ TEMP))  ;(eq? (car TEMP) EXP)
  (je 'variable_found)
  (mov (@ 4 VAL) VAL) ;VAL <- (cdr VAL)
  (jmp 'variable_binding_loop)
(: 'variable_enclosing_scope)
  (mov (@ 4 ENV) ENV) ;ENV <- enclosing scope
  (test ENV ENV)
  (jnz 'ev_variable)
  (jmpl 'error_undefined_var)
(: 'variable_found)
  (mov (@ 4 TEMP) VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'ev_combination)
  (mov (@ 4 EXP) UNEV)
  (mov (@ EXP) VAL)
  (test VAL VAL)
  (jzl 'error_expected_procedure)
  (mov (@ VAL) TEMP)
  (cmp MACRO_CLOSURE TEMP)
  (jel 'apply_macro_direct)
  (cmpb PROCEDURE TEMP)
  (jel 'apply_procedure)
  (cmp SYMBOL TEMP)
  (jel 'apply_symbol)
  (mov VAL EXP)
  (save ENV UNEV)
  (call 'eval_dispatch)
  (restore UNEV ENV)
  (test VAL VAL)
  (jzl 'error_expected_procedure)
  (jmpl 'apply_procedure)

(: 'apply_symbol)
  (push ENV)
  (push EXP)
  (mov VAL EXP)
  (call 'ev_variable)
  (pop EXP)
  (test VAL VAL)
  (jzl 'error_expected_procedure)
  (cmp MACRO_CLOSURE (@ VAL))
  (je 'apply_macro)
  (pop ENV)
  (jmpl 'apply_procedure)

(: 'apply_macro) ;ENV on stack, EXP is compound macro, VAL is macro
  (pop ENV)
(: 'apply_macro_direct)
  (save EXP ENV)
  (mov UNEV ARGL)
  (call 'apply_compound)
  (restore ENV EXP)
  (mov VAL EXP)
  (jmpl 'eval_dispatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Evaluate and apply arguments in UNEV
;;to non-null procedure in VAL
(: 'apply_procedure)
  (cmp SYNTAX_PRIMITIVE (@ VAL))
  (jel 'apply_builtin) ;UNEV points to arguments
  (clear ARGL)
  (test UNEV UNEV)
  (jzl 'apply_dispatch) ;No operands
  (save VAL)
(: 'apply_operand_loop)
  (save ARGL)
  (mov (@ UNEV) EXP)
  (cmp 0 (@ 4 UNEV))
  (jel 'apply_last_arg)
  (save ENV UNEV)
  (call 'eval_dispatch)
  (restore UNEV ENV ARGL)
  (mov (object VAL ARGL) ARGL)
  (call 'advance_free)
  (mov (@ 4 UNEV) UNEV)
  (jmpl 'apply_operand_loop)
(: 'apply_last_arg)
  (call 'eval_dispatch)
  (restore ARGL)
  (mov (object VAL ARGL) ARGL)
  (call 'advance_free)
  (call 'reverse)
  (mov VAL ARGL)
  (restore VAL)
(: 'apply_dispatch)
  (cmp PRIMITIVE (@ VAL))
  (je 'apply_builtin) ;ARGL points to arguments
  (cmp CLOSURE (@ VAL))
  (je 'apply_compound)
  (cmp CONTINUATION (@ VAL))
  (je 'apply_continuation)
  (opd-size)(cmp REAL (@ VAL))
  (je 'apply_compound)
  (jmpl 'error_expected_procedure)
(: 'apply_builtin)
  (jmpn (@ 4 VAL))

(: 'apply_continuation)
  (insure-one-last-arg ARGL)
  (mov (@ 4 VAL) VAL) ;VAL <- (root . stack)
  (mov (@ VAL) TEMP)
  (mov TEMP (@ 'root))
  (mov (@ 4 VAL) VAL) ;VAL <- stack
  (mov (@ 'stackbase) SP)
(: 'apply_continuation_loop)
  (test VAL VAL)
  (jz 'apply_continuation_end)
  (mov (@ VAL) TEMP)
  (push (@ 4 TEMP))
  (mov (@ 4 VAL) VAL)
  (jmp 'apply_continuation_loop)
(: 'apply_continuation_end)
  (restore ENV)
  (mov (@ ARGL) VAL)
  (ret)

(: 'apply_compound)
  (mov (@ 4 VAL) VAL) ;VAL <- (environment . (formals . body)) or (environment . (ASSEMBLY . VECTOR))
  (mov (@ VAL) ENV)   ;ENV <- environment
  (mov (object 0 ENV) ENV) ;Prepare to extend environment
  (call 'advance_free)
  (mov (@ 4 VAL) UNEV) ;UNEV <- (formals . body) or (ASSEMBLY . VECTOR)
  (mov (@ 4 UNEV) TEMP) ;body or VECTOR
  (test TEMP TEMP)
  (jzl 'compiler) ;empty body
  (test 1 (@ TEMP)) ; is (cdr UNEV) a VECTOR and hence CLOSURE has been compiled?
  (jzl 'compiler) ;nope
  (mov (@ UNEV) VAL) ;VAL <- ASSEMBLY
  (mov (@ 4 UNEV) UNEV) ;#(QUOTATION ...)
  (jmpn (@ 4 VAL))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-syntax "begin")
  (test UNEV UNEV)
  (jel 'return_false)
(: 'begin_loop)
  (mov (@ UNEV) EXP)
  (cmp 0 (@ 4 UNEV))
  (jel 'eval_dispatch)
  (save UNEV ENV)
  (call 'eval_dispatch)
  (restore ENV UNEV)
  (mov (@ 4 UNEV) UNEV)
  (jmp 'begin_loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-syntax "quote")
  (insure-one-last-arg UNEV)
  (mov (@ UNEV) VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-syntax "lambda")
  (insure-more-args UNEV)
  (cmp 0 (@ 4 UNEV))
  (jzl 'error_too_few_args)
(: 'lambda)
  (mov (object ENV UNEV) VAL)
  (add 8 FREE)
  (mov (object CLOSURE VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "make-closure")
  (insure-more-args ARGL)
  (mov (@ 4 ARGL) VAL)
  (insure-one-last-arg VAL)
  (mov (@ VAL) VAL)
  (insure-vector VAL TEMP)
  (mov VAL (@ 4 ARGL))
  (mov (object ENV ARGL) VAL)
  (add 8 FREE)
  (mov (object CLOSURE VAL) VAL)
  (jmpl 'advance_free)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-syntax "macro")
  (insure-more-args UNEV)
  (mov (object ENV UNEV) VAL)
  (add 8 FREE)
  (mov (object MACRO_CLOSURE VAL) VAL)
  (jmpl 'advance_free)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-syntax "if")
  (insure-more-args UNEV)
  (mov (@ UNEV) EXP) ;EXP <- predicate
  (mov (@ 4 UNEV) UNEV) ;UNEV <- (consequent alternate)
  (insure-more-args UNEV)
  (save UNEV ENV)
  (call 'eval_dispatch)
  (restore ENV UNEV)
  (cmp 'false VAL)
  (jne 'if_consequent)
(: 'if_alternate)
  (mov (@ 4 UNEV) UNEV)
  (test UNEV UNEV)
  (jnz 'if_consequent)
  (ret) ;No alternate
(: 'if_consequent)
  (mov (@ UNEV) EXP)
  (jmpl 'eval_dispatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-syntax "set!")
  (insure-more-args UNEV)
  (mov (@ 4 UNEV) EXP) ;EXP <- (value)
  (insure-one-last-arg EXP)
  (mov (@ EXP) EXP) ;EXP <- value
  (mov (@ UNEV) UNEV) ;UNEV <- symbol
  (save UNEV ENV)
  (call 'eval_dispatch)
  (restore ENV UNEV)
(: 'set_scope_loop)
  (mov (@ ENV) TEMP) ;TEMP <- ((symbol . value) ...)
                     ;or #(((symbol . value) ...) ...)
  (cmp 0 (@ 4 ENV)) ;is this the top-level environment?
  (jnz 'set_binding_loop)
  (test 1 (@ TEMP))
  (jz 'set_binding_loop)
  (mov UNEV EXP) ;Environment must be a VECTOR
  (and! TOPLEVEL_HASH_MASK EXP)
  (mov (@ 4 TEMP EXP 0) TEMP)
(: 'set_binding_loop)
  (jecxz 'set_enclosing_scope)
  (mov (@ TEMP) EXP) ;EXP <- (symbol . value)
  (cmp UNEV (@ EXP))
  (je 'set_found)
  (mov (@ 4 TEMP) TEMP) ;TEMP <- next binding
  (jmp 'set_binding_loop)

(: 'set_enclosing_scope)
  (mov (@ 4 ENV) ENV) ;ENV <- enclosing scope
  (test ENV ENV)
  (jnz 'set_scope_loop)
  (mov UNEV EXP)
  (jmpl 'error_undefined_var)

(: 'set_found)
  (mov VAL (@ 4 EXP))
  (ret)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-syntax "define")
  (insure-more-args UNEV)
  (mov (@ 4 UNEV) EXP) ;EXP <- (value)
  (insure-more-args EXP)
  (mov (@ UNEV) UNEV) ;UNEV <- variable to be
  (test UNEV UNEV)
  (jzl 'error_expected_symbol)
  (test 1 (@ UNEV))
  (jzl 'error_expected_symbol)
  (insure-last-arg EXP)
  (mov (@ EXP) EXP) ;EXP <- value
  (save UNEV ENV)
  (call 'eval_dispatch)
  (restore ENV UNEV)
(: 'define_define)
  (mov ENV ARGL)
  (cmp 0 (@ 4 ENV)) ;is this the top-level environment?
  (jnz 'define_binding_env)
  (mov (@ ARGL) ARGL) ;ARGL <- #(((symbol . value) ...) ...)
                      ;or ((symbol . value) ...)
  (test 1 (@ ARGL))
  (jzl 'error_immutable_environment)
  (mov UNEV TEMP)
  (and! TOPLEVEL_HASH_MASK TEMP)
  (lea (@ 4 ARGL TEMP 0) ARGL)
(: 'define_binding_env)
  (mov (@ ARGL) TEMP) ;TEMP <- ((symbol . value) ...)
(: 'define_binding_loop)
  (jecxz 'define_new_var)
  (mov (@ TEMP) EXP) ;EXP <- (symbol . value)
  (cmp UNEV (@ EXP))
  (je 'define_found)
  (mov (@ 4 TEMP) TEMP)
  (jmp 'define_binding_loop)

(: 'define_new_var)
  (mov (object UNEV VAL) (@ 8 FREE))
  (add 8 FREE)
  (mov (@ ARGL) TEMP)
  (mov TEMP (@ 4 FREE))
  (mov FREE (@ ARGL)) ;(set-car! ARGL ((symbol . value) ARGL))
  (jmpl 'advance_free)

(: 'define_found)
  (mov VAL (@ 4 EXP))
  (ret)

;;;;;;;;;;;;;;;;;;;;
;; Library Syntax ;;
;;;;;;;;;;;;;;;;;;;;
;To become macros when bootstrapping
(new-syntax "quasiquote")
(new-syntax "cond")
(new-syntax "do")
(new-syntax "or")
(new-syntax "and")
(new-syntax "letrec")
(new-syntax "let")
(new-syntax "let*")
(new-syntax "case")
(new-primitive "<")
(new-primitive "<=")
(new-primitive "lcm")
(ret)

;;;;;;;;;;;;;;;;;;;;
;; I/O Primitives ;;
;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "read-char")
  (call 'get_last_input_port)
(: 'prim_read-char_argl)
  (mov (@ ARGL) TEMP)
  (test #x02000000 TEMP)
  (jnzl 'read_eof)
  (test #x01000000 TEMP)
  (jnz 'consume_peeked_char)
  (call 'getch)
  (cmp -1 (@ 'input))
  (je 'read_fresh_eof)
  (mov (@ 'input) TEMP)
  (lea (@ 'chararray #f TEMP 2) VAL)
  (ret)

(: 'consume_peeked_char)
  (and! #xffffff TEMP)
  (mov TEMP (@ ARGL))
  (shr 16 TEMP)
  (mov TEMP (@ 'input))
  (lea (@ 'chararray #f TEMP 2) VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "peek-char")
  (call 'get_last_input_port)
  (mov (@ ARGL) TEMP)
  (test #x02000000 TEMP)
  (jnzl 'read_eof)
  (test #x01000000 TEMP)
  (jnz 'prim_read_peeked_char)
  (call 'getch)
  (cmp -1 (@ 'input))
  (je 'read_fresh_eof)
  (mov (@ 'input) TEMP)
  (lea (@ 'chararray #f TEMP 2) VAL)
  (or! #x100 TEMP)
  (shl 16 TEMP)
  (movb INPUT_PORT TEMP)
  (mov TEMP (@ ARGL)) ;remember peeked ascii
  (ret)

(: 'prim_read_peeked_char)
  (and! #xffffff TEMP)
  (shr 16 TEMP)
  (lea (@ 'chararray #f TEMP 2) VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'read_fresh_eof)
  (or! #x02000000 (@ ARGL))
(: 'read_eof)
  (mov -1 (@ 'input))
  (mov 'eof VAL)
  (ret)

(new-primitive "read")
  (call 'get_last_input_port)
(: 'prim_read_argl)
  (call 'prim_read-char_argl)
  (save ARGL) ;remember port so that we can update peek char later.
  (call 'read_dispatch)
  (restore ARGL)
  (mov (@ 'input) TEMP)
  (and! #x2ff TEMP)
  (or! #x100 TEMP)
  (shl 16 TEMP)
  (movb INPUT_PORT TEMP)
  (mov TEMP (@ ARGL)) ;remember last char read
  (cmp (string->symbol "sym_)") VAL)
  (jel 'error_extra_eol)
  (ret)

(: 'read_input_newline)
  (inc (@ 'load_line))
(: 'read_input)
  (call 'getch)
(: 'read_dispatch)
  (mov (@ 'input) TEMP)
  (cmp -1 TEMP)
  (jel 'read_eof)

(define (dispatch c d)
  (cmpb c TEMP)
  (jel d))

  (dispatch #\space 'read_input)
  (dispatch 9   'read_input) ;tab
  (dispatch 13  'read_input) ;carriage-return
  (dispatch 10  'read_input_newline) ;newline
  (dispatch #\( 'read_list)
  (dispatch #\) 'read_eol)
  (dispatch #\# 'read_literal)
  (dispatch #\' 'read_quote)
  (dispatch #\` 'read_quasiquote)
  (dispatch #\, 'read_unquote)
  (dispatch #\. 'read_dot)
  (dispatch #\" 'read_string)
  (dispatch #\; 'read_comment)

  (cmpb #\0 TEMP)
  (jbl 'read_symbol)
  (cmpb #\9 TEMP)
  (jal 'read_symbol)
  (jmpl 'read_exact_decimal_number)

(: 'read_dot)
  (call 'getch)
  (mov (@ 'input) VAL)

(for-each
  (lambda (c)
    (cmpb c VAL)
    (je 'read_dot_confirmed))
  '(#\space 9 10 13 #\( #\) #\;))

  (cmp -1 VAL)
  (je 'read_dot_confirmed)
(: 'read_dot_prefix)
  (mov (@ 'freesymbol) ARGL)
  (cld)
  (mov SYMBOL (@ ARGL))
  (movb #\. (@ 4 ARGL))
  (add 5 ARGL)
  (jmpl 'read_symbol_chars)

(: 'read_dot_confirmed)
  (mov 'sym_. VAL)
  (ret)

(: 'read_comment)
  (call 'getch)
  (cmp -1 (@ 'input))
  (jel 'read_eof)
  (cmpb 10 (@ 'input)) ;newline
  (jne 'read_comment)
  (jmpl 'read_input_newline)

(: 'read_eol)
  (call 'getch)
  (mov (string->symbol "sym_)") VAL)
  (ret)

(: 'read_literal)
  (call 'getch)
  (mov (@ 'input) TEMP)
  (cmp -1 TEMP)
  (jel 'error_eof)
  (cmpb #\( TEMP)
  (jel 'read_vector)
  (to_lower TEMP)
  (call 'getch)
  (dispatch #\t 'read_boolean_true)
  (dispatch #\f 'read_boolean_false)
  (dispatch #\\ 'read_character)
  (mov INTEGER (@ FREE))
  (dispatch #\e 'read_exact_prefix)
  (dispatch #\i 'read_inexact_prefix)
(: 'read_number_literal)
  (dispatch #\x 'read_hexadecimal_number)
  (dispatch #\o 'read_octal_number)
  (dispatch #\b 'read_binary_number)
  (dispatch #\d 'read_decimal_number)
  (jmpl 'error_unknown_literal)

(: 'read_inexact_prefix)
  (or! #x10000 (@ FREE))
(: 'read_exact_prefix)
  (cmp -1 (@ 'input))
  (jel 'error_eof)
  (cmpb #\# (@ 'input))
  (jnel 'read_decimal_number)
  (call 'getch)
  (mov (@ 'input) TEMP)
  (cmp -1 TEMP)
  (jel 'error_eof)
  (to_lower TEMP)
  (call 'getch)
  (jmpl 'read_number_literal)

(: 'read_boolean_true)
  (mov 'true VAL)
  (ret)

(: 'read_boolean_false)
  (mov 'false VAL)
  (ret)

(: 'read_character)
  (mov (@ 'input) TEMP)
  (dispatch #\space 'read_character_direct)
  (dispatch 10 'read_character_direct_newline) ;newline
  (dispatch 13 'read_character_direct_newline) ;newline
  (dispatch 9 'read_character_direct) ;tab
  (dispatch #\( 'read_character_direct)
  (dispatch #\) 'read_character_direct)
  (dispatch #\; 'read_character_direct)
  (mov TEMP (@ 'backup_char))
  (call 'read_symbol)
  (movzb (@ 5 VAL) TEMP)
  (test TEMP TEMP)
  (jz 'read_character_simple)
  (cmp 'sym_space VAL)
  (je 'read_character_space)
  (cmp 'sym_newline VAL)
  (je 'read_character_newline)
  (jmpl 'error_unknown_literal)

(: 'read_character_direct_newline)
  (inc (@ 'load_line))
(: 'read_character_direct)
  (lea (@ 'chararray #f TEMP 2) VAL)
  (jmpl 'getch)

(: 'read_character_simple)
  (mov (@ 'backup_char) VAL)
  (lea (@ 'chararray #f VAL 2) VAL)
  (ret)

(: 'read_character_simple_space)
  (call 'getch)
(: 'read_character_space)
  (lea (@ (+ (* 32 4) (lookup 'chararray))) VAL)
  (ret)

(: 'read_character_simple_newline)
  (call 'getch)
(: 'read_character_newline)
  (lea (@ (+ (* 10 4) (lookup 'chararray))) VAL)
  (ret)

(: 'read_binary_number)
  (mov #b10 (@ 'radix))
  (jmpl 'read_number)

(: 'read_octal_number)
  (mov #o10 (@ 'radix))
  (jmpl 'read_number)

(: 'read_exact_decimal_number)
  (mov INTEGER (@ FREE))
(: 'read_decimal_number)
  (mov 10 (@ 'radix))
  (jmpl 'read_number)

(: 'read_hexadecimal_number)
  (mov #x10 (@ 'radix))
  (jmpl 'read_number)

(: 'read_quasiquote)
  (push 'sym_quasiquote)
  (jmp 'read_abbreviation)

(: 'read_unquote)
  (call 'getch)
  (cmpb #\@ (@ 'input))
  (je 'read_unquote_splicing)
  (push 'sym_unquote)
  (jmp 'read_abbreviation_contents)

(: 'read_unquote_splicing)
  (push 'sym_unquote-splicing)
  (jmp 'read_abbreviation)

(: 'read_quote)
  (push 'sym_quote)
(: 'read_abbreviation)
  (call 'getch)
(: 'read_abbreviation_contents)
  (call 'read_dispatch)
  (cmp (string->symbol "sym_)") VAL)
  (jel 'error_extra_eol)
  (mov (object VAL 0) VAL)
  (add 8 FREE)
  (mov VAL (@ 4 FREE))
  (pop (@ FREE))
  (mov FREE VAL)
  (jmpl 'advance_free)

(: 'read_list)
  (save ENV)
  (clear ARGL)
  (call 'getch)
(: 'read_list_loop)
  (save ARGL)
  (call 'read_dispatch)
  (restore ARGL)
  (cmp (string->symbol "sym_)") VAL)
  (je 'read_list_end)
  (cmp 'eof VAL)
  (jel 'error_eof)
  (cmp 'sym_. VAL)
  (je 'read_cdr)
  (mov (object VAL 0) VAL)
  (call 'advance_free)
  (test ARGL ARGL)
  (jz 'read_list_first)
  (mov VAL (@ 4 ENV))
  (mov VAL ENV)
  (jmp 'read_list_loop)

(: 'read_list_first)
  (mov VAL ARGL)
  (mov VAL ENV)
  (jmp 'read_list_loop)

(: 'read_list_end)
  (mov ARGL VAL)
  (restore ENV)
  (ret)

(: 'read_cdr)
  (test ARGL ARGL)
  (jzl 'error_invalid_pair)
  (save ARGL)
  (call 'read_dispatch)
  (restore ARGL)
  (cmp (string->symbol "sym_)") VAL)
  (jel 'error_invalid_pair)
  (cmp 'eof VAL)
  (jel 'error_eof)
  (cmp 'sym_. VAL)
  (jel 'error_invalid_pair)
  (mov VAL (@ 4 ENV))
  (save ARGL)
  (call 'read_dispatch)
  (cmp (string->symbol "sym_)") VAL)
  (jnel 'error_expected_paren)
  (restore VAL)
  (jmpl 'read_list_end)

(: 'read_symbol)
  (mov (@ 'freesymbol) ARGL)
  (cld)
  (mov SYMBOL (@ ARGL))
  (add 4 ARGL)
  (mov (@ 'input) VAL)
(: 'read_symbol_chars)
  (to_lower VAL)
  (stosb)
  (call 'getch)
  (cmpb #\- VAL)
  (jne 'read_name_loop)
  (cmpb #\0 (@ 'input))
  (jb 'read_name_loop)
  (cmpb #\9 (@ 'input))
  (ja 'read_name_loop)
  (clear VAL ARGL)
  (call 'read_exact_decimal_number)
  (opd-size)(cmp INTEGER (@ VAL))
  (jne 'read_negate_rational)
  (cmp 0 (@ 4 VAL))
  (je 'read_negate_zero)
  (xor #x00020000 (@ VAL))
(: 'read_negate_zero)
  (ret)

(: 'read_negate_rational)
  (mov (object VAL 0) VAL)
  (call 'advance_free)
  (mov (symbol-name "-") EXP)
  (mov (object EXP VAL) VAL)
  (jmpl 'advance_free)
 
(: 'read_name_loop)
  (mov (@ 'input) VAL)
  (cmp -1 VAL)
  (je 'read_name_end)
  (for-each
    (lambda (x)
      (cmpb x VAL)
      (je 'read_name_end))
    '(#\space 9 10 13 #\( #\) #\;))
  (to_lower VAL)
  (stosb)
  (call 'getch)
  (jmp 'read_name_loop)

(: 'read_name_end)
  (movb 0 (@ ARGL))
  (inc ARGL)
  (test 3 ARGL) ;are we at 4 byte boundary?
  (jnz 'read_name_end)
  (mov ARGL (@ 'backup))    ;new freesymbol in case this symbol is new symbol
  (mov (@ 'freesymbol) VAL) ;we are interning.
  (mov (@ 4 VAL) TEMP)      ;first letter
  (and! #x7f TEMP)
  (shl HASHPOWER TEMP)
  (add 'obhash TEMP)
;;TEMP is now indexed into obhash according to first letter
(: 'read_compare_loop)
  (cmp 0 (@ TEMP)) ;no more symbols?
  (je 'read_new_symbol)
  (mov (@ TEMP) UNEV) ;symbol
  (mov (@ 'freesymbol) ARGL) ;read symbol
  (cmps) ;skip SYMBOL type header
(: 'read_symbol_compare_loop)
  (cmpsb)
  (jne 'read_next_symbol)
  (cmpb 0 (@ -1 UNEV)) ;end of read symbol?
  (jne 'read_symbol_compare_loop)
  (mov (@ TEMP) VAL) ;found! VAL is matching symbol.
  (clear UNEV ARGL)
  (ret)

(: 'read_next_symbol)
  (add 4 TEMP)
  (cmp 'obhash_end TEMP)
  (jne 'read_compare_loop)
  (mov 'obhash TEMP)
  (jmp 'read_compare_loop)

(: 'read_new_symbol)
  (mov (@ 'backup) ARGL) ;next free symbol space
  (mov ARGL (@ 'freesymbol))
  (mov VAL (@ TEMP)) ;set to new symbol
  (clear UNEV ARGL)
  (ret)

(: 'read_rational)
  (save VAL)
  (mov INTEGER (@ FREE))
  (call 'getch)
  (call 'stream_to_integer)
  (test VAL VAL)
  (jzl 'error_unknown_literal)
  (restore EXP)
  (mov (object VAL 0) UNEV)
  (call 'advance_free)
  (mov (object EXP UNEV) UNEV)
  (call 'advance_free)
  (mov (symbol-name "/") EXP)
  (mov (object EXP UNEV) VAL)
  (jmpl 'advance_free)

(: 'read_number)
  (mov 'getch (@ 'thunk))
  (call 'stream_to_integer_nonempty)
  (test VAL VAL)
  (jzl 'error_unknown_literal)
  (cmpb #\/ (@ 'input))
  (jel 'read_rational)
  (ret)

(: 'read_string)
  (mov (@ 'freestring) ARGL)
  (mov ARGL (@ 4 FREE)) ;we will make the TYPE_STRING later
  (cld)
(: 'read_string_loop)
  (call 'getch)
  (mov (@ 'input) TEMP)
  (dispatch #\" 'read_string_end)
  (dispatch 10 'read_string_newline)
  (cmpb #\\ TEMP)
  (jne 'read_string_continue)
  (call 'getch)
  (mov (@ 'input) TEMP)
  (dispatch 10 'read_string_newline)
(: 'read_escape_newline)
  (cmpb #\n TEMP)
  (jne 'read_escape_tab)
  (movb 10 TEMP) ;newline
  (jmp 'read_string_continue)

(: 'read_escape_tab)
  (cmpb #\t TEMP)
  (jne 'read_string_continue)
  (movb 9 TEMP) ;tab
  (jmp 'read_string_continue)

(: 'read_string_newline)
  (inc (@ 'load_line))
(: 'read_string_continue)
  (cmp -1 TEMP)
  (jel 'error_eof)
  (movb TEMP (@ ARGL))
  (inc ARGL)
  (cmp (@ 'memstrlimit) ARGL)
  (if (not DEBUG) (jb 'read_string_loop))
  (push (@ 'freestring)) ;string data
  (sub (@ 'freestring) ARGL)
  (push ARGL) ;length so far
  (clear ARGL)
  (call 'gc_strings)
  (pop TEMP) ;length so far
  (pop UNEV) ;string data
  (mov (@ 'freestring) ARGL)
  (mov ARGL (@ 4 FREE)) ;new string pointer
  (cld)
  (rep)(movsb)
  (clear UNEV VAL)
  (cmp (@ 'memstrlimit) ARGL)
  (jael 'error_out_of_memory)
  (jmpl 'read_string_loop)

(: 'read_string_end)
  (movb 0 (@ ARGL))
  (mov ARGL TEMP)
  (sub (@ 'freestring) TEMP)
  (shl LENGTH_SHIFT TEMP)
  (add MUTABLE_STRING TEMP)
  (mov TEMP (@ FREE))
  (inc ARGL)
  (mov ARGL (@ 'freestring))
  (clear ARGL)
  (mov FREE VAL)
  (call 'advance_free)
  (jmpl 'getch)

(: 'read_vector)
  (call 'read_list)
  (jmpl 'list_to_vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "display")
  (insure-more-args ARGL)
  (mov (@ ARGL) VAL) ;object to display
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_output_port)
  (mov 'false UNEV) ;signal to display chars and strings
  (jmp 'display_dispatch)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "write")
  (insure-more-args ARGL)
  (mov (@ ARGL) VAL) ;object to write
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_output_port)
(: 'write_dispatch)
  (mov 'true UNEV) ;signal to write chars and strings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'display_dispatch)
  (test VAL VAL)
  (jzl 'write_empty_list)
  (mov (@ VAL) TEMP)
  (test 1 TEMP)
  (jzl 'write_pair)
  (cmpb NUMBER TEMP)
  (jel 'write_number)
  (cmpb SYMBOL TEMP)
  (jel 'write_symbol)
  (cmpb CHAR TEMP)
  (jel 'write_char)
  (cmpb TYPE_STRING TEMP)
  (jel 'write_string)
  (movb #\# (@ 'output))
  (call 'putch)
  (cmp 'true VAL)
  (jel 'write_boolean_true)
  (cmp 'false VAL)
  (jel 'write_boolean_false)
  (cmpb TYPE_VECTOR TEMP)
  (jel 'write_vector)
  (movb #\< (@ 'output))
  (call 'putch)
  (opd-size)(cmp CLOSURE TEMP)
  (jel 'write_closure)
  (opd-size)(cmp CONTINUATION TEMP)
  (jel 'write_continuation)
  (cmp 'eof VAL)
  (jel 'write_eof)
  (opd-size)(cmp PRIMITIVE TEMP)
  (jel 'write_primitive)
  (opd-size)(cmp SYNTAX_PRIMITIVE TEMP)
  (jel 'write_syntax)
  (opd-size)(cmp MACRO_CLOSURE TEMP)
  (jel 'write_macro)
  (cmpb PORT TEMP)
  (jel 'write_port)
  (opd-size)(cmp ASSEMBLY TEMP)
  (jel 'write_assembly)
  (jmpl 'write_other)

(: 'write_inexact_prefix)
  (putchar #\#)
  (putchar #\i)
  (jmp 'write_number_value)

(: 'write_number)
  (opd-size)(cmp COMPLEX TEMP)
  (je 'write_complex)
  (test #x10000 TEMP)
  (jnz 'write_inexact_prefix)
(: 'write_number_value)
  (opd-size)(cmp INTEGER TEMP)
  (jel 'write_decimal_number)
  (mov (@ 4 VAL) VAL)
  (opd-size)(cmp RATIONAL TEMP)
  (je 'write_rational_number)
  (new-message 'msg_real "#<real>")
  (init-message 'msg_real)
  (jmpl 'puts)

(: 'write_rational_number)
  (push VAL)
  (mov (@ VAL) VAL)
  (call 'write_decimal_number)
  (putchar #\/)
  (pop VAL)
  (mov (@ 4 VAL) VAL)
  (jmpl 'write_decimal_number)

(: 'write_complex)
  (new-message 'msg_complex "(make-rectangular ")
  (init-message 'msg_complex)
  (call 'puts)
  (mov (@ 4 VAL) VAL)
  (push VAL)
  (mov (@ VAL) VAL)
  (call 'display_dispatch)
  (putchar #\space)
  (pop VAL)
  (mov (@ 4 VAL) VAL)
  (call 'display_dispatch)
  (putchar-tail #\))

(: 'write_empty_list)
  (putchar #\()
  (putchar-tail #\))
  
(: 'write_boolean_true)
  (putchar-tail #\t)

(: 'write_boolean_false)
  (putchar-tail #\f)

(: 'write_char)
  (cmp 'true UNEV)
  (jne 'display_char)
  (putchar #\#)
  (putchar #\\)
(: 'display_char)
  (putchar-tail TEMPH)

(: 'write_string)
  (cmp 'true UNEV)
  (jne 'display_string)
  (putchar #\")
(: 'puts_escape)
  (mov 'output TEMP)
  (mov (@ VAL) EXP)
  (shr LENGTH_SHIFT EXP)
  (mov (@ 4 VAL) UNEV)
  (test EXP EXP)
  (jz 'write_string_end)
(: 'puts_escape_segment)
  (mov 0 (@ 'str_len))
  (mov 'output ARGL)
(: 'puts_escape_loop)
  (cmp 'outputend ARGL)
  (jge 'puts_escape_flush)
  (lodsb)
  (cmpb #\" VAL)
  (je 'puts_escape_backslash)
  (cmpb #\\ VAL)
  (je 'puts_escape_backslash)
(: 'puts_escape_continue)
  (stosb)
  (inc (@ 'str_len))
  (dec EXP)
  (jnz 'puts_escape_loop)
  (call 'puts)
(: 'write_string_end)
  (clear VAL ARGL)
  (mov 'true UNEV) ;keep writing!
  (putchar-tail #\")

(: 'puts_escape_backslash)
  (movb #\\ (@ ARGL))
  (inc ARGL)
  (inc (@ 'str_len))
  (jmp 'puts_escape_continue)

(: 'puts_escape_flush)
  (push 'puts_escape_segment)
  (jmpl 'puts)

(: 'display_string)
  (mov (@ VAL) TEMP)
  (shr LENGTH_SHIFT TEMP)
  (mov TEMP (@ 'str_len))
  (mov (@ 4 VAL) TEMP)
  (jmpl 'puts)

(: 'write_symbol)
  (push UNEV)
  (mov VAL TEMP)
  (add 4 TEMP)
  (mov TEMP UNEV)
(: 'write_symbol_loop)
  (inc UNEV) ;symbol will always have at least one character.
  (cmpb 0 (@ UNEV))
  (jne 'write_symbol_loop)
  (sub TEMP UNEV)
  (mov UNEV (@ 'str_len))
  (pop UNEV)
  (jmpl 'puts)

(: 'write_pair)
  (putchar #\()

(: 'write_car)
  (push VAL)
  (mov (@ VAL) VAL)
  (call 'display_dispatch)

(: 'write_cdr)
  (putchar #\space)
  (pop VAL)
  (mov (@ 4 VAL) VAL)
  (test VAL VAL)
  (jz 'write_end_pair)
  (test 1 (@ VAL))
  (jz 'write_car)
  (putchar #\.)
  (putchar #\space)
  (call 'display_dispatch)
(: 'write_end_pair)
  (putchar-tail #\))

(: 'write_hexadecimal_number)
  (mov #x10 (@ 'radix))
  (jmp 'write_digits)

(: 'write_decimal_number)
  (mov 10 (@ 'radix))
(: 'write_digits)
  (call 'stringify_integer)
  (jmpl 'puts)

(: 'write_closure)
  (new-message 'msg_closure "closure:")
  (init-message 'msg_closure)
  (call 'puts)
(: 'write_closure_entry_point)
  (mov (@ 4 VAL) VAL) ;(env . (ASSEMBLY . VECTOR)) if compiled
  (mov (@ 4 VAL) VAL) ;(ASSEMBLY . VECTOR) if compiled
  (mov (@ 4 VAL) TEMP)
  (test 1 (@ TEMP)) ;PAIR if not compiled
  (jzl 'write_close_bracket) ;not compiled yet
  (mov (@ VAL) VAL) ;assembly
  (mov (@ 4 VAL) VAL) ;addr
  (jmpl 'write_other)

(: 'write_macro)
  (new-message 'msg_macro "macro:")
  (init-message 'msg_macro)
  (call 'puts)
  (jmpl 'write_closure_entry_point)

(: 'write_continuation)
  (new-message 'msg_continuation "continuation:")
  (init-message 'msg_continuation)
  (call 'puts)
  (jmp 'write_other)

(: 'write_primitive)
  (new-message 'msg_primitive "primitive:")
  (init-message 'msg_primitive)
  (call 'puts)
  (jmp 'write_other)

(: 'write_syntax)
  (new-message 'msg_syntax "syntax:")
  (init-message 'msg_syntax)
  (call 'puts)
  (jmp 'write_other)

(: 'write_port)
  (new-message 'msg_port "port:")
  (init-message 'msg_port)
  (call 'puts)
  (jmp 'write_other)

(: 'write_assembly)
  (new-message 'msg_assembly "assembly:")
  (init-message 'msg_assembly)
  (call 'puts)

(: 'write_other)
  (mov #x10 (@ 'radix))
  (call 'stringify_integer32)
  (call 'puts)
(: 'write_close_bracket)
  (putchar-tail #\>)

(: 'write_eof)
  (new-message 'ext_eof "eof>")
  (init-message 'ext_eof)
  (jmpl 'puts)

(: 'write_vector)
  (putchar #\()
  (shr LENGTH_SHIFT TEMP) ;TEMP now holds length of vector
  (jmp 'write_vector_loop_begin)

(: 'write_vector_loop)
  (add 4 VAL)
  (push VAL)
  (push TEMP)
  (mov (@ VAL) VAL)
  (call 'display_dispatch)
  (putchar #\space)
  (pop TEMP)
  (pop VAL)
(: 'write_vector_loop_begin)
  (dec TEMP)
  (jnl 'write_vector_loop)
(: 'write_end_vector)
  (putchar #\))
  (clear VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "write-char")
  (insure-more-args ARGL)
  (mov (@ ARGL) VAL)
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_output_port)
  (fetch-char VAL TEMP)
  (putchar-tail TEMPH)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "newline")
  (call 'get_last_output_port)
  (putchar-tail 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "current-input-port")
  (mov (@ 'current_input_port) VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "current-output-port")
  (mov (@ 'current_output_port) VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "open-output-file")
  (mov OUTPUT_PORT (@ FREE))
  (mov GENERIC_WRITE (@ 'io_file))
  (jmpl 'open_io_file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "open-input-file")
  (mov INPUT_PORT (@ FREE))
  (mov GENERIC_READ (@ 'io_file))
(: 'open_io_file)
  (call 'get_last_string)
  (shr LENGTH_SHIFT TEMP)
  (mov TEMP (@ 'str_len))
  (mov (@ 4 ARGL) TEMP)
(: 'open_io_file_scratch)
  (call 'open_file)
  (mov (@ 'io_file) TEMP)
  (mov TEMP (@ 4 FREE))
  (mov FREE VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (prim-call-with-file p)
  (insure-more-args ARGL)
  (mov (@ 4 ARGL) VAL) ;(proc)
  (mov 0 (@ 4 ARGL)) ;ARGL <- (path)
  (insure-one-last-arg VAL)
  (mov (@ VAL) VAL) ;proc
  (save VAL)
  (call-prim p)
  (mov (object VAL 0) UNEV) ;(port)
  (call 'advance_free)
  (restore VAL) ;proc
  (mov (@ UNEV) ARGL) ;port
  (save ARGL)
  (call 'apply_direct)
  (restore ARGL) ;port
  (jmpl 'close_port_argl))

;;Apply non-null procedure in VAL to args in UNEV
;;WITHOUT evaluating them.
(: 'apply_direct)
  (mov (@ VAL) TEMP)
  (cmpb PROCEDURE TEMP)
  (jnel 'error_expected_procedure)
  (opd-size)(cmp SYNTAX_PRIMITIVE TEMP)
  (jel 'apply_builtin)
  (mov UNEV ARGL)
  (jmpl 'apply_dispatch)

(new-primitive "call-with-input-file")
  (prim-call-with-file "open-input-file")

(new-primitive "call-with-output-file")
  (prim-call-with-file "open-output-file")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "load")
  (call 'prim_string->symbol)
  (lea (@ 4 VAL) TEMP)
(: 'prim_load_scratch)
  (push (@ 'load_filename))
  (push TEMP)
  (mov GENERIC_READ (@ 'io_file))
  (mov INPUT_PORT (@ FREE))
  (call 'open_io_file_scratch)
  (pop (@ 'load_filename))
  (mov VAL ARGL)

  (mov 1 (@ 'load_line))
  (jmp 'prim_load_top_level_loop_begin)
(: 'prim_load_top_level_loop)
  (mov (@ 4 ENV) ENV)
(: 'prim_load_top_level_loop_begin)
  (cmp 0 (@ 4 ENV))
  (jne 'prim_load_top_level_loop)
  (mov (object ARGL ENV) VAL) ;(INPUT_PORT . ENV)
  (call 'advance_free)
  (save VAL)
  (jmp 'load_scheme_loop_begin)

(: 'load_scheme_loop)
  (mov VAL EXP)
  (mov (@ 'root) ENV)
  (mov (@ ENV) ENV) ;(INPUT_PORT . ENV)
  (mov (@ 4 ENV) ENV)
  (push (@ 'load_line))
  (call 'eval_dispatch)
  (pop (@ 'load_line))
  (mov (@ 'root) ARGL) 
  (mov (@ ARGL) ARGL) ;(INPUT_PORT . ENV)
  (mov (@ ARGL) ARGL)
(: 'load_scheme_loop_begin)
  (call 'set_input_port)
  (call 'prim_read_argl)
  (cmp 'eof VAL)
  (jne 'load_scheme_loop)

  (restore ARGL) ;(INPUT_PORT . ENV)
  (mov (@ ARGL) ARGL) ;INPUT_PORT
  (mov (@ 4 ARGL) TEMP)
  (pop (@ 'load_filename))
  (jmpl 'close_file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "close-input-port")
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) ARGL)
  (test ARGL ARGL)
  (jzl 'error_expected_input_port)
  (mov (@ ARGL) TEMP)
  (opd-size)(cmp INPUT_PORT TEMP)
  (jnel 'error_expected_input_port)
(: 'close_port_argl)
  (mov (@ 4 ARGL) TEMP)
  (mov -1 (@ 4 ARGL))
  (jmpl 'close_file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "close-output-port")
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) ARGL)
  (test ARGL ARGL)
  (jzl 'error_expected_output_port)
  (mov (@ ARGL) TEMP)
  (cmp OUTPUT_PORT TEMP)
  (jnel 'error_expected_output_port)
  (mov (@ 4 ARGL) TEMP)
  (mov -1 (@ 4 ARGL))
  (jmpl 'close_file)
