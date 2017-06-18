;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;;;Windows Interface;;;
;;;;;;;;;;;;;;;;;;;;;;;

(define GENERIC_READ #x80000000)
(define GENERIC_WRITE #x40000000)

(define CREATE_ALWAYS 2)
(define OPEN_EXISTING 3)

(define STDERR (if LIBFCGI 0 -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Initialize Default Input and Output Ports;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'initialize_ports)
(if LIBFCGI
  (clear eax)
  (begin
    (push -10) ;STDIN
    (calln (@ 'GetStdHandle))))
  (mov (object INPUT_PORT eax) (@ 'current_input_port))
  (add 8 FREE)
(if LIBFCGI
  (clear eax)
  (begin
    (push -11) ;STDOUT
    (calln (@ 'GetStdHandle))))
  (mov (object OUTPUT_PORT eax) (@ 'current_output_port))
  (add 8 FREE)
(if LIBFCGI
  (begin
    (push 'fcgi_name)
    (calln (@ 'dlopen_rel))
    (test eax eax)
    (jzl 'error_no_fcgi)
    (mov eax (@ 'fcgi))
    (for-each
      (lambda (x)
        (push (car x))
        (push (@ 'fcgi))
        (calln (@ 'dlsym_rel))
        (test eax eax)
        (jzl 'error_no_fcgi)
        (mov eax (@ (cdr x))))
      '((fcgx_finish_name . FCGX_Finish)
        (fcgx_getchar_name . FCGX_GetChar)
        (fcgx_putchar_name . FCGX_PutChar)
        (fcgx_putstr_name . FCGX_PutStr)
        (fcgx_accept_name . FCGX_Accept)
        (fcgx_getparam_name . FCGX_GetParam)
        (fcgx_fflush_name . FCGX_FFlush)))))
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (exit-with-code x)
  (push x)
(if LIBFCGI (calln (@ 'FCGX_Finish)))
  (calln (@ 'ExitProcess)))

(new-primitive "exit")
  (exit-with-code 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Load Files Specified on Command-Line;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'parse_command_line)
  (pusha)
  (calln (@ 'GetCommandLineA))
  (mov VAL (@ 'io_file))
  (popa)
  (mov (@ 'io_file) TEMP)
(: 'parse_command_loop)
  (cmpb #\space (@ TEMP))
  (je 'parse_file_loop)
  (cmpb 0 (@ TEMP))
  (je 'parse_command_line_return)
  (inc TEMP)
  (jmp 'parse_command_loop)
(: 'parse_file_loop)
  (inc TEMP)
  (cmpb 0 (@ TEMP))
  (je 'parse_command_line_return)
  (cmpb #\space (@ TEMP))
  (je 'parse_file_loop)
  (mov 'exit_not_ok (@ 'error_continuation))
  (call 'prim_load_scratch)
  (exit-with-code 0)
(: 'parse_command_line_return)
(if LIBFCGI
  (exit-with-code 0)
  (ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Read one character from file given in io_file (tetra) and place
;;character in input (tetra).
(: 'getch)
  (mov 0 (@ 'input))
  (pusha)
(if LIBFCGI
 (begin
  (cmp 0 (@ 'io_file))
  (ife
   (begin
    (push (@ 'fcgx_in))
    (calln (@ 'FCGX_GetChar))
    (add 4 esp)
    (mov eax (@ 'input))
    (popa)
    (ret)))))
  (push 0)
  (push 'bytes_io) ;lpNumberOfBytesRead
  (push 1) ;nNumberOfBytesToRead
  (push 'input) ;lpBuffer
  (push (@ 'io_file)) ;HANDLE
  (calln (@ 'ReadFile))
(: 'getch_end)
  (test eax eax)
  (jzl 'error_io)
  (cmp 0 (@ 'bytes_io))
  (ife (mov -1 (@ 'input)))
  (popa)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Display character in output (byte) to file given in io_file (tetra)
(: 'putch)
  (pusha)
(if LIBFCGI
 (begin
  (cmp 0 (@ 'io_file))
  (ife
   (begin
    (push (@ 'fcgx_out))
    (clear eax)
    (movb (@ 'output) eax)
    (push eax)
    (calln (@ 'FCGX_PutChar))
    (add 8 esp)
    (cmp -1 eax)
    (jel 'error_io)
    (popa)
    (ret)))))
  (push 0)
  (push 'bytes_io) ;lpNumberOfBytesWritten
  (push 1) ;nNumberOfBytesToWrite
  (push 'output) ;lpBuffer
  (cmp STDERR (@ 'io_file))
  (ife
    (begin
      (push -12)
      (calln (@ 'GetStdHandle))
      (mov eax (@ 'io_file))))
  (push (@ 'io_file)) ;HANDLE
  (calln (@ 'WriteFile))
(: 'putch_end)
  (test eax eax)
  (jzl 'error_io)
  (popa)
  (ret)

(if LIBFCGI
 (let ((content-type "Content-Type: text/html\n\n"))
  (: 'content_type)
    (asciz content-type)
  (: 'insure_content_type)
   (pusha)
   (cmp 0 (@ 'fcgx_out))
   (ife (call 'call_fcgx_accept))
   (push (@ 'fcgx_out))
   (push (string-length content-type))
   (push 'content_type)
   (calln (@ 'FCGX_PutStr))
   (add 12 esp)
   (popa)
   (ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Display string at ecx with length in str_len to file given in io_file (tetra)
(: 'puts)
  (pusha)
(if LIBFCGI
 (begin
  (cmp 0 (@ 'io_file))
  (ife
   (begin
    (push (@ 'fcgx_out))
    (push (@ 'str_len))
    (push ecx)
    (calln (@ 'FCGX_PutStr))
    (add 12 esp)
    (test eax eax)
    (jsl 'error_io)
    (popa)
    (ret)))))
  (push 0)
  (push 'bytes_io) ;lpNumberOfBytesWritten
  (push (@ 'str_len)) ;nNumberOfBytesToWrite
  (push ecx) ;lpBuffer
  (cmp STDERR (@ 'io_file))
  (ife
    (begin
      (push -12)
      (calln (@ 'GetStdHandle))
      (mov eax (@ 'io_file))))
  (push (@ 'io_file)) ;HANDLE
  (calln (@ 'WriteFile))
  (test eax eax)
  (jzl 'error_io)
  (popa)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Open file whose path is at ecx using flags in io_file (tetra).
;;Place descriptor in io_file
(: 'open_file)
  (pusha)
  (push 0) ;hTemplateFile
  (push #x80) ;dwFlagsAndAttributes (FILE_ATTRIBUTE_NORMAL)
  (cmp GENERIC_WRITE (@ 'io_file))
  (ifne
    (push OPEN_EXISTING) ;dwCreationDisposition
    (push CREATE_ALWAYS)) ;dwCreationDisposition
  (push 0) ;lpSecurityAttributes
  (push 1) ;dwShareMode (FILE_SHARE_READ)
  (push (@ 'io_file)) ;dwDesiredAccess
  (push ecx) ;lpFileName
  (calln (@ 'CreateFileA))
  (cmp -1 eax)
  (jel 'error_io)
  (mov eax (@ 'io_file))
  (popa)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Close file whose descriptor is ecx
(: 'close_file)
  (test ecx ecx)
  (ifns
    (begin
      (pusha)
      (push ecx)
      (calln (@ 'CloseHandle))
      (test eax eax)
      (jzl 'error_io)
      (popa)))
  (ret)

;;;;;;;;;;;;
;;; DLLs ;;;
;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "get-module-handle")
  (call 'get_last_string)
  (pusha)
  (push (@ 4 ARGL))
  (calln (@ 'GetModuleHandleA))
  (mov eax (@ 'io_file))
  (popa)
  (mov (@ 'io_file) VAL)
  (mov (object INTEGER VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "dlopen")
  (call 'get_last_string)
  (pusha)
  (push (@ 4 ARGL))
  (calln (@ 'dlopen_rel))
  (mov eax (@ 'io_file))
  (popa)
  (mov (@ 'io_file) VAL)
  (mov (object INTEGER VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "dlclose")
  (call 'get_last_exact_natural)
  (pusha)
  (push (@ 4 TEMP))
  (calln (@ 'dlclose_rel))
  (mov eax (@ 'io_file))
  (popa)
  (cmp 0 (@ 'io_file))
  (ifnz
    (begin
      (mov 'true VAL)
      (ret)))
  (mov 'false VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "dlsym")
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov (@ 4 ARGL) ARGL)
  (mov (@ 4 TEMP) TEMP)
  (mov TEMP (@ 'io_file))
  (call 'get_last_string)
  (pusha)
  (push (@ 4 ARGL))
  (push (@ 'io_file))
  (calln (@ 'dlsym_rel))
  (mov eax (@ 'io_file))
  (popa)
  (mov (@ 'io_file) VAL)
  (mov (object INTEGER VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "dlcall")
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (pusha)
  (mov esp (@ 'io_file))
  (jmp 'call-proc_loop_begin)
(: 'call-proc_loop)
  (mov (@ ARGL) VAL)
  (test VAL VAL)
  (ifnz (mov (@ 4 VAL) VAL))
  (push VAL)
(: 'call-proc_loop_begin)
  (mov (@ 4 ARGL) ARGL)
  (test ARGL ARGL)
  (jnz 'call-proc_loop)
  (calln (@ 4 TEMP))
  (mov (@ 'io_file) esp)
  (mov eax (@ 'io_file))
  (popa)
  (mov (@ 'io_file) VAL)
  (mov (object INTEGER VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if LIBFCGI
  (begin
   (: 'call_fcgx_accept)
    (push 'fcgx_envp)
    (push 'fcgx_err)
    (push 'fcgx_out)
    (push 'fcgx_in)
    (calln (@ 'FCGX_Accept))
    (add 16 esp)
    (ret)

    (new-additional-primitive "fcgx-accept?")
    (insure-no-more-args ARGL)
    (pusha)
    (call 'call_fcgx_accept)
    (test eax eax)
    (jsl 'fcgx_accept_false)
    (popa)
    (jmpl 'return_true)
   (: 'fcgx_accept_false)
    (popa)
    (jmpl 'return_false)

    (new-additional-primitive "fcgx-finish")
    (insure-no-more-args ARGL)
    (pusha)
    (calln (@ 'FCGX_Finish))
    (test eax eax)
    (jsl 'fcgx_finish_false)
    (popa)
    (jmpl 'return_true)
   (: 'fcgx_finish_false)
    (popa)
    (jmpl 'return_false)

    (new-additional-primitive "fcgx-flush")
    (insure-no-more-args ARGL)
    (pusha)
    (push (@ 'fcgx_out))
    (calln (@ 'FCGX_FFlush))
    (add 4 esp)
    (popa)
    (ret)
    
    (new-additional-primitive "fcgx-getparam")
    (call 'get_last_string)
    (pusha)
    (push (@ 'fcgx_envp))
    (push (@ 4 ARGL))
    (calln (@ 'FCGX_GetParam))
    (add 8 esp)
    (mov eax (@ 'io_file))
    (popa)
    (mov (@ 'io_file) eax)
    (test eax eax)
    (jzl 'return_false)
    (call 'make_immutable_string)
    (mov (@ VAL) TEMP)
    (shr LENGTH_SHIFT TEMP) ;string length
    (mov (@ 4 VAL) UNEV)
    (jmpl 'substring_unev_temp)))
