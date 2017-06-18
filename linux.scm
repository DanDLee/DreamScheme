;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Linux Kernel Interface;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(call number goes in eax.
;;Up to 5 arguments go in ebx, ecx, edx, esi, edi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Linux Interface Constants;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define STDIN 0)
(define STDOUT 1)
(define FCGI_IN_OUT -2)
(define STDERR (if LIBFCGI FCGI_IN_OUT 2))
(define O_RDONLY 0)
(define O_WRONLY 1)
(define O_CREAT 64)
(define O_TRUNC 512)
(define GENERIC_READ O_RDONLY)
(define GENERIC_WRITE (+ O_WRONLY O_CREAT O_TRUNC))

;;;;;;;;;;;;;;;;;;;;;
;;;Syscall Numbers;;;
;;;;;;;;;;;;;;;;;;;;;

;int sys_exit(int status)
(define SYS_EXIT 1)

;ssize_t sys_read(unsigned int fd, char* buf, size_t count)
(define SYS_READ 3)

;ssize_t sys_write(unsigned int fd, const char* buf, size_t count)
(define SYS_WRITE 4)

;int sys_open(const char* filename, int flags, int mode)
(define SYS_OPEN 5)

;sys_close(unsigned int fd)
(define SYS_CLOSE 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (exit-with-code x)
(if LIBFCGI
  (calln (@ 'FCGX_Finish)))
  (mov x ebx)
  (mov SYS_EXIT eax)
  (int #x80))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Initialize Default Input and Output Ports;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'initialize_ports)
(if LIBFCGI
  (mov (object INPUT_PORT FCGI_IN_OUT) (@ 'current_input_port))
  (mov (object INPUT_PORT STDIN) (@ 'current_input_port)))
  (add 8 FREE)
(if LIBFCGI
  (mov (object OUTPUT_PORT FCGI_IN_OUT) (@ 'current_output_port))
  (mov (object OUTPUT_PORT STDOUT) (@ 'current_output_port)))
  (add 8 FREE)
(if LIBFCGI
  (begin
    (push 1)
    (push 'fcgi_name)
    (calln (@ 'dlopen_rel))
    (mov eax (@ 'fcgi))
    (add 8 esp)
    (test eax eax)
    (jzl 'error_no_fcgi)
    (for-each
      (lambda (x)
        (push (car x))
        (push (@ 'fcgi))
        (calln (@ 'dlsym_rel))
        (add 8 esp)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Load Files Specified on Command-Line;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'parse_command_line)
  (lea (@ 12 SP) TEMP)
  (cmp 0 (@ TEMP))
  (jnz 'command_line_begin)
(if LIBFCGI
  (exit-with-code 0)
  (ret))
(: 'command_line_begin)
  (mov 'exit_not_ok (@ 'error_continuation))
(: 'command_line_loop)
  (push TEMP)
  (mov (@ TEMP) TEMP)
  (call 'prim_load_scratch)
  (pop TEMP)
  (add 4 TEMP)
  (cmp 0 (@ TEMP))
  (jnz 'command_line_loop)
  (exit-with-code 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Read one character from file given in io_file (tetra) and place
;;character in input (tetra).
(: 'getch)
  (pusha)
  (mov 0 (@ 'input))
(if LIBFCGI
 (begin
  (cmp FCGI_IN_OUT (@ 'io_file))
  (ife
   (begin
    (push (@ 'fcgx_in))
    (calln (@ 'FCGX_GetChar))
    (add 4 esp)
    (mov eax (@ 'input))
    (popa)
    (ret)))))
  (mov (@ 'io_file) ebx) ;File descriptor
  (mov 'input ecx)       ;Buffer
  (mov 1 edx)            ;Count
  (mov SYS_READ eax)
  (int #x80)
  (test eax eax)
  (jz 'getch_eof)
  (ifs (mov eax (@ 'input)))
  (popa)
  (ret)

(: 'getch_eof)
  (mov -1 (@ 'input))
  (popa)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Display character in output (byte) to file given in io_file (tetra)
(: 'putch)
  (pusha)
(if LIBFCGI
 (begin
  (cmp FCGI_IN_OUT (@ 'io_file))
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
  (mov (@ 'io_file) ebx) ;File descriptor
  (mov 'output ecx)      ;Buffer
  (mov 1 edx)            ;Count
  (mov SYS_WRITE eax)
  (int #x80)
  (test eax eax)
  (jsl 'error_io)
  (popa)
  (ret)

(if LIBFCGI
 (let ((content-type "Content-Type: text/html\n\n"))
  (: 'content_type)
    (asciz content-type)
  (: 'insure_content_type)
   (cmp 0 (@ 'fcgi))
   (je 'no_content_type)
   (pusha)
   (cmp 0 (@ 'fcgx_out))
   (ife (call 'call_fcgx_accept))
   (push (@ 'fcgx_out))
   (push (string-length content-type))
   (push 'content_type)
   (calln (@ 'FCGX_PutStr))
   (add 12 esp)
   (popa)
  (: 'no_content_type)
   (ret)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Display string at ecx with length in str_len to file given in io_file (tetra)
(: 'puts)
  (pusha)
(if LIBFCGI
 (begin
  (cmp FCGI_IN_OUT (@ 'io_file))
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
  (mov (@ 'io_file) ebx) ;File descriptor
  (mov (@ 'str_len) edx) ;Count
  (mov SYS_WRITE eax)
  (int #x80)
  (test eax eax)
  (jsl 'error_io)
  (popa)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Open file whose path is at ecx using flags in io_file (tetra).
;;Place descriptor in io_file
(: 'open_file)
  (pusha)
  (mov ecx ebx) ;Filename
  (mov (@ 'io_file) ecx) ;Flags
  (mov #o666 edx) ;Mode
  (mov SYS_OPEN eax)
  (int #x80)
  (test eax eax)
  (jsl 'error_io)
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
      (mov ecx ebx)
      (mov SYS_CLOSE eax)
      (int #x80)
      (cmp -1 eax)
      (jel 'error_io)
      (popa)))
  (ret)

;;;;;;;;;;;;;;;;;;;;;;
;;;Linux Primitives;;;
;;;;;;;;;;;;;;;;;;;;;;

(define regops (list ebx ecx edx esi edi))
(define (bind-regops regs x)
  (if (pair? x)
    (let ((end_ops (symbol-seq))
          (t (car x))
          (r (car regs)))
      (if (eq? t 'OPTIONAL)
        (begin
          (clear r)
          (test VAL VAL)
          (jzl end_ops))
        (begin
          (test VAL VAL)
          (jzl 'linux_error_too_few_args)))
      (mov (@ VAL) r)
      (test r r)
      (ifnz
        (case t
          ((INTEGER OPTIONAL)
            (opd-size)(cmp INTEGER (@ r))
            (jnel 'linux_error_expected_exact_integer)
            (mov (@ 4 r) r))
          ((ADDRESS)
            (opd-size)(cmp INTEGER (@ r))
            (jnel 'linux_error_expected_exact_integer)
            (lea (@ 4 r) r))
          ((INPUT_PORT)
            (opd-size)(cmp INPUT_PORT (@ r))
            (jnel 'linux_error_expected_input_port)
            (mov (@ 4 r) r))
          ((OUTPUT_PORT)
            (opd-size)(cmp OUTPUT_PORT (@ r))
            (jnel 'linux_error_expected_output_port)
            (mov (@ 4 r) r))
          ((STRING)
      	    (cmpb TYPE_STRING (@ r))
            (jnel 'linux_error_expected_string)
            (mov (@ 4 r) r))))
      (mov (@ 4 VAL) VAL)
      (: end_ops)
      (bind-regops (cdr regs) (cdr x)))))
(define (syscall num name . operands)
  (new-primitive name)
  (pusha)
  (if (<= (length operands) 5)
    (begin
      (mov ARGL VAL)
      (bind-regops regops operands)
      (test VAL VAL)
      (jnzl 'linux_error_too_many_args)
      (mov num eax))
    (begin
      (display "ERROR: I don't yet know how to handle >5 syscall operands.")
      (exit)))
  (int #x80)
  (if (> num 1) ;anything but sys-exit returns an INTEGER
    (begin
      (mov eax (@ 'backup))
      (popa)
      (mov (@ 'backup) TEMP)
      (mov (object INTEGER TEMP) VAL)
      (jmpl 'advance_free))))

(: 'linux_error_expected_exact_integer)
  (popa)
  (jmpl 'error_expected_exact_nonnegative_integer)
(: 'linux_error_expected_input_port)
  (popa)
  (jmpl 'error_expected_input_port)
(: 'linux_error_expected_output_port)
  (popa)
  (jmpl 'error_expected_output_port)
(: 'linux_error_expected_mutable_string)
  (popa)
  (jmpl 'error_expected_mutable_string)
(: 'linux_error_expected_string)
  (popa)
  (jmpl 'error_expected_string)
(: 'linux_error_too_many_args)
  (popa)
  (jmpl 'error_too_many_args)
(: 'linux_error_too_few_args)
  (popa)
  (jmpl 'error_too_few_args)

(new-primitive "exit")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     NUM NAME          OPERANDS                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(syscall  1 "sys-exit"    'OPTIONAL)
(syscall  2 "sys-fork")
(syscall  3 "sys-read"    'INPUT_PORT 'STRING 'INTEGER)
(syscall  4 "sys-write"   'OUTPUT_PORT 'STRING 'INTEGER)
(syscall  5 "sys-open"    'STRING 'INTEGER 'INTEGER)
(syscall  6 "sys-close"   'INTEGER)
(syscall  7 "sys-waitpid" 'INTEGER 'ADDRESS 'INTEGER)
(syscall  8 "sys-creat"   'STRING 'INTEGER)
(syscall  9 "sys-link"    'STRING 'STRING)
(syscall 10 "sys-unlink"  'STRING)
;(syscall 11 "sys-execve"  '???)
(syscall 12 "sys-chdir"   'STRING)
(syscall 13 "sys-time"    'ADDRESS)
(syscall 14 "sys-mknod"   'STRING 'INTEGER 'INTEGER)
(syscall 15 "sys-chmod"   'STRING 'INTEGER)
(syscall 16 "sys-lchown"  'STRING 'INTEGER 'INTEGER)
;syscall 17 "sys-break" is obsolete
;syscall 18 "sys-oldstat" is obsolete
(syscall 19 "sys-lseek"   'INTEGER 'INTEGER 'INTEGER)
(syscall 20 "sys-getpid")
(syscall 21 "sys-mount"   'STRING 'STRING 'STRING 'INTEGER 'INTEGER)
(syscall 22 "sys-umount"  'STRING)
(syscall 23 "sys-setuid"  'INTEGER)
(syscall 24 "sys-getuid")
(syscall 25 "sys-stime"   'ADDRESS)
(syscall 26 "sys-ptrace"  'INTEGER 'INTEGER 'INTEGER 'INTEGER)
(syscall 27 "sys-alarm"   'INTEGER)
(syscall 29 "sys-pause")
(syscall 106 "sys-stat"   'STRING 'STRING)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic Link Procedures ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "dlopen")
  (call 'get_last_string)
  (pusha)
  (push 1)
  (push (@ 4 ARGL))
  (calln (@ 'dlopen_rel))
  (add 8 esp)
  (mov eax (@ 'io_file))
  (popa)
  (mov (@ 'io_file) VAL)
  (mov (object INTEGER VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "dlclose")
  (call 'get_last_exact_natural)
  (pusha)
  (push (@ 4 TEMP))
  (calln (@ 'dlclose_rel))
  (add 4 esp)
  (mov eax (@ 'io_file))
  (popa)
  (mov (@ 'io_file) VAL)
  (mov (object INTEGER VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "dlsym")
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov TEMP VAL)
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_string)
  (pusha)
  (push (@ 4 ARGL))
  (push (@ 4 VAL))
  (calln (@ 'dlsym_rel))
  (add 8 esp)
  (mov eax (@ 'io_file))
  (popa)
  (mov (@ 'io_file) VAL)
  (mov (object INTEGER VAL) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "dlcall")
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (pusha)
  (mov esp (@ 'backup))
  (jmp 'dlcall_loop_begin)
(: 'dlcall_loop)
  (mov (@ ARGL) VAL)
  (test VAL VAL)
  (jz 'dlcall_null)
  (push (@ 4 VAL))
(: 'dlcall_loop_begin)
  (mov (@ 4 ARGL) ARGL)
  (test ARGL ARGL)
  (jnz 'dlcall_loop)
  (calln (@ 4 TEMP))
  (mov (@ 'backup) esp)
  (mov eax (@ 'io_file))
  (popa)
  (mov (@ 'io_file) VAL)
  (mov (object INTEGER VAL) VAL)
  (jmpl 'advance_free)
(: 'dlcall_null)
  (push 0)
  (jmp 'dlcall_loop_begin)

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
    (mov (@ 'current_input_port) ARGL)
    (mov (@ ARGL) TEMP)
    (and! #xfdffffff TEMP) ;Reset EOF condition
    (mov TEMP (@ ARGL))
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
