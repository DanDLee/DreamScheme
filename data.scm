;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;
;;;Data;;;
;;;;;;;;;;
(data)
(for-each
  (lambda (x)
    (: (car x))
    (asciz (cdr x)))
  messages)
(if BOOTSTRAP_FILE
 (begin
  (: 'bootstrap_file)
  (asciz BOOTSTRAP_FILE)))

(align 4)
(: 'dream_false)
(: 'false)
  (tetra BOOLEAN_FALSE)
(: 'dream_true)
(: 'true)
  (tetra BOOLEAN_TRUE)
(: 'eof)
  (tetra EOF)
(align 4)
(: 'string_slash)
  (tetra (+ IMMUTABLE_STRING #x400))
  (tetra 'slash)
(: 'slash)
  (asciz "/")
(align 4)

(define (sym t z)
  (align 4)
  (: (symbol-name z))
  (tetra t)
  (asciz z))

(for-each
  (lambda (s) (sym SYMBOL s))
  special-symbols)

(for-each
  (lambda (s) (sym SYMBOL s))
  syntax)

(for-each
  (lambda (s) (sym SYMBOL s))
  primitives)

(for-each
  (lambda (s) (sym SYMBOL s))
  additional-primitives)

(align 4)
(: 'freesymbol)     (tetra 'symbols)
(: 'radix)          (tetra 10)
(: 'root)           (tetra 0)
(: 'mem)            (tetra 'mem1)
(: 'dream_memlimit)
(: 'memlimit)       (tetra 'memlimit1)
(: 'memnew)         (tetra 'mem2)
(: 'memlimitnew)    (tetra 'memlimit2)
(: 'memstr)         (tetra 'memstr1)
(: 'memstrlimit)    (tetra 'memstrlimit1)
(: 'memstrnew)      (tetra 'memstr2)
(: 'memstrlimitnew) (tetra 'memstrlimit2)

(if LIBFCGI
  (begin
    (: 'fcgx_in)   (tetra 0)
    (: 'fcgx_out)  (tetra 0)
    (: 'fcgx_err)  (tetra 0)
    (: 'fcgx_envp) (tetra 0)
    (: 'fcgi_name)          (asciz LIBFCGI)
    (: 'fcgx_finish_name)   (asciz "FCGX_Finish")
    (: 'fcgx_getchar_name)  (asciz "FCGX_GetChar")
    (: 'fcgx_putchar_name)  (asciz "FCGX_PutChar")
    (: 'fcgx_putstr_name)   (asciz "FCGX_PutStr")
    (: 'fcgx_accept_name)   (asciz "FCGX_Accept")
    (: 'fcgx_getparam_name) (asciz "FCGX_GetParam")
    (: 'fcgx_fflush_name)   (asciz "FCGX_FFlush")))
(align 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Built-in Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'builtins)

(for-each
  (lambda (s)
    (tetra (symbol-name s))
    (: (proc-name s))
    (tetra SYNTAX_PRIMITIVE)
    (tetra (prim-name s)))
  syntax)

(: 'first_primitive)
(for-each
  (lambda (p)
    (tetra (symbol-name p))
    (: (proc-name p))
    (tetra PRIMITIVE)
    (tetra (prim-name p)))
  primitives)

(: 'first_additional_primitive)
(for-each
  (lambda (p)
    (tetra (symbol-name p))
    (: (proc-name p))
    (tetra PRIMITIVE)
    (tetra (prim-name p)))
  additional-primitives)

(tetra 0)
