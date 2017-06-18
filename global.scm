;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global Constants, Variables, and Macros ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define syntax '())
(define primitives '())
(define additional-primitives '())
(define messages '())

(define (power b p)
  (if (zero? p)
    1
    (* b (power b (- p 1)))))
(define TOPLEVEL_SIZE (power 2 TOPLEVELPOWER))
(define TOPLEVEL_HASH_MASK (* 4 (- TOPLEVEL_SIZE 1)))

;;;;;;;;;;;;;
;;;Objects;;;
;;;;;;;;;;;;;
;;;Object types all must have their LSB set
;;;to distinguish them from the car field of pairs

(define (lohi lo hi)
  ((if (negative? lo) - +) lo (* hi 256)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Negative types are not copied by the garbage collector

;;;;;;;;;;;;;
;;;Symbols;;;
;;;;;;;;;;;;;
(define SYMBOL -9)

;;;;;;;;;;;;;;;;
;;;Characters;;;
;;;;;;;;;;;;;;;;
(define CHAR -7) ;High byte holds character value

;;;;;;;;;;;;;;
;;;Booleans;;;
;;;;;;;;;;;;;;
(define BOOLEAN -5)
(define BOOLEAN_TRUE (lohi BOOLEAN 1))
(define BOOLEAN_FALSE (lohi BOOLEAN 0))

;;;;;;;;;;;;;;;;;
;;;End of File;;;
;;;;;;;;;;;;;;;;;
(define EOF -3)

;;;;;;;;;;;;;;;;;;
;;;BROKEN_HEART;;;
;;;;;;;;;;;;;;;;;;
;;is used during garbage collection as a forwarding address
(define BROKEN_HEART -1)

;;;;;;;;;;;;;;;;
;;;Procedures;;;
;;;;;;;;;;;;;;;;
(define PROCEDURE 1)

;;;A SYNTAX_PRIMITIVE is a pointer to a procedure accepting
;;;arguments in UNEV
(define SYNTAX_PRIMITIVE (lohi PROCEDURE -2))

;;;A PRIMITIVE is a pointer to a procedure accepting
;;;arguments in ARGL
(define PRIMITIVE (lohi PROCEDURE -4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Positive types are copied by the garbage collector, and
;;;if (test 256 type) is non-zero then cdr field is copied as well.
(define ASSEMBLY (lohi PROCEDURE 0))

;;;A CLOSURE is (environment formals . body)
;;;where environment is (bindings . enclosing_scope)
;;;where bindings are ((symbol . value) ...)
(define CLOSURE (lohi PROCEDURE 1))

;;;A CONTINUATION is (root . stack)
(define CONTINUATION (lohi PROCEDURE 3))

;;;A MACRO_CLOSURE is stored in the same way as a CLOSURE
(define MACRO_CLOSURE (lohi PROCEDURE 5))

;;;;;;;;;;;;;
;;;Numbers;;;
;;;;;;;;;;;;;
(define NUMBER 3) ;Lowest bit of high word marks exactness.

;;;The following values must increase as the numeric complexity increases.
(define INTEGER (lohi NUMBER 0)) ;Bit #x00020000 marks sign (set = negative).
                                 ;High word, with lowest two bits masked out,
                                 ;holds length when integer value exceeds 32 bits.

;;;A RATIONAL is (numerator . denominator)
;;;where numerator and denominator are both integers
;;;so that the value is produced by (/ numerator denominator)
(define RATIONAL (lohi NUMBER 1))

;;;A REAL is a one argument lambda.
(define REAL (lohi NUMBER 3))

;;;A COMPLEX should be (real-part . imaginary-part)
(define COMPLEX (lohi NUMBER 5))

;;;;;;;;;;;
;;;Ports;;;
;;;;;;;;;;;
(define PORT 5)
(define INPUT_PORT (lohi PORT 0)) ;Low byte of high word holds peeked char
(define OUTPUT_PORT (lohi PORT 2))

;;;;;;;;;;;;;
;;;Strings;;;
;;;;;;;;;;;;;
(define LENGTH_SHIFT 10) ;applicable to both strings and vectors
(define TYPE_STRING 7) ;High 22 bits hold length, sign clear (up to #x1fffff)
(define MUTABLE_STRING (lohi TYPE_STRING 0))
(define IMMUTABLE_STRING (lohi TYPE_STRING 2))
(define STRING_IMMUTABILITY (lohi 0 2))

;;;;;;;;;;;;;
;;;Vectors;;;
;;;;;;;;;;;;;
(define TYPE_VECTOR 9) ;High 22 bits hold length, sign clear (up to #x1fffff)
(define SCHEME_VECTOR (lohi TYPE_VECTOR 1))

;;;;;;;;;;;;;;;
;;;Registers;;;
;;;;;;;;;;;;;;;
(define SP esp)
(define EXP edx)
(define ENV ebp)
(define UNEV esi)
(define ARGL edi)
(define VAL eax)
(define VALH ah)
(define VALL al)
(define FREE ebx)
(define TEMP ecx)
(define TEMPH ch)

;;;Garbage collector registers
(define OLD esi)
(define NEW edi)
(define SCAN eax)

;;;;;;;;;;;;
;;;Macros;;;
;;;;;;;;;;;;

(define (putchar c)
  (movb c (@ 'output))
  (call 'putch))
(define (putchar-tail c)
  (movb c (@ 'output))
  (jmpl 'putch))

(define (new-message s x)
  (set! messages
    (cons (cons s x) messages)))
(define (message-length s)
  (string-length (cdr (assq s messages))))
(define (init-message s)
  (mov s TEMP)
  (mov (message-length s) (@ 'str_len)))

(define (symbol-name z)
  (string->symbol
    (string-append "sym_" z)))

(define (proc-name z)
  (string->symbol
    (string-append "proc_" z)))

(define (prim-name z)
  (string->symbol
    (string-append "prim_" z)))

(define (call-prim z)
  (call (prim-name z)))

(define (new-syntax x)
  (display x)
  (write-char #\space)
  (set! syntax (cons x syntax))
  (: (prim-name x)))

(define (new-primitive x)
  (display x)
  (write-char #\space)
  (set! primitives (cons x primitives))
  (: (prim-name x)))

(define (new-additional-primitive x)
  (display x)
  (write-char #\space)
  (set! additional-primitives (cons x additional-primitives))
  (: (prim-name x)))

(define (stack_push . regs) ;uses TEMP
  (for-each
    (lambda (reg)
      (mov reg (@ FREE))
      (mov (@ 'root) TEMP)
      (mov TEMP (@ 4 FREE))
      (mov FREE (@ 'root))
      (add 8 FREE))
    regs))

(define (save . regs) ;uses TEMP
  (for-each
    (lambda (reg)
      (let ((no_gc (symbol-seq)))
        (stack_push reg)
        (cmp (@ 'memlimit) FREE)
        (if (not DEBUG) (jl no_gc))
        (call 'gc_sans_strings)
        (: no_gc)))
    regs))

(define (save-with-ignored reg ignore) ;uses TEMP
  (let ((no_gc (symbol-seq)))
    (stack_push reg)
    (cmp (@ 'memlimit) FREE)
    (if (not DEBUG) (jl no_gc))
    (push ignore)
    (clear ignore)
    (call 'gc_sans_strings)
    (pop ignore)
    (: no_gc)))

(define (restore . regs) ;uses TEMP
  (for-each
    (lambda (reg)
      (mov (@ 'root) TEMP)
      (if reg (mov (@ TEMP) reg))
      (mov (@ 4 TEMP) TEMP)
      (mov TEMP (@ 'root)))
    regs))

(define (peek x)
  (mov (@ 'root) x)
  (mov (@ x) x))

(define (insure-no-more-args reg)
  (test reg reg)
  (jnzl 'error_too_many_args))

(define (insure-more-args reg)
  (test reg reg)
  (jzl 'error_too_few_args))

(define (insure-last-arg reg)
  (cmp 0 (@ 4 reg))
  (jnel 'error_too_many_args))

(define (insure-one-last-arg reg)
  (insure-more-args reg)
  (insure-last-arg reg))

(define (insure-object-is-pair reg)
  (test 1 (@ reg))
  (jnzl 'error_expected_pair))

(define (insure-list reg)
  (test reg reg)
  (ifnz (insure-object-is-pair reg)))

(define (insure-pair reg)
  (test reg reg)
  (jzl 'error_expected_pair)
  (insure-object-is-pair reg))

(define (insure-vector reg head)
  (test reg reg)
  (jzl 'error_expected_vector)
  (mov (@ reg) head)
  (cmpb TYPE_VECTOR head)
  (jnel 'error_expected_vector))

(define (insure-string reg head)
  (test reg reg)
  (jzl 'error_expected_string)
  (mov (@ reg) head)
  (cmpb TYPE_STRING head)
  (jnel 'error_expected_string))

(define (fetch-char source dest)
  (test source source)
  (jzl 'error_expected_char)
  (mov (@ source) dest)
  (cmpb CHAR dest)
  (jnel 'error_expected_char))

;Standard case is lower-case
(define (to_lower byte_reg)
  (let ((standard_case (symbol-seq)))
      (cmpb #\A byte_reg)
      (jb standard_case)
      (cmpb #\Z byte_reg)
      (ja standard_case)
      (addb 32 byte_reg)
    (: standard_case)))

(define (ascii-to-char src dest)
  (lea (@ 'chararray #f src 2) dest))

(define (get-number-exactness x) ;EXP <- number, TEMP <- header
  (mov (@ ARGL) EXP)
  (test EXP EXP)
  (jzl 'error_expected_number)
  (mov (@ EXP) TEMP)
  (test #x00010000 TEMP)
  (ifne (or! #x00010000 (@ x))))

(define (get-integer-exactness x) ;EXP <- integer, TEMP <- header
  (get-number-exactness x)
  (opd-size)(cmp INTEGER TEMP)
  (jnel 'error_expected_integer))

(define (clear . regs)
  (for-each
    (lambda (reg)
      (xor reg reg))
    regs))

(define (object type value)
  (mov type (@ FREE))
  (mov value (@ 4 FREE))
  FREE)

(define (return-true)
  (mov 'true VAL)
  (ret))

(define (return-false)
  (mov 'false VAL)
  (ret))
