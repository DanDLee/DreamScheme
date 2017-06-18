;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicate Primitives ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'return_true)
  (return-true)

(: 'return_false)
  (return-false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "not")
  (insure-one-last-arg ARGL)
  (cmp 'false (@ ARGL))
  (je 'return_true)
  (mov 'false VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "list?")
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) ARGL)
  (clear UNEV) ;to keep list of pairs visited
(: 'prim_list_loop)
  (test ARGL ARGL)
  (jz 'return_true)
  (test 1 (@ ARGL))
  (jnz 'prim_list_false)
  (mov UNEV TEMP)
  (jmp 'prim_list_visited_loop_begin)

(: 'prim_list_visited_loop)
  (mov (@ 4 TEMP) TEMP)
(: 'prim_list_visited_loop_begin)
  (test TEMP TEMP)
  (jz 'prim_list_not_visited)
  (cmp (@ TEMP) ARGL)
  (jne 'prim_list_visited_loop)
(: 'prim_list_false)
  (return-false)

(: 'prim_list_not_visited)
  (mov (object ARGL UNEV) UNEV)
  (call 'advance_free)
  (mov (@ 4 ARGL) ARGL)
  (jmp 'prim_list_loop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Major Type Predicates ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "eof-object?")
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) TEMP)
  (test ecx ecx)
  (jzl 'return_false)
  (cmp EOF (@ TEMP))
  (jnel 'return_false)
  (mov 'true VAL)
  (ret)
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "null?")
  (insure-one-last-arg ARGL)
  (cmp 0 (@ ARGL))
  (jel 'return_true)
  (mov 'false VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "pair?")
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) TEMP)
  (test TEMP TEMP)
  (jzl 'return_false)
  (test 1 (@ TEMP))
  (jnzl 'return_false)
  (return-true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'test_major_type)
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) TEMP)
  (test TEMP TEMP)
  (jzl 'return_false)
  (mov (@ TEMP) TEMP)
  (cmpb VAL TEMP)
  (jnzl 'return_false)
  (return-true)

(for-each
  (lambda (x)
    (new-primitive (car x))
    (mov (cdr x) VAL)
    (jmp 'test_major_type))
  (list
    (cons "symbol?" SYMBOL)
    (cons "number?" NUMBER)
    (cons "procedure?" PROCEDURE)
    (cons "boolean?" BOOLEAN)
    (cons "char?" CHAR)
    (cons "string?" TYPE_STRING)
    (cons "port?" INPUT_PORT)
    (cons "vector?" TYPE_VECTOR)))

(: 'test_minor_type)
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) TEMP)
  (test TEMP TEMP)
  (jzl 'return_false)
  (mov (@ TEMP) TEMP)
  (opd-size)(cmp VAL TEMP)
  (jnzl 'return_false)
  (return-true)

(for-each
  (lambda (x)
    (new-primitive (car x))
    (mov (cdr x) VAL)
    (jmp 'test_minor_type))
  (list
    (cons "input-port?" INPUT_PORT)
    (cons "output-port?" OUTPUT_PORT)))

(for-each
  (lambda (x)
    (new-additional-primitive (car x))
    (mov (cdr x) VAL)
    (jmp 'test_minor_type))
  (list
    (cons "closure?" CLOSURE)
    (cons "macro?" MACRO_CLOSURE)
    (cons "syntax?" SYNTAX_PRIMITIVE)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mathematical Predicates ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(for-each
  (lambda (x)
    (new-primitive (car x))
    (mov (cdr x) VAL)
    (jmp 'test_type_complexity))
  (list
    (cons "complex?" COMPLEX)
    (cons "real?" REAL)
    (cons "rational?" RATIONAL)
    (cons "integer?" INTEGER)))

(: 'test_type_complexity)
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) TEMP)
  (test TEMP TEMP)
  (jzl 'return_false)
  (mov (@ TEMP) TEMP)
  (cmpb VAL TEMP)
  (jnel 'return_false)
  (cmpb VALH TEMPH)
  (jal 'return_false)
  (return-true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "zero?")
  (call 'get_last_number)
  (opd-size)(cmp INTEGER TEMP)
  (jnel 'return_false)
  (cmp 0 (@ 4 VAL))
  (jzl 'return_true)
  (return-false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "exact?")
  (insure-one-last-arg ARGL)
  (call 'get_number)
  (test #x10000 TEMP)
  (jzl 'return_true)
  (return-false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "inexact?")
  (insure-one-last-arg ARGL)
  (call 'get_number)
  (test #x10000 TEMP)
  (jnzl 'return_true)
  (return-false)

;;;;;;;;;;;;;;;;;;;;;;;
;;; Char Predicates ;;;
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "char-alphabetic?")
  (call 'get_last_char_ascii)
  (cmpb #\a TEMP)
  (jb 'prim_char_upper_case_test)
  (cmpb #\z TEMP)
  (jal 'return_false)
  (return-true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "char-lower-case?")
  (call 'get_last_char_ascii)
  (cmpb #\a TEMP)
  (jbl 'return_false)
  (cmpb #\z TEMP)
  (jal 'return_false)
  (return-true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "char-upper-case?")
  (call 'get_last_char_ascii)
(: 'prim_char_upper_case_test)
  (cmpb #\A TEMP)
  (jbl 'return_false)
  (cmpb #\Z TEMP)
  (jal 'return_false)
  (return-true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "char-numeric?")
  (call 'get_last_char_ascii)
  (cmpb #\0 TEMP)
  (jbl 'return_false)
  (cmpb #\9 TEMP)
  (jal 'return_false)
  (return-true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "char-whitespace?")
  (call 'get_last_char_ascii)
  (cmpb #\space TEMP)
  (jel 'return_true)
  (cmpb 9 TEMP) ;tab
  (jel 'return_true)
  (cmpb 10 TEMP) ;linefeed
  (jel 'return_true)
  (cmpb 12 TEMP) ;formfeed
  (jel 'return_true)
  (cmpb 13 TEMP) ;carriage-return
  (jel 'return_true)
  (return-false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Equivalence Predicates ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "eq?")
  (test ARGL ARGL)
  (jzl 'return_true)
  (mov (@ ARGL) EXP)
(: 'eq_loop)
  (mov (@ 4 ARGL) ARGL)
  (test ARGL ARGL)
  (jzl 'return_true)
  (cmp (@ ARGL) EXP)
  (je 'eq_loop)
  (mov 'false VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "eqv?")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;first argument
  (mov (@ 4 ARGL) UNEV)
  (insure-one-last-arg UNEV)
  (mov (@ UNEV) VAL) ;second argument

  (cmp EXP VAL)
  (jel 'return_true)
  (test VAL VAL)
  (jz 'prim_eqv_pred_false)
  (test EXP EXP)
  (jz 'prim_eqv_pred_false)
  (mov (@ VAL) TEMP)
  (cmp (@ EXP) TEMP)
  (jne 'prim_eqv_pred_false)
  (test 1 TEMP) ;pairs are not allowed to be eqv? unless eq?
  (jz 'prim_eqv_pred_false)
  (cmpb TYPE_VECTOR TEMP) ;vectors are not allowed to be eqv? unless eq?
  (je 'prim_eqv_pred_false)
  (cmpb SYMBOL TEMP) ;symbols cannot be eqv? unless eq?
  (je 'prim_eqv_pred_false)
  (cmpb INTEGER TEMP)
  (jel (prim-name "="))
  (mov (@ 4 VAL) TEMP)
  (cmp (@ 4 EXP) TEMP)
  (jel 'return_true)
(: 'prim_eqv_pred_false)
  (mov 'false VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'compare_chars)
  (test ARGL ARGL)
  (jzl 'return_true)
  (mov (@ ARGL) EXP)
  (fetch-char EXP TEMP)
(: 'compare_chars_loop)
  (mov (@ 4 ARGL) ARGL)
  (test ARGL ARGL)
  (jzl 'return_true)
  (mov (@ ARGL) EXP)
  (fetch-char EXP VAL)
  (cmpb VALH TEMPH)
  (jmpn (@ 'thunk))

(define (make-char-comparator name type ci)
  (new-primitive name)
  (let ((comparator (symbol-seq)))
    (mov comparator (@ 'thunk))
    (jmpl (if ci 'compare_chars_ci 'compare_chars))
    (: comparator)
    (type (if ci 'compare_chars_ci_loop 'compare_chars_loop))
    (return-false)))

(make-char-comparator "char=?" je #f)
(make-char-comparator "char>?" ja #f)
(make-char-comparator "char<?" jb #f)
(make-char-comparator "char>=?" jael #f)
(make-char-comparator "char<=?" jbel #f)

(define (ascii-to-lower reg)
  (let ((continue (symbol-seq)))
    (cmpb #\A reg)
    (jb continue)
    (cmpb #\Z reg)
    (ja continue)
    (addb 32 reg)
    (: continue)))

(: 'compare_chars_ci)
  (test ARGL ARGL)
  (jzl 'return_true)
  (mov (@ ARGL) EXP)
  (fetch-char EXP TEMP)
  (ascii-to-lower TEMPH)
(: 'compare_chars_ci_loop)
  (mov (@ 4 ARGL) ARGL)
  (test ARGL ARGL)
  (jzl 'return_true)
  (mov (@ ARGL) EXP)
  (fetch-char EXP VAL)
  (ascii-to-lower VALH)
  (cmpb VALH TEMPH)
  (jmpn (@ 'thunk))

(make-char-comparator "char-ci=?" je #t)
(make-char-comparator "char-ci>?" ja #t)
(make-char-comparator "char-ci<?" jb #t)
(make-char-comparator "char-ci>=?" jael #t)
(make-char-comparator "char-ci<=?" jbel #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'compare_strings)
  (test ARGL ARGL)
  (jzl 'return_true)
  (mov ARGL EXP)
  (mov (@ EXP) UNEV)
  (test UNEV UNEV)
  (jzl 'error_expected_string)
  (mov (@ UNEV) VAL)
  (cmpb TYPE_STRING VAL)
  (jnel 'error_expected_string)
  (shr LENGTH_SHIFT VAL) ;length of first string
  (mov (@ 4 UNEV) UNEV)
(: 'compare_strings_loop)
  (mov (@ 4 EXP) EXP)
  (test EXP EXP)
  (jz 'compare_strings_true)
  (mov (@ EXP) ARGL)
  (test ARGL ARGL)
  (jzl 'error_expected_string)
  (mov (@ ARGL) TEMP)
  (cmpb TYPE_STRING TEMP)
  (jnel 'error_expected_string)
  (shr LENGTH_SHIFT TEMP) ;length of second string
  (mov (@ 4 ARGL) ARGL) ;first character
  (mov ARGL ENV) ;remember second string for next pair
  (opd-size)(cmp TEMP VAL)
  (ifb
    (begin
      (opd-size)(mov VAL TEMP))) ;use shorter string
  (opd-size)(mov TEMP VAL) ;remember length of shorter string
  (jmpn (@ 'thunk))

(: 'compare_strings_ci_next)
  (pop EXP)
  (pop VAL)
(: 'compare_strings_next)
  (mov ENV UNEV) ;second string becomes first
  (jmp 'compare_strings_loop)

(: 'compare_strings_true)
  (mov 'true VAL)
  (jmp 'compare_strings_end)

(: 'compare_strings_ci_false)
  (pop EXP)
  (pop VAL)
(: 'compare_strings_false)
  (mov 'false VAL)
(: 'compare_strings_end)
  (clear UNEV ARGL ENV)
  (ret)

(define (compare-string-byte-ci)
  (lodsb)
  (movb (@ ARGL) EXP)
  (inc ARGL)
  (ascii-to-lower VAL)
  (ascii-to-lower EXP)
  (cmpb EXP VAL))

(define (make-string-comparator name type ci)
  (let ((comparator (symbol-seq))
        (comparator-loop (symbol-seq))
        (next (if ci 'compare_strings_ci_next 'compare_strings_next))
        (false (if ci 'compare_strings_ci_false 'compare_strings_false)))
    (: comparator)
    (case type
      ((=) (jnel 'compare_strings_false)))
    (cmp ARGL UNEV)
    (case type
      ((= >= <=) (jel 'compare_strings_next))
      (else (jel 'compare_strings_false)))
    (inc TEMP) ;compare the null character as well
    (if ci
      (begin
        (push VAL)
        (push EXP)))
    (: comparator-loop)
    (if ci
      (compare-string-byte-ci)
      (cmpsb))
    (case type
      ((=) (jnel false))
      ((> >=) (jal next) (jbl false))
      ((< <=) (jal false) (jbl next)))
    (loop comparator-loop)
    (case type
      ((= <= >=) (jmpl next))
      (else (jmpl false)))

    (new-primitive name)
    (mov comparator (@ 'thunk))
    (jmpl 'compare_strings)))


(make-string-comparator "string=?" '= #f)
(make-string-comparator "string>?" '> #f)
(make-string-comparator "string<?" '< #f)
(make-string-comparator "string>=?" '>= #f)
(make-string-comparator "string<=?" '<= #f) 
(make-string-comparator "string-ci=?" '= #t)
(make-string-comparator "string-ci>?" '> #t)
(make-string-comparator "string-ci<?" '< #t)
(make-string-comparator "string-ci>=?" '>= #t)
(make-string-comparator "string-ci<=?" '<= #t) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "equal?")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;first argument
  (mov (@ 4 ARGL) UNEV)
  (insure-one-last-arg UNEV)
  (mov (@ UNEV) VAL) ;second argument

  (test EXP EXP)
  (jz 'equal_null)
  (test VAL VAL)
  (jz 'equal_false)
  (test 1 (@ EXP))
  (jz 'equal_pair)
  (mov (@ EXP) TEMP)
  (cmpb TYPE_STRING TEMP)
  (je 'equal_string)
  (cmpb TYPE_VECTOR TEMP)
  (je 'equal_vector)
  (jmpl (prim-name "eqv?"))

(: 'equal_null)
  (test VAL VAL)
  (jnz 'equal_false)
  (return-true)

(: 'equal_pair)
  (test 1 (@ VAL))
  (jnz 'equal_false)
  (mov (@ EXP) TEMP)
  (mov TEMP (@ ARGL))
  (mov (@ VAL) TEMP)
  (mov TEMP (@ UNEV))
  (push ARGL)
  (push UNEV)
  (push VAL)
  (push EXP)
  (call-prim "equal?") ;compare cars
  (cmp 'false VAL)
  (je 'equal_pair_false)
  (pop EXP)
  (pop VAL)
  (pop UNEV)
  (pop ARGL)
  (mov (@ 4 EXP) TEMP)
  (mov TEMP (@ ARGL))
  (mov (@ 4 VAL) TEMP)
  (mov TEMP (@ UNEV))
  (jmpl (prim-name "equal?")) ;compare cdrs

(: 'equal_pair_false)
  (add 16 SP)
  (ret)

(: 'equal_string)
  (cmpb TYPE_STRING (@ VAL))
  (jel (prim-name "string=?"))
(: 'equal_false)
  (return-false)

(: 'equal_vector)
  (cmp TEMP (@ VAL))
  (jnel 'return_false)
  (shr LENGTH_SHIFT TEMP) ;vector length
  (jzl 'return_true)
  (push TEMP)
  (jmp 'equal_vector_loop_begin)

(: 'equal_vector_loop)
  (push ARGL)
  (push UNEV)
  (push VAL)
  (push EXP)
  (call-prim "equal?")
  (cmp 'false VAL)
  (je 'equal_vector_false)
  (pop EXP)
  (pop VAL)
  (pop UNEV)
  (pop ARGL)
  (mov (@ SP) TEMP)
(: 'equal_vector_loop_begin)
  (mov (@ 0 EXP TEMP 2) TEMP)
  (mov TEMP (@ ARGL))
  (mov (@ SP) TEMP)
  (mov (@ 0 VAL TEMP 2) TEMP)
  (mov TEMP (@ UNEV))
  (dec (@ SP))
  (jnz 'equal_vector_loop)
  (add 4 SP)
  (jmpl (prim-name "equal?"))

(: 'equal_vector_false)
  (add 20 SP)
  (ret)
