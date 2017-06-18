;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;; Vector Primitives ;;
;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "make-vector")
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov (@ 4 ARGL) ARGL)
  (mov 'false VAL) ;default fill
  (test ARGL ARGL)
  (ifnz
    (begin
      (insure-last-arg ARGL)
      (mov (@ ARGL) VAL))) ;fill
  (mov (@ 4 TEMP) TEMP)
(: 'make_vector)
  (cmp (power 2 (- 31 LENGTH_SHIFT)) TEMP)
  (jael 'error_vector_too_large)
  (push TEMP) ;length
  (lea (@ 4 FREE TEMP 2) TEMP)
  (cmp (@ 'memlimit) TEMP) ;will we have enough room?
  (ifnb
    (begin
      (call 'gc_sans_strings)
      (mov (@ SP) TEMP)
      (lea (@ 4 FREE TEMP 2) TEMP)
      (cmp (@ 'memlimit) TEMP) ;we ought to have enough room now.
      (jael 'error_out_of_memory)))
  (mov (@ SP) TEMP)
  (shl LENGTH_SHIFT TEMP)
  (add SCHEME_VECTOR TEMP)
  (mov FREE ARGL)
  (mov TEMP (@ ARGL))
  (add 4 ARGL)
  (pop TEMP)
  (rep)
  (stos)
  (test #b111 ARGL)
  (jz 'prim_make_vector_aligned)
  (clear VAL)
  (stos)
(: 'prim_make_vector_aligned)
  (mov FREE VAL)
  (mov ARGL FREE)
  (clear ARGL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "vector->list")
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) EXP)
  (insure-vector EXP TEMP)
  (jmpl 'vector_to_list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "vector")
  (mov ARGL VAL)
  (jmpl 'list_to_vector)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "list->vector")
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) VAL)
  (test VAL VAL)
  (jzl 'list_to_vector)
  (test 1 (@ VAL))
  (jzl 'list_to_vector)
  (jmpl 'error_expected_pair)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "vector-ref")
  (insure-more-args ARGL)
  (mov (@ ARGL) VAL) ;vector
  (insure-vector VAL EXP) ;EXP <- vector header
  (mov (@ 4 ARGL) ARGL)
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) TEMP) ;index
  (shr LENGTH_SHIFT EXP) ;vector length
  (cmp EXP TEMP)
  (jael 'error_invalid_index)
  (clear EXP)
  (mov (@ 4 VAL TEMP 2) VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "vector-set!")
  (insure-more-args ARGL)
  (mov (@ ARGL) VAL) ;vector
  (insure-vector VAL EXP) ;EXP <- vector header
  (mov (@ 4 ARGL) ARGL)
  (insure-more-args ARGL)
  (call 'get_exact_natural)
  (mov (@ 4 ARGL) ARGL)
  (insure-one-last-arg ARGL)
  (mov (@ 4 TEMP) TEMP) ;index
  (shr LENGTH_SHIFT EXP) ;vector length
  (cmp EXP TEMP)
  (jael 'error_invalid_index)
  (lea (@ 4 VAL TEMP 2) EXP)
  (mov (@ ARGL) VAL)
  (mov VAL (@ EXP))
  (clear EXP)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;:;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "vector-length")
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) TEMP) ;vector
  (insure-vector TEMP TEMP) ;TEMP <- vector header
  (shr LENGTH_SHIFT TEMP)
  (mov (object INTEGER TEMP) VAL)
  (jmpl 'advance_free)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "vector-fill!")
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP)
  (insure-vector EXP TEMP)
  (mov (@ 4 ARGL) ARGL)
  (insure-one-last-arg ARGL)
  (mov (@ ARGL) VAL)
  (shr LENGTH_SHIFT TEMP) ;TEMP <- vector length
  (test TEMP TEMP)
  (ifnz
    (begin
      (lea (@ 4 EXP) ARGL)
      (rep)
      (stos)
      (clear ARGL)))
  (ret)
