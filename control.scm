;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; Control Procedures ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "apply")
  (insure-more-args ARGL)
  (mov (@ ARGL) VAL)
  (test VAL VAL)
  (jzl 'error_expected_procedure)
  (mov (@ 4 ARGL) ARGL)
  (insure-more-args ARGL)
  (mov ARGL UNEV)
  (cmp 0 (@ 4 ARGL))
  (jnz 'prim_apply_append)
  (mov (@ ARGL) UNEV)
  (test UNEV UNEV)
  (jzl 'prim_apply_call)
  (insure-object-is-pair UNEV)
(: 'prim_apply_call)
  (cmp MACRO_CLOSURE (@ VAL))
  (jnel 'apply_direct)
  (save ENV)
  (mov UNEV ARGL)
  (call 'apply_compound)
  (restore ENV)
  (mov VAL EXP)
  (jmpl 'eval_dispatch)

(: 'prim_apply_append_loop)
  (mov EXP ARGL)
(: 'prim_apply_append)
  (mov (@ 4 ARGL) EXP)
  (cmp 0 (@ 4 EXP))
  (jnz 'prim_apply_append_loop)
  (mov (@ EXP) EXP)
  (insure-list EXP)
  (mov EXP (@ 4 ARGL))
  (jmp 'prim_apply_call)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "for-each")
  (mov 'true TEMP)
  (jmp 'prim_map_begin)
(new-primitive "map")
  (mov 0 TEMP)
(: 'prim_map_begin)
  (insure-more-args ARGL)
  (mov (@ ARGL) EXP) ;proc
  (test EXP EXP)
  (jzl 'error_expected_procedure)
  (save TEMP)
  (mov (@ 4 ARGL) ARGL)
  (insure-more-args ARGL)
(: 'prim_map_list_loop)
  (save EXP)
  (clear VAL UNEV) ;VAL will be cars, UNEV will be cdrs
(: 'prim_map_loop)
  (mov (@ ARGL) EXP) ;current list
  (test EXP EXP)
  (ifnzl
    (begin
      (mov (@ EXP) TEMP)
      (test 1 TEMP)
      (jnzl 'error_expected_pair)
      (mov (object TEMP UNEV) UNEV)
      (call 'advance_free)
      (mov (@ 4 EXP) TEMP)
      (mov (object TEMP VAL) VAL)
      (call 'advance_free)
      (mov (@ 4 ARGL) ARGL) ;next list
      (test ARGL ARGL)
      (jnz 'prim_map_loop)
      (mov VAL EXP) ;so it will not get clobbered
      (mov UNEV ARGL)
      (call 'reverse)
      (mov VAL UNEV) ;current args
      (mov EXP ARGL)
      (call 'reverse) ;VAL is now next list of lists
      (mov (@ 'root) EXP)
      (mov (@ EXP) EXP) ;peek at proc
      (save VAL)
      (mov EXP VAL)
      (save ENV)
      (call 'prim_apply_call)
      (restore ENV)
      (restore ARGL EXP UNEV)
      (cmp 'true UNEV)
      (ifne
        (begin
          (mov (object VAL UNEV) UNEV) ;(cons VAL UNEV)
          (call 'advance_free)))
      (save UNEV)
      (jmpl 'prim_map_list_loop)))
  (mov (@ 'root) TEMP)
  (mov (@ 4 TEMP) TEMP)
  (mov TEMP (@ 'root))
  (restore ARGL)
  (cmp 'true ARGL) ;was this a for-each?
  (jnel 'reverse)
  (mov ARGL VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "call-with-current-continuation")
  (insure-one-last-arg ARGL)
  (clear UNEV)
  (mov SP TEMP)
(: 'prim_cc_loop)
  (cmp (@ 'stackbase) TEMP)
  (ifne
    (begin
      (push TEMP)
      (mov (@ TEMP) TEMP)
      (mov (object INTEGER TEMP) VAL)
      (add 8 FREE)
      (mov (object VAL UNEV) UNEV)
      (call 'advance_free)
      (pop TEMP)
      (add 4 TEMP)
      (jmp 'prim_cc_loop)))
  (mov (@ ENV) TEMP)
  (mov TEMP (@ FREE))
  (mov (@ 4 ENV) TEMP)
  (mov TEMP (@ 4 FREE))
  (mov FREE TEMP)
  (add 8 FREE)
  (mov (@ 'root) VAL)
  (mov (object TEMP VAL) VAL)
  (call 'advance_free)
  (mov (object VAL UNEV) VAL) ;((ENV . root) . stack)
  (add 8 FREE)
  (mov (object CONTINUATION VAL) UNEV)
  (call 'advance_free)
  (mov (object UNEV 0) UNEV)
  (call 'advance_free)
  (mov (@ ARGL) VAL)
  (test VAL VAL)
  (jzl 'error_expected_procedure)
  (jmpl 'apply_procedure)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "interaction-environment")
  (insure-no-more-args ARGL)
  (mov (@ 'interaction_environment) VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "scheme-report-environment")
  (push 'first_additional_primitive)
  (jmp 'construct_environment)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "null-environment")
  (push 'first_primitive)
  (jmp 'construct_environment)
(: 'construct_environment)
  (call 'get_last_exact_natural)
  (mov (@ 4 TEMP) TEMP)
  (cmp 4 TEMP)
  (jnel 'error_unsupported_environment)
  (save ENV)
  (mov TOPLEVEL_SIZE TEMP) ;length
  (clear VAL) ;element
  (call 'make_vector)
  (mov (object VAL 0) VAL)
  (call 'advance_free)
  (mov VAL ENV)
  (save ENV) ;null-environment
  (mov 'builtins TEMP)
 (: 'construct_environment_loop)
  (mov (@ TEMP) UNEV)
  (push TEMP)
  (mov UNEV EXP)
  (mov (@ 'interaction_environment) ENV)
  (call 'ev_variable)
  (mov VAL EXP)
  (peek ENV)
  (call 'define_define)
  (pop TEMP)
  (add 12 TEMP)
  (cmp (@ esp) TEMP)
  (jne 'construct_environment_loop)
  (restore VAL)
  (restore ENV)
  (add 4 esp)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-primitive "current-environment")
  (insure-no-more-args ARGL)
  (mov ENV VAL)
  (ret)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(new-additional-primitive "dictionary-space")
  (insure-no-more-args ARGL)
  (mov (@ 'freedictend) TEMP)
  (sub (@ 'freedict) TEMP)
  (mov (object INTEGER TEMP) VAL)
  (jmpl 'advance_free)
