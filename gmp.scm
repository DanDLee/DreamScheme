;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: 'gmp_name)
  (asciz LIBGMP)
(: 'insure_gmp)
  (cmp 0 (@ 'gmp))
  (ifz
    (begin
      (if (not WINDOWS) (push 1))
      (push 'gmp_name)
      (cmp 0 (@ 'dlopen_rel))
      (jzl 'error_no_bignums)
      (calln (@ 'dlopen_rel))
      (if (not WINDOWS) (add 8 esp))
      (test eax eax)
      (jzl 'error_no_bignums)
      (mov eax (@ 'gmp))))
  (ret)

(: 'mpn_add_name)
  (asciz "__gmpn_add")
(: 'calln_mpn_add)
  (cmp 0 (@ 'mpn_add))
  (ifz
    (begin
      (call 'insure_gmp)
      (push 'mpn_add_name)
      (push (@ 'gmp))
      (calln (@ 'dlsym_rel))
      (if (not WINDOWS) (add 8 esp))
      (test eax eax)
      (jzl 'error_no_bignums)
      (mov eax (@ 'mpn_add))))
  (jmpn (@ 'mpn_add))

(: 'mpn_sub_name)
  (asciz "__gmpn_sub")
(: 'calln_mpn_sub)
  (cmp 0 (@ 'mpn_sub))
  (ifz
    (begin
      (call 'insure_gmp)
      (push 'mpn_sub_name)
      (push (@ 'gmp))
      (calln (@ 'dlsym_rel))
      (if (not WINDOWS) (add 8 esp))
      (test eax eax)
      (jzl 'error_no_bignums)
      (mov eax (@ 'mpn_sub))))
  (jmpn (@ 'mpn_sub))

(: 'mpn_mul_name)
  (asciz "__gmpn_mul")
(: 'calln_mpn_mul)
  (cmp 0 (@ 'mpn_mul))
  (ifz
    (begin
      (call 'insure_gmp)
      (push 'mpn_mul_name)
      (push (@ 'gmp))
      (calln (@ 'dlsym_rel))
      (if (not WINDOWS) (add 8 esp))
      (test eax eax)
      (jzl 'error_no_bignums)
      (mov eax (@ 'mpn_mul))))
  (jmpn (@ 'mpn_mul))

(: 'mpn_divrem_name)
  (asciz "__gmpn_divrem")
(: 'calln_mpn_divrem)
  (cmp 0 (@ 'mpn_divrem))
  (ifz
    (begin
      (call 'insure_gmp)
      (push 'mpn_divrem_name)
      (push (@ 'gmp))
      (calln (@ 'dlsym_rel))
      (if (not WINDOWS) (add 8 esp))
      (test eax eax)
      (jzl 'error_no_bignums)
      (mov eax (@ 'mpn_divrem))))
  (jmpn (@ 'mpn_divrem))

(: 'mpn_gcd_name)
  (asciz "__gmpn_gcd")
(: 'calln_mpn_gcd)
  (cmp 0 (@ 'mpn_gcd))
  (ifz
    (begin
      (call 'insure_gmp)
      (push 'mpn_gcd_name)
      (push (@ 'gmp))
      (calln (@ 'dlsym_rel))
      (if (not WINDOWS) (add 8 esp))
      (test eax eax)
      (jzl 'error_no_bignums)
      (mov eax (@ 'mpn_gcd))))
  (jmpn (@ 'mpn_gcd))
