;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This is an x86 assembler implemented in Scheme.                         ;;;
;;; Not all x86 instructions are yet implemented,                           ;;;
;;; But enough are implemented to compile the Dream Scheme Interpreter. :-) ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x86-assembler-environment (scheme-report-environment 4))
(for-each (lambda (x) (eval x x86-assembler-environment))
  `((define & ,&) (define ~ ,~) (define error ,error)))
(define (x86-set-text-start! addr)
  (eval `(set! x86-text-start ,addr) x86-assembler-environment))
(define x86-assemble
  (lambda (program)
    (eval '(x86-first-pass) x86-assembler-environment)
    (eval program x86-assembler-environment)
    (eval '(x86-second-pass) x86-assembler-environment)
    (eval program x86-assembler-environment)
    (eval '(x86-finalize) x86-assembler-environment)))
(for-each (lambda (x) (eval x x86-assembler-environment))
'((define x86-text '())
  (define (x86-finalize)
    (let ((code (list->string (reverse x86-text))))
      (set! x86-text '())
      code))
  (define (x86-first-pass)
    (x86-rewind)
    (set! x86-symbols '())
    (set! x86-write-byte (lambda (n) #t))
    (set! ascii
      (lambda (s)
        (set! x86-address (+ x86-address (string-length s)))))
    (set! x86-default-address (lambda (x) x86-address)))
  (define (x86-second-pass)
    (x86-insure-bss)
    (set! x86-bss-end x86-address)
    (x86-rewind)
    (set! x86-write-byte
      (lambda (n)
        (set! x86-text
          (cons
            (if (char? n) n (integer->char (modulo n #x100)))
            x86-text))))
    (set! ascii
      (lambda (s)
        (set! x86-text (append (reverse (string->list s)) x86-text))
        (set! x86-address (+ x86-address (string-length s)))))
    (set! x86-default-address
      (lambda (x) (write x) (x86-error 'label-undefined))))
  (define x86-text-start #x08048000) ;use #x400000 on Windows
  (define x86-data-start 0)
  (define x86-bss-start 0)
  (define x86-bss-end 0)
  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;Assembly Registers;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;
  (define r0 (lambda () 0))
  (define r1 (lambda () 1))
  (define r2 (lambda () 2))
  (define r3 (lambda () 3))
  (define r4 (lambda () 4))
  (define r5 (lambda () 5))
  (define r6 (lambda () 6))
  (define r7 (lambda () 7))
  (define cr0 (lambda () 0))
  (define cr1 (lambda () 1))
  (define es (lambda () 0))
  (define cs (lambda () 1))
  (define ss (lambda () 2))
  (define ds (lambda () 3))
  (define fs (lambda () 4))
  (define gs (lambda () 5))
  (define eax r0)
  (define ax r0)
  (define al r0)
  (define ecx r1)
  (define cx r1)
  (define cl r1)
  (define edx r2)
  (define dx r2)
  (define dl r2)
  (define ebx r3)
  (define bx r3)
  (define bl r3)
  (define esp r4)
  (define sp r4)
  (define ah r4)
  (define ebp r5)
  (define bp r5)
  (define ch r5)
  (define esi r6)
  (define si r6)
  (define dh r6)
  (define edi r7)
  (define di r7)
  (define bh r7)
  
  ;;;;;;;;;;;;;;;;;;;;;;
  ;;;One-byte Opcodes;;;
  ;;;;;;;;;;;;;;;;;;;;;;
  (define (opd-size)
    (x86-byte #x66)
    (set! x86-opd-size? #t))
  (define (addr-size)
    (x86-byte #x67)
    (set! x86-addr-size? #t))
  
  (define (pusha) (op #x60))
  (define (popa) (op #x61))
  (define (pushf) (op #x9c))
  (define (popf) (op #x9d))
  (define (nop) (op #x90))
  (define (movsb) (op #xa4))
  (define (movs) (op #xa5))
  (define (cmpsb) (op #xa6))
  (define (cmps) (op #xa7))
  (define (stosb) (op #xaa))
  (define (stos) (op #xab))
  (define (lodsb) (op #xac))
  (define (lods) (op #xad))
  (define (cdq) (op #x99))
  (define (ret) (op #xc3))
  (define (iret) (op #xcf))
  (define (repne) (op #xf2))
  (define (rep) (op #xf3))
  (define repe rep)
  (define (clc) (op #xf8))
  (define (stc) (op #xf9))
  (define (cli) (op #xfa))
  (define (sti) (op #xfb))
  (define (cld) (op #xfc))
  (define (std) (op #xfd))
  
  (define (inb x)
    (cond
      ((eq? x dx)
       (op #xec))
      (else
       (op #xe4)
       (x86-byte x))))
  (define (in x)
    (cond
      ((eq? x dx)
       (op #xed))
      (else
       (op #xe5)
       (x86-byte x))))
  (define (outb x)
    (cond
      ((eq? x dx)
       (op #xee))
      (else
       (op #xe6)
       (x86-byte x))))
  (define (out x)
    (cond
      ((eq? x dx)
       (op #xef))
      (else
       (op #xe7)
       (x86-byte x))))
  
  (define (int n) (op #xcd) (x86-byte n))
  
  (define (seg= x)
    (cond
      ((eq? x fs) (op #x64))
      ((eq? x gs) (op #x65))
      ((x86-segment-register? x) (op (+ #x26 (* 8 (x)))))
      (else (x86-error 'segment-reg))))
  (define (pop x)
    (cond
      ((x86-segment-register? x)
       (cond
         ((eq? x cs)
          (x86-error 'pop-cs))
         ((eq? x fs) (op2 #xa1))
         ((eq? x gs) (op2 #xa9))
         (else (op (+ #x07 (* 8 (x)))))))
      ((x86-register? x)
       (op (+ #x58 (x))))
      ((pair? x)
       (op #x8f)
       (apply x86-r/m (cons r0 x)))
      (else
        (x86-error 'pop-constant))))
  (define (xchg r)
    (x86-assert-reg r)
    (op (+ #x90 (r))))
  
  (define (pushb x)
    (op #x6a)
    (x86-byte x))
  
  (define (lea a b)
    (if (pair? a)
      (begin (op #x8d) (apply x86-r/m (cons b a)))
      (x86-error 'lea-first-arg)))
  
  ;;;;;;;;;;;;;
  ;;;Group 1;;;
  ;;;;;;;;;;;;;
  (define (addb a b) (x86-group1b a b #x00 r0))
  (define (add a b) (x86-group1tb a b #x01 r0))
  
  (define (orb a b) (x86-group1b a b #x08 r1))
  (define (or! a b) (x86-group1tb a b #x09 r1))
  
  (define (adcb a b) (x86-group1b a b #x10 r2))
  (define (adc a b) (x86-group1tb a b #x11 r2))
  
  (define (sbbb a b) (x86-group1b a b #x18 r3))
  (define (sbb a b) (x86-group1tb a b #x19 r3))
  
  (define (andb a b) (x86-group1b a b #x20 r4))
  (define (and! a b) (x86-group1t a b #x21 r4))
  
  (define (subb a b) (x86-group1b a b #x28 r5))
  (define (sub a b) (x86-group1tb a b #x29 r5))
  
  (define (xorb a b) (x86-group1b a b #x30 r6))
  (define (xor a b) (x86-group1tb a b #x31 r6))
  
  (define (cmpb a b) (x86-group1b a b #x38 r7))
  (define (cmp a b) (x86-group1tb a b #x39 r7))
  
  ;;;;;;;;;;;;;
  ;;;Group 2;;;
  ;;;;;;;;;;;;;
  (define (rolb a b) (x86-group2 a b r0 0))
  (define (rol a b) (x86-group2 a b r0 1))
  (define (rorb a b) (x86-group2 a b r1 0))
  (define (ror a b) (x86-group2 a b r1 1))
  (define (rclb a b) (x86-group2 a b r2 0))
  (define (rcl a b) (x86-group2 a b r2 1))
  (define (rcrb a b) (x86-group2 a b r3 0))
  (define (rcr a b) (x86-group2 a b r3 1))
  (define (shlb a b) (x86-group2 a b r4 0))
  (define salb shlb)
  (define (shl a b) (x86-group2 a b r4 1))
  (define sal shl)
  (define (shrb a b) (x86-group2 a b r5 0))
  (define (shr a b) (x86-group2 a b r5 1))
  (define (sarb a b) (x86-group2 a b r7 0))
  (define (sar a b) (x86-group2 a b r7 1))
  
  ;;;;;;;;;;;;;
  ;;;Group 3;;;
  ;;;;;;;;;;;;;
  (define (test a b)
    (x86-op-r/m a b #x85 #x85
      (lambda ()
        (op #xf7)
        (x86-r/m-eg r0 b)
        (word a))))
  
  (define (testb a b)
    (x86-op-r/m a b #x84 #x84
      (lambda ()
        (op #xf6)
        (x86-r/m-eg r0 b)
        (x86-byte a))))
  
  (define (notb x) (op #xf6) (x86-r/m-eg r2 x))
  (define (not! x) (op #xf7) (x86-r/m-eg r2 x))
  (define (negb x) (op #xf6) (x86-r/m-eg r3 x))
  (define (neg x) (op #xf7) (x86-r/m-eg r3 x))
  (define (mulb x) (op #xf6) (x86-r/m-eg r4 x))
  (define (mul x) (op #xf7) (x86-r/m-eg r4 x))
  (define (imulb x) (op #xf6) (x86-r/m-eg r5 x))
  (define (imul x) (op #xf7) (x86-r/m-eg r5 x))
  (define (divb x) (op #xf6) (x86-r/m-eg r6 x))
  (define (div x) (op #xf7) (x86-r/m-eg r6 x))
  (define (idivb x) (op #xf6) (x86-r/m-eg r7 x))
  (define (idiv x) (op #xf7) (x86-r/m-eg r7 x))
  
  ;;;;;;;;;;;;;
  ;;;Group 4;;;
  ;;;;;;;;;;;;;
  (define (incb x) (op #xfe) (x86-r/m-eg r0 x))
  (define (decb x) (op #xfe) (x86-r/m-eg r1 x))
  
  ;;;;;;;;;;;;;
  ;;;Group 5;;;
  ;;;;;;;;;;;;;
  (define (inc x)
    (cond
      ((x86-register? x)
       (op (+ #x40 (x))))
      ((pair? x)
       (op #xff)
       (apply x86-r/m (cons r0 x)))
      (else
        (x86-error 'inc-constant))))
  (define (dec x)
    (cond
      ((x86-register? x)
       (op (+ #x48 (x))))
      ((pair? x)
       (op #xff)
       (apply x86-r/m (cons r1 x)))
      (else
        (x86-error 'dec-constant))))
  (define (calln x)
    (if (pair? x)
      (begin
        (op #xff)
        (apply x86-r/m (cons r2 x)))
      (x86-error 'calln-arg)))
  (define (callf x)
    (if (pair? x)
      (begin
        (op #xff)
        (apply x86-r/m (cons r3 x)))
      (x86-error 'callf-arg)))
  (define (jmpn x)
    (if (pair? x)
      (begin
        (op #xff)
        (apply x86-r/m (cons r4 x)))
      (x86-error 'jmpn-arg)))
  (define (jmpf x)
    (if (pair? x)
      (begin
        (op #xff)
        (apply x86-r/m (cons r5 x)))
      (x86-error 'jmpf-arg)))
  (define (push x)
    (cond
      ((x86-segment-register? x)
       (cond
         ((eq? x fs) (op2 #xa0))
         ((eq? x gs) (op2 #xa8))
         (else (op (+ #x06 (* 8 (x)))))))
      ((x86-register? x)
       (op (+ #x50 (x))))
      ((pair? x)
       (op #xff)
       (apply x86-r/m (cons r6 x)))
      (else
        (op #x68)
        (word x))))
  
  ;;;;;;;;;;;;;;
  ;;;Group 11;;;
  ;;;;;;;;;;;;;;
  (define (mov a b)
    (cond
      ((and (eq? a r0) (x86-direct-address? b))
       (op #xa3)
       (word-address (car b)))
      ((and (eq? b r0) (x86-direct-address? a))
       (op #xa1)
       (word-address (car a)))
      ((x86-segment-register? a)
       (op #x8c)
       (x86-r/m-eg a b))
      ((x86-segment-register? b)
       (op #x8e)
       (x86-r/m-eg b a))
      ((x86-control-register? a)
       (op2 #x20)
       (x86-mod-r/m 3 a b))
      ((x86-control-register? b)
       (op2 #x22)
       (x86-mod-r/m 3 b a))
      (else
        (x86-op-r/m a b #x89 #x8b
          (lambda ()
            (cond
              ((x86-register? b)
               (op (+ #xb8 (b)))
               (word a))
              ((pair? b)
               (op #xc7)
               (apply x86-r/m (cons r0 b))
               (word a))
              (else (x86-error 'mov-args))))))))
         
  (define (movb a b)
    (x86-op-r/m a b #x88 #x8a
      (lambda ()
        (cond
          ((x86-register? b)
           (op (+ #xb0 (b)))
           (x86-byte a))
          ((pair? b)
           (op #xc6)
           (apply x86-r/m (cons r0 b))
           (x86-byte a))
          (else (x86-error 'movb-args))))))
  
  ;;;;;;;;;;;
  ;;;Jumps;;;
  ;;;;;;;;;;;
  (define (jo d) (op #x70) (rel-byte d))
  (define (jno d) (op #x71) (rel-byte d))
  (define (jb d) (op #x72) (rel-byte d))
  (define jnae jb)
  (define jc jb)
  (define (jnb d) (op #x73) (rel-byte d))
  (define jae jnb)
  (define jnc jnb)
  (define (jz d) (op #x74) (rel-byte d))
  (define je jz)
  (define (jnz d) (op #x75) (rel-byte d))
  (define jne jnz)
  (define (jbe d) (op #x76) (rel-byte d))
  (define jna jbe)
  (define (jnbe d) (op #x77) (rel-byte d))
  (define ja jnbe)
  (define (js d) (op #x78) (rel-byte d))
  (define (jns d) (op #x79) (rel-byte d))
  (define (jp d) (op #x7a) (rel-byte d))
  (define jpe jp)
  (define (jnp d) (op #x7b) (rel-byte d))
  (define jpo jnp)
  (define (jl d) (op #x7c) (rel-byte d))
  (define jnge jl)
  (define (jnl d) (op #x7d) (rel-byte d))
  (define jge jnl)
  (define (jle d) (op #x7e) (rel-byte d))
  (define jng jle)
  (define (jnle d) (op #x7f) (rel-byte d))
  (define jg jnle)
  
  (define (jcxz d) (op #xe3) (rel-byte d))
  (define jecxz jcxz)
  
  (define (loopne d) (op #xe0) (rel-byte d))
  (define loopnz loopne)
  
  (define (loope d) (op #xe1) (rel-byte d))
  (define loopz loope)
  
  (define (loop d) (op #xe2) (rel-byte d))
  
  (define (call a) (op #xe8) (rel-word a))
  (define (jmp d) (op #xeb) (rel-byte d))
  (define (far-jmp s a)
    (op #xea)
    (word a)
    (x86-wyde s))
  (define (jmpl d) (op #xe9) (rel-word d))
  
  ;;;;;;;;;;;;;;;;;;;;;;
  ;;;Two-byte Opcodes;;;
  ;;;;;;;;;;;;;;;;;;;;;;
  (define (movzb a b)
    (if (x86-register? b)
      (begin
        (op2 #xb6)
        (x86-r/m-eg b a))
      (x86-error 'movzb-destination)))
  
  (define (jol d) (op2 #x80) (rel-word d))
  (define (jnol d) (op2 #x81) (rel-word d))
  (define (jbl d) (op2 #x82) (rel-word d))
  (define jnael jbl)
  (define jcl jbl)
  (define (jnbl d) (op2 #x83) (rel-word d))
  (define jael jnbl)
  (define jncl jnbl)
  (define (jzl d) (op2 #x84) (rel-word d))
  (define jel jzl)
  (define (jnzl d) (op2 #x85) (rel-word d))
  (define jnel jnzl)
  (define (jbel d) (op2 #x86) (rel-word d))
  (define jnal jbel)
  (define (jnbel d) (op2 #x87) (rel-word d))
  (define jal jnbel)
  (define (jsl d) (op2 #x88) (rel-word d))
  (define (jnsl d) (op2 #x89) (rel-word d))
  (define (jpl d) (op2 #x8a) (rel-word d))
  (define jpel jpl)
  (define (jnpl d) (op2 #x8b) (rel-word d))
  (define jpol jnpl)
  (define (jll d) (op2 #x8c) (rel-word d))
  (define jngel jll)
  (define (jnll d) (op2 #x8d) (rel-word d))
  (define jgel jnll)
  (define (jlel d) (op2 #x8e) (rel-word d))
  (define jngl jlel)
  (define (jnlel d) (op2 #x8f) (rel-word d))
  (define jgl jnlel)
  
  (define (rdtsc) (op2 #x31))
  
  ;;;;;;;;;;;;;
  ;;;Group 7;;;
  ;;;;;;;;;;;;;
  (define (lgdt a)
    (op2 #x01)
    (x86-mod-r/m 0 r2 r6)
    (word a))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;Pseudo Instructions;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (protected-mode)
    (set! x86-protected-mode #t))
  (define (real-mode)
    (set! x86-protected-mode #f))
  (define (ascii s) s)
  (define (data)
    (set! x86-data-start x86-address))
  (define (bss s n)
    (x86-insure-bss)
    (: s)
    (x86-bss n))
  (define (lookup s)
    (let ((n (assq s x86-symbols)))
      (if n (cdr n) (x86-default-address s))))
  (define (file-offset s)
    (if (number? s)
      (- s x86-text-start)
      (- (lookup s) x86-text-start)))
  (define (: s)
    (let ((n (assq s x86-symbols)))
      (if n
        (cond ((not (= (cdr n) x86-address))
               (write s)
               (x86-error 'label-multiple)))
        (set! x86-symbols (cons (cons s x86-address) x86-symbols)))))
  (define (symbol-seq)
    (set! x86-sequence (+ x86-sequence 1))
    (string->symbol (number->string x86-sequence)))
  (define (byte n)
    (x86-byte n))
  (define (bytes . x)
    (if (pair? x)
      (begin
        (x86-byte (car x))
        (apply bytes (cdr x)))))
  (define (wyde n)
    (x86-wyde n))
  (define (wydes . x)
    (if (pair? x)
      (begin
        (x86-wyde (car x))
        (apply wydes (cdr x)))))
  (define (word n)
    (if (x86-opd-16)
      (x86-wyde n)
      (x86-tetra n)))
  (define (word-address n)
    (if (x86-addr-16)
      (x86-wyde n)
      (x86-tetra n)))
  (define (tetra n)
    (x86-tetra n))
  (define (tetras . x)
    (if (pair? x)
      (begin
        (x86-tetra (car x))
        (apply tetras (cdr x)))))
  (define (align n)
    (if (positive? (remainder x86-address n))
      (begin
        (if x86-bss?
          (x86-bss 1)
          (x86-byte 0))
        (align n))))
  (define (asciis . x)
    (if (pair? x)
      (begin
        (ascii (car x))
        (apply asciis (cdr x)))))
  (define (asciz s)
    (ascii s)
    (x86-byte 0))
  
  (define (@ d . x)
    (if (x86-register? d)
      (begin
        (set! x (cons d x))
        (set! d 0)))
    (if (pair? x)
      (if (pair? (cdr x))
        (list d (car x)
                (cadr x)
                (if (pair? (cddr x))
                  (caddr x)
                  0))
        (list d (car x) #f 0))
      (list d #f #f 0)))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Compiler Internals ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;
   
  ;;;;;;;;;;;;;;
  ;;;Messages;;;
  ;;;;;;;;;;;;;;
  (define (x86-message msg)
    (cdr
      (assq msg x86-messages)))
  (define (x86-error msg)
    (error (x86-message msg)))
  (define x86-messages
    '((first-pass . "\n-------First Pass-------\n")
      (second-pass . "\n-------Second Pass-------\n")
      (bss-bytes . "No bytes can be written in the 'bss' section.")
      (label-undefined . ": Label is not defined.")
      (label-multiple . ": Label exists for multiple distinct addresses.")
      (byte-overflow . "Number is too wide for one byte.")
      (wyde-overflow . "Number is too wide for a 16-bit word.")
      (invalid-register . ": Invalid register")
      (invalid-r/m-prefix . "Invalid mod-r/m prefix.")
      (ebp-with-index . "Cannot use ebp for address when using an index register.")
      (reg-with-word-displacement . "Cannot use register for address with word displacement.")
      (real-shift-index . "Cannot shift index in real mode.")
      (real-mem-with-index . "Only BX and BP may be used as memory pointers with an index in real mode.")
      (real-index . "Only SI and DI may be used as index registers in real mode.")
      (real-mem . "Only SI, DI, BP, or BX may be used as memory pointers in real mode.")
      (second-operand . "Expected register or address as second operand.")
      (segment-reg . "Expected segment register for (seg= x).")
      (pop-cs . "Cannot pop CS register.")
      (pop-constant . "Popping into a constant?  Surely not.")
      (lea-first-arg . "Expected address as first argument to (lea a b)")
      (shift-reg . "Among registers, only CL may be used as a shift index.")
      (shift-constant . "Among integers, only an unsigned byte may be used as a shift index.")
      (shift . "Only byte integers or register cl may be used as a shift index.")
      (inc-constant . "Incrementing a constant?  Surely not.")
      (dec-constant . "Decrementing a constant?  Surely not.")
      (calln-arg . "Expected address as argument to (calln x)")
      (callf-arg . "Expected address as argument to (callf x)")
      (jmpn-arg . "Expected address as argument to (jmpn x)")
      (jmpf-arg . "Expected address as argument to (jmpf x)")
      (mov-args . "Invalid arguments for (mov a b)")
      (movb-args . "Invalid arguments for (movb a b)")
      (byte-branch . ": Branch is too far for one byte branch.")
      (movzb-destination . "The movzb instruction must have a register as its destination.")))
  
  ;;;;;;;;;;;;;;;;
  ;;;Validation;;;
  ;;;;;;;;;;;;;;;;
  (define (x86-bss-error n)
    (x86-error 'bss-bytes))
  (define (x86-large? n)
    (if (positive? n)
      (> n #xff)
      (< n #x-80)))
  (define (x86-large-unsigned? n)
    (or (negative? n) (> n #xff)))
  (define (x86-large-signed? n)
    (if (positive? n)
      (> n #x7f)
      (< n #x-80)))
  (define (x86-too-wyde? n)
    (if (positive? n)
      (> n #xffff)
      (< n #x-8000)))
  
  ;;;;;;;;;;;;;;;;;
  ;;;Compilation;;;
  ;;;;;;;;;;;;;;;;;
  (define x86-address x86-text-start)
  (define x86-bss? #f)
  (define x86-sequence 0)
  (define x86-symbols '())
  (define (x86-default-address x) x86-address)
  (define (x86-write-byte n)
    (if (char? n) (set! n (char->integer n)))
    (if (negative? n) (set! n (+ #x100 n)))
    (display "#x")
    (display (number->string n 16))
    (newline))
  (define (x86-rewind)
    (set! x86-text '())
    (set! x86-address x86-text-start)
    (set! x86-bss? #f)
    (set! x86-sequence 0))
  (define (x86-inc-addr)
    (set! x86-address (+ 1 x86-address)))
  (define x86-protected-mode #t)
  (define (x86-insure-bss)
    (if (not x86-bss?)
      (begin
        (if (zero? x86-data-start)
          (data))
        (set! x86-bss-start x86-address)
        (set! x86-bss? #t)
        (set! x86-write-byte x86-bss-error)
        (set! ascii x86-bss-error))))
  (define (x86-bss n)
    (set! x86-address (+ n x86-address)))
  
  (define x86-opd-size? #f)
  (define x86-addr-size? #f)
  (define x86-opd-size-reset? #f)
  (define x86-addr-size-reset? #f)
  (define (x86-byte n)
    (if (and (number? n) (x86-large? n))
      (x86-error 'byte-overflow))
    (x86-write-byte n)
    (x86-inc-addr))
  (define (x86-wyde n)
    (cond
      ((symbol? n)
       (set! n (lookup n)))
      ((char? n)
       (set! n (char->integer n)))
      ((negative? n)
       (set! n (+ n #x10000))))
    (x86-byte (& n #xff))
    (x86-byte (quotient (& n #xff00) #x100))
    (if (x86-too-wyde? n)
      (x86-error 'wyde-overflow)))
  (define (x86-opd-16)
    (eq? x86-protected-mode x86-opd-size?))
  (define (x86-addr-16)
    (eq? x86-protected-mode x86-addr-size?))
  (define (x86-tetra n)
    (cond
      ((symbol? n)
       (set! n (lookup n)))
      ((char? n)
       (set! n (char->integer n)))
      ((negative? n)
       (set! n (+ n #xffffffff 1))))
    (x86-byte (& n #xff))
    (x86-byte (quotient (& n #xff00) #x100))
    (x86-byte (quotient (& n #xff0000) #x10000))
    (x86-byte (quotient (& n #xff000000) #x1000000)))
  
  (define x86-register? procedure?)
  (define (x86-segment-register? x)
    (memq x (list es cs ss ds fs gs)))
  (define (x86-control-register? x)
    (memq x (list cr0 cr1)))
  (define (x86-assert-reg r)
    (cond
      ((not (x86-register? r))
       (write r)
       (x86-error 'invalid-register))))
  (define (op n)
    (x86-byte n)
    (if x86-addr-size?
      (if x86-addr-size-reset?
        (begin
          (set! x86-addr-size-reset? #f)
          (set! x86-addr-size? #f))
        (set! x86-addr-size-reset? #t)))
    (if x86-opd-size?
      (if x86-opd-size-reset?
        (begin
          (set! x86-opd-size-reset? #f)
          (set! x86-opd-size? #f))
        (set! x86-opd-size-reset? #t))))
  (define (op2 n)
    (op #x0f)
    (x86-byte n))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;Mod-R/M byte encodings;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (define (x86-mod-r/m p r m)
    (if (not m) (set! m r4))
    (x86-assert-reg r)
    (x86-assert-reg m)
    (if (and (>= p 0) (<= p 3))
      (x86-byte
        (+ (* p 64)
           (* (r) 8)
           (m)))
      (x86-error 'invalid-r/m-prefix)))
  (define (x86-r/m2 i m d s)
    (if (and m (eq? m r5))
      (x86-error 'ebp-with-index)
      (if (zero? d)
        (x86-mod-r/m s i m)
        (if m
          (x86-error 'reg-with-word-displacement)
          (begin
            (x86-mod-r/m s i r5)
            (word-address d))))))
  (define (x86-r/m r d m i s)
    (if (symbol? d) (set! d (lookup d)))
    (if x86-protected-mode
      (x86-r/m-32 r d m i s)
      (if (positive? s)
        (x86-error 'real-shift-index)
        (x86-r/m-16 r d m i))))
  (define (x86-mod-r/m-16 r d m)
    (cond
      ((zero? d)
       (if (eq? m r6)
         (begin
           (x86-r/m 1 r m)
           (x86-byte 0))
         (x86-mod-r/m 0 r m)))
      ((x86-large? d)
       (x86-mod-r/m 2 r m)
       (x86-wyde d))
      (else
       (x86-mod-r/m 1 r m)
       (x86-byte d))))
  (define (x86-r/m-16 r d m i)
    (if i
      (cond
        ((eq? i r6)
         (cond
           ((eq? m r3)
            (x86-mod-r/m-16 r d r0))
           ((eq? m r5)
            (x86-mod-r/m-16 r d r2))
           (else (x86-error 'real-mem-with-index))))
        ((eq? i r7)
         (cond
           ((eq? m r3)
            (x86-mod-r/m-16 r d r1))
           ((eq? m r5)
            (x86-mod-r/m-16 r d r3))
           (else (x86-error 'real-mem-with-index))))
        (else (x86-error 'real-index)))
      (if m
        (cond
          ((eq? m r6)
           (x86-mod-r/m-16 r d r4))
          ((eq? m r7)
           (x86-mod-r/m-16 r d r5))
          ((eq? m r5)
           (x86-mod-r/m-16 r d r6))
          ((eq? m r3)
           (x86-mod-r/m-16 r d r7))
          (else (x86-error 'real-mem)))
        (begin
          (x86-mod-r/m 0 r r6)
          (word d)))))
  (define (x86-r/m-32 r d m i s)
    (if i
      (if (or (zero? d) (x86-large? d))
        (begin
          (x86-mod-r/m 0 r #f)
          (x86-r/m2 i m d s))
        (begin
          (x86-mod-r/m 1 r #f)
          (x86-r/m2 i m 0 s)
          (x86-byte d)))
      (if m
        (if (or (x86-large? d) (eq? m r4))
          (x86-r/m r d #f m 0)
          (if (and (zero? d)
                   (not (eq? m r5)))
            (x86-mod-r/m 0 r m)
            (begin
              (x86-mod-r/m 1 r m)
              (x86-byte d))))
        (begin
          (x86-mod-r/m 0 r r5)
          (word-address d)))))
  (define (x86-r/m-eg a b)
    (cond
      ((x86-register? b)
       (x86-mod-r/m 3 a b))
      ((pair? b)
        (apply x86-r/m (cons a b)))
      (else (x86-error 'second-operand))))
  (define (x86-op-r/m a b EG GE immed)
    (cond
      ((x86-register? a)
       (op EG)
       (x86-r/m-eg a b))
      ((pair? a)
       (op GE)
       (apply x86-r/m (cons b a)))
      (else (immed))))
  (define (x86-group1 a b EG ext bop top)
    (x86-op-r/m a b EG (+ EG 2)
      (lambda ()
        (if (symbol? a) (set! a (lookup a)))
        (if (eq? b r0)
          (begin
            (op (+ EG 4))
            (if top (word a) (x86-byte a)))
          (begin
            (if (char? a)
              (set! a (char->integer a)))
            (if (or (not bop) (x86-large? a))
              (if top
                (begin
                  (op top)
                  (x86-r/m-eg ext b)
                  (word a))
                (x86-error 'byte-overflow))
              (begin
                (op bop)
                (x86-r/m-eg ext b)
                (x86-byte a))))))))
  
  (define (x86-group1b a b EG ext) (x86-group1 a b EG ext #x80 #f))
  (define (x86-group1tb a b EG ext) (x86-group1 a b EG ext #x83 #x81))
  (define (x86-group1t a b EG ext) (x86-group1 a b EG ext #f #x81))
  
  (define (x86-group2 a b ext size) ;size: 0=byte, 1=word
    (cond
      ((x86-register? a)
       (if (eq? a cl)
         (begin
           (op (+ #xd2 size))
           (x86-r/m-eg ext b))
         (x86-error 'shift-reg)))
      ((integer? a)
       (if (x86-large-unsigned? a)
         (x86-error 'shift-constant)
         (if (= 1 a)
           (begin
             (op (+ #xd0 size))
             (x86-r/m-eg ext b))
           (begin
             (op (+ #xc0 size))
             (x86-r/m-eg ext b)
             (x86-byte a)))))
      (else (x86-error 'shift))))
  
  (define (x86-direct-address? x)
    (and (pair? x)
         (not (cadr x))
         (not (caddr x))
         (zero? (cadddr x))))
  
  (define (rel-byte dest)
    (let* ((da (if (symbol? dest) (lookup dest) dest))
           (vec (- da x86-address 1)))
      (if (x86-large-signed? vec)
        (begin
          (if (symbol? dest)
            (write dest))
          (x86-error 'byte-branch)))
      (x86-byte vec)))
  (define (rel-word dest)
    (if (symbol? dest)
      (set! dest (lookup dest)))
    (word (- dest x86-address (if (x86-opd-16) 2 4))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;Convenience Macros;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;
  (define (make-if jmp-else jmp-then)
    (macro (a . b)
      (if (null? b)
        `(let ((then (symbol-seq)))
           (,jmp-else then)
           ,a
           (: then))
        (if (null? (cdr b))
          `(let ((else (symbol-seq))
                 (then (symbol-seq)))
             (,jmp-else else)
             ,a
             (,jmp-then then)
             (: else)
             ,(car b)
             (: then))
          (error "Too many arguments")))))
  (define ifo (make-if jno jmp))
  (define ifno (make-if jo jmp))
  (define ifb (make-if jnb jmp))
  (define ifnae ifb)
  (define ifc ifb)
  (define ifnb (make-if jb jmp))
  (define ifae ifnb)
  (define ifnc ifnb)
  (define ifz (make-if jnz jmp))
  (define ife ifz)
  (define ifnz (make-if jz jmp))
  (define ifne ifnz)
  (define ifbe (make-if jnbe jmp))
  (define ifna ifbe)
  (define ifnbe (make-if jbe jmp))
  (define ifa ifnbe)
  (define ifs (make-if jns jmp))
  (define ifns (make-if js jmp))
  (define ifp (make-if jnp jmp))
  (define ifpe ifp)
  (define ifnp (make-if jp jmp))
  (define ifpo ifnp)
  (define ifl (make-if jnl jmp))
  (define ifnge ifl)
  (define ifnl (make-if jl jmp))
  (define ifge ifnl)
  (define ifle (make-if jnle jmp))
  (define ifng ifle)
  (define ifnle (make-if jle jmp))
  (define ifg ifnle)
  
  (define ifol (make-if jnol jmpl))
  (define ifnol (make-if jol jmpl))
  (define ifbl (make-if jnbl jmpl))
  (define ifnael ifbl)
  (define ifcl ifbl)
  (define ifnbl (make-if jbl jmpl))
  (define ifael ifnbl)
  (define ifncl ifnbl)
  (define ifzl (make-if jnzl jmpl))
  (define ifel ifzl)
  (define ifnzl (make-if jzl jmpl))
  (define ifnel ifnzl)
  (define ifbel (make-if jnbel jmpl))
  (define ifnal ifbel)
  (define ifnbel (make-if jbel jmpl))
  (define ifal ifnbel)
  (define ifsl (make-if jnsl jmpl))
  (define ifnsl (make-if jsl jmpl))
  (define ifpl (make-if jnpl jmpl))
  (define ifpel ifpl)
  (define ifnpl (make-if jpl jmpl))
  (define ifpol ifnpl)
  (define ifll (make-if jnll jmpl))
  (define ifngel ifll)
  (define ifnll (make-if jll jmpl))
  (define ifgel ifnll)
  (define iflel (make-if jnlel jmpl))
  (define ifngl iflel)
  (define ifnlel (make-if jlel jmpl))
  (define ifgl ifnlel)))
