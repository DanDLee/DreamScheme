;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;
;;; ELF Header ;;;
;;;;;;;;;;;;;;;;;;
(define dynstr-names '())
(define (dynstr-index name)
  (define (lookup i names)
    (cond
      ((null? names)
       (set! dynstr-names (cons name dynstr-names))
       i)
      ((string=? name (car names)) (display (car names)) i)
      (else (lookup (+ i (string-length (car names)) 1) (cdr names)))))
  (lookup 1 (reverse dynstr-names)))

(bytes
  #x7f #\E #\L #\F ;ELF identification
  1  ;32bit
  1  ;Little endian
  1  ;File version
  0  ;System V ABI
  0  ;ABI version
  0 0 0 0 0 0 0)
(wyde 2)      ;Object File Type (Executable)
(wyde 3)      ;Architecture (Intel 386)
(tetras 1     ;ELF Version (Current)
       'start ;Entry Point
       (file-offset 'program_header) ;Program Header File Offset
       0 ;Section Table File Offset
       0)     ;Processor-specific Flags
(wydes (file-offset 'program_header)   ;ELF Header Size
       #x20   ;Program Header Table Entry Size
       (if LIBDL 4 2) ;Program Header Table Entry Count
       #x28   ;Section Header Table Entry Size
       0      ;Section Header Table Entry Count
       0)     ;Section Header String Table Index
;;
;;Program header table
;;
(: 'program_header)
(if LIBDL
  (begin
;;;INTERP entry
    (tetras
        3                ;Segment Type (PT_INTERP)
        (file-offset 'interp) ;Segment File Offset
        (lookup 'interp) ;Segment Virtual Address
        0                ;Segment Physical Address (Ignored)
        (- (lookup 'interp_end) (lookup 'interp)) ;Segment File Size
        (- (lookup 'interp_end) (lookup 'interp)) ;Segment Memory Size
        4                ;Segment Flags (read)
        #x1000           ;Segment Alignment
        )
;;;DYNAMIC entry
    (tetras
        2                ;Segment Type (PT_DYNAMIC)
        (file-offset 'dynamic) ;Segment File Offset
        (lookup 'dynamic) ;Segment Virtual Address
        0                ;Segment Physical Address (Ignored)
        (- (lookup 'dynamic_end) (lookup 'dynamic)) ;Segment File Size
        (- (lookup 'dynamic_end) (lookup 'dynamic)) ;Segment Memory Size
        6                ;Segment Flags (read+write)
        #x1000           ;Segment Alignment
        )))

;;;TEXT entry
(tetras 1                ;Segment Type (PT_LOAD)
        0                ;Segment File Offset
        x86-text-start   ;Segment Virtual Address
        0                ;Segment Physical Address (Ignored)
        (- x86-data-start x86-text-start) ;Segment File Size
        (- x86-data-start x86-text-start) ;Segment Memory Size
        5                ;Segment Flags (read+execute)
        #x1000           ;Segment Alignment
        )
;;;DATA entry
(tetras 1                ;Segment Type (PT_LOAD)
        (- x86-data-start x86-text-start) ;Segment File Offset
        x86-data-start ;Segment Virtual Address
        0                ;Segment Physical Address (Ignored)
        (- x86-bss-start x86-data-start) ;Segment File Size
        (- x86-bss-end x86-data-start) ;Segment Memory Size
        7                ;Segment Flags (read+write+execute)
        #x1000           ;Segment Alignment
        )

(: 'hash)
  (tetras 1 5 0 1 2 3 4 0)

(define dynamic_sym 1)
(define dlopen_sym 2)
(define dlclose_sym 3)
(define dlsym_sym 4)
(if LIBDL
 (begin
  ;;
  ;;DYNAMIC Segment
  ;;
  (: 'dynamic)
  (tetras 1 (dynstr-index LIBDL)) ;DT_NEEDED
  (tetras 4 'hash)      ;DT_HASH
  (tetras 5 'dynstr)    ;DT_STRTAB
  (tetras 6 'dynsym)    ;DT_SYMTAB
  (tetras 10 (- (lookup 'dynstr_end) (lookup 'dynstr))) ;DT_STRSZ
  (tetras 11 #x10)      ;DT_SYMENT
  (tetras 17 'reltext)  ;DT_REL
  (tetras 18 (- (lookup 'reltext_end) (lookup 'reltext)));DT_RELSZ
  (tetras 19 #x08)      ;DT_RELENT
  (: 'dynamic_end)
  (: 'dynsym)
  (tetras 0 0 0)(wydes 0 0)
  (tetras (dynstr-index "_DYNAMIC") 'dynamic 0)
  (wydes #x11   ;STB_GLOBAL+STT_OBJECT
         #xfff1);SHN_ABS
  (tetras (dynstr-index "dlopen") 0 0)
  (wydes #x22   ;STB_WEAK+STT_FUNC
         0)
  (tetras (dynstr-index "dlclose") 0 0)
  (wydes #x22   ;STB_WEAK+STT_FUNC
         0)
  (tetras (dynstr-index "dlsym") 0 0)
  (wydes #x22   ;STB_WEAK+STT_FUNC
         0)
  (: 'reltext)
  (tetra 'dlopen_rel)
  (byte 1) ;R_386_32
  (byte dlopen_sym)
  (wyde 0)

  (tetra 'dlclose_rel)
  (byte 1) ;R_386_32
  (byte dlclose_sym)
  (wyde 0)

  (tetra 'dlsym_rel)
  (byte 1) ;R_386_32
  (byte dlsym_sym)
  (wyde 0)
  (: 'reltext_end)

  (: 'dynstr)
  (byte 0)
  (for-each asciz (reverse dynstr-names))
  (: 'dynstr_end)

  ;;
  ;;INTERP Segment
  ;;
  (: 'interp)
  (asciz "/lib/ld-linux.so.2")
  (: 'interp_end)))
(align 8)
