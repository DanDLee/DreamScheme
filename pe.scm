;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;This program is distributed under the terms of the       ;;;
;;;GNU General Public License.                              ;;;
;;;Copyright (C) 2011 David Joseph Stith                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (distance a b)
  (- (lookup b) (lookup a)))

;;;;;;;;;;;;;;;;;
;;; PE Header ;;;
;;;;;;;;;;;;;;;;;
(: 'mzhdr)
  (ascii "MZ")                  ; e_magic
  (wydes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  (tetra (file-offset 'pesig))  ; e_lfanew
(: 'pesig)
  (ascii "PE")(byte 0)(byte 0)
  (wyde #x014C)                 ; Machine (Intel 386)
  (wyde 1)                      ; NumberOfSections
  (tetras 0 0 0)
  (wyde (distance 'opthdr 'opthdr_end)) ; SizeOfOptionalHeader
  (wyde #x103)                  ; Characteristics (no relocations, executable, 32 bit)
(: 'opthdr)
  (wyde #x10B)(bytes 8 0)
  (tetra (file-offset x86-bss-start)) ; SizeOfCode
  (tetra 0)                     ; SizeOfInitializedData
  (tetra 0)                     ; SizeOfUninitializedData
  (tetra (file-offset 'start))  ; AddressOfEntryPoint
  (tetra 0)                     ; BaseOfCode
  (tetra 0)                     ; BaseOfData
  (tetra x86-text-start)        ; ImageBase
  (tetra 1)                     ; SectionAlignment
  (tetra 1)                     ; FileAlignment
  (wydes 4 0 0 0 4 0)(tetra 0)
  (tetra (file-offset x86-bss-end)) ; SizeOfImage
  (tetra (file-offset 'hdr_end)); SizeOfHeaders
  (tetra 0)                     ; CheckSum
  (wyde 3)                      ; Subsystem (Win32 CUI)
  (wyde #x400)                  ; DllCharacteristics
  (tetra #x100000)              ; SizeOfStackReserve
  (tetra #x1000)                ; SizeOfStackCommit
  (tetra #x100000)              ; SizeOfHeapReserve
  (tetra #x1000)                ; SizeOfHeapCommit
  (tetra 0)                     ; LoaderFlags
  (tetra 16)                    ; NumberOfRvaAndSizes
  (tetras 0 0)                  ; Export table
  (tetras (file-offset 'idata)
    (distance 'idata 'idata_end));Import table
  (tetras 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
(: 'opthdr_end)
  (ascii ".text")(bytes 0 0 0)        ; Name
  (tetra (file-offset x86-bss-end))   ; VirtualSize
  (tetra 0)                           ; VirtualAddress
  (tetra (file-offset x86-bss-end))   ; SizeOfRawData
  (tetra 0)                           ; PointerToRawData
  (tetras 0 0 0)
  (tetra #xe0000020)                  ; Characteristics (code, execute, read, write)
(: 'hdr_end)
(: 'idata)
  (tetra (file-offset 'ilt_kernel32)) ; OriginalFirstThunk
  (tetras 0 0)
  (tetra (file-offset 'kernel32))     ; Name
  (tetra (file-offset 'iat_kernel32)) ; FirstThunk
  (tetras 0 0 0 0 0)
(: 'idata_end)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'ilt_kernel32)
  (tetra (file-offset 'name_ExitProcess))
  (tetra (file-offset 'name_GetStdHandle))
  (tetra (file-offset 'name_ReadFile))
  (tetra (file-offset 'name_WriteFile))
  (tetra (file-offset 'name_CreateFileA))
  (tetra (file-offset 'name_CloseHandle))
  (tetra (file-offset 'name_GetModuleHandleA))
  (tetra (file-offset 'name_GetCommandLineA))
  (tetra (file-offset 'name_LoadLibraryA))
  (tetra (file-offset 'name_FreeLibrary))
  (tetra (file-offset 'name_GetProcAddress))
  (tetra 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'iat_kernel32)
(: 'ExitProcess)      (tetra (file-offset 'name_ExitProcess))
(: 'GetStdHandle)     (tetra (file-offset 'name_GetStdHandle))
(: 'ReadFile)         (tetra (file-offset 'name_ReadFile))
(: 'WriteFile)        (tetra (file-offset 'name_WriteFile))
(: 'CreateFileA)      (tetra (file-offset 'name_CreateFileA))
(: 'CloseHandle)      (tetra (file-offset 'name_CloseHandle))
(: 'GetModuleHandleA) (tetra (file-offset 'name_GetModuleHandleA))
(: 'GetCommandLineA)  (tetra (file-offset 'name_GetCommandLineA))
(: 'dlopen_rel)       (tetra (file-offset 'name_LoadLibraryA))
(: 'dlclose_rel)      (tetra (file-offset 'name_FreeLibrary))
(: 'dlsym_rel)        (tetra (file-offset 'name_GetProcAddress))
  (tetra 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(align 2) (: 'name_ExitProcess)      (wyde 0) (asciz "ExitProcess")
(align 2) (: 'name_GetStdHandle)     (wyde 0) (asciz "GetStdHandle")
(align 2) (: 'name_ReadFile)         (wyde 0) (asciz "ReadFile")
(align 2) (: 'name_WriteFile)        (wyde 0) (asciz "WriteFile")
(align 2) (: 'name_CreateFileA)      (wyde 0) (asciz "CreateFileA")
(align 2) (: 'name_CloseHandle)      (wyde 0) (asciz "CloseHandle")
(align 2) (: 'name_GetModuleHandleA) (wyde 0) (asciz "GetModuleHandleA")
(align 2) (: 'name_GetCommandLineA)  (wyde 0) (asciz "GetCommandLineA")
(align 2) (: 'name_LoadLibraryA)     (wyde 0) (asciz "LoadLibraryA")
(align 2) (: 'name_FreeLibrary)      (wyde 0) (asciz "FreeLibrary")
(align 2) (: 'name_GetProcAddress)   (wyde 0) (asciz "GetProcAddress")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: 'kernel32)
  (asciz "KERNEL32.dll")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
