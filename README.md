#Dream Scheme Interpreter

This program is distributed under the terms of the GNU General Public License.
Copyright (C) 2011 David Joseph Stith

This is a scheme interpreter implemented in x86 machine language.
Many thanks to Abelson and Sussman for Structure and Interpretation of Computer
Programs from which the basic algorithms for the core of the interpreter and
its stop-and-copy garbage collector are derived.
Many thanks also to Chuck Moore whose ColorForth provided inspiration and
floppy controller code.

All essential syntax and procedures from R4RS are implemented.
Integers have 32 bit magnitude (with sign stored separately).
Real and complex numbers are still in the works. (Use at your own risk!)
Reals are treated as one argument lambdas (a positive integer indicating how
accurately to approximate).

All required forms and procedures of R5RS are implemented except:
let-syntax letrec-syntax syntax-rules define-syntax
rationalize exp log tan asin acos atan
values call-with-values dynamic-wind
char-ready? 

###Additional non-standard procedures include:

```
(macro FORMALS . BODY)  ==> MACRO
                       ;;A syntax closure whose arguments are not evaluated;
                       ;;when the MACRO itself is evaluated, it is replaced by
                       ;;its return value. Then this return value is evaluated
                       ;;for this and all future invokations.
(macro? OBJECT)               ==> BOOLEAN  ;;Object created by 'macro'

(& INTEGER INTEGER ...)       ==> INTEGER  ;;Bitwise and
(| INTEGER INTEGER ...)       ==> INTEGER  ;;Bitwise or
(^ INTEGER INTEGER ...)       ==> INTEGER  ;;Bitwise xor
(~ INTEGER)                   ==> INTEGER  ;;Bitwise not
(syntax? OBJECT)              ==> BOOLEAN  ;;(memq OBJECT (list begin lambda if
                                           ;;    define set! quote quasiquote))
(closure? OBJECT)             ==> BOOLEAN  ;;Object created by 'lambda'
(port? OBJECT)                ==> BOOLEAN  ;;(or (input-port? OBJECT)
                                           ;;    (output-port? OBJECT))
(make-immutable-string INTEGER [INTEGER])
                              ==> STRING   ;;Treats INTEGER as address
                                           ;;with optional INTEGER length.

(string-ref->byte STRING INTEGER)          ==> INTEGER (between 0 and #xff)
(string-ref->wyde STRING INTEGER)          ==> INTEGER (between 0 and #xffff)
(string-ref->triad STRING INTEGER)         ==> INTEGER (between 0 and #xffffff)
(string-ref->tetra STRING INTEGER)         ==> INTEGER
(string-set-byte! STRING INTEGER1 INTEGER2)   ;;Sets 8 bits at byte INTEGER1
(string-set-wyde! STRING INTEGER1 INTEGER2)   ;;Sets 16 bits at byte INTEGER1
(string-set-triad! STRING INTEGER1 INTEGER2)  ;;Sets 24 bits at byte INTEGER1
(string-set-tetra! STRING INTEGER1 INTEGER2)  ;;Sets 32 bits at byte INTEGER1
(substring-set! STRING1 INTEGER1 INTEGER2 STRING2)
                        ;;Sets (substring STRING1 INTEGER1 INTEGER2) to STRING2 
(string-hash STRING)                       ==> INTEGER
(string-ci->symbol STRING)                 ==> SYMBOL (lowercase)

(dlopen STRING)                            ==> ADDRESS
(dlclose ADDRESS)                          ==> INTEGER
(dlsym ADDRESS STRING)                     ==> ADDRESS
(dlcall ADDRESS [INTEGER|STRING|PORT] ...) ==> INTEGER
```
