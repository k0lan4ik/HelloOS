include 'macro\proc32.inc'

use32


proc Interrupt.Void
     xchg bx, bx
     iretd
endp

proc Interrupt.Make;{ 
; 
; dx:eax - адрес прерывания, 
; es:esi - адрес в ivt
     mov       [es:esi+0], eax
     mov       [es:esi+4], eax
     mov       [es:esi+4], word 0x8E00
     mov       [es:esi+2], dx
     add       esi, 8
     ret
endp