include 'macro\proc32.inc'

use32

block(.text){
proc Interrupt.Void
     xchg bx, bx
     iretd
endp

proc Interrupt.Make uses ecx es;{ 
; 
; dx:eax - адрес прерывания, 
; esi - смещение  в IDT
     mov       cx, KERNEL_DATA_SELECTOR
     mov       es, cx
     mov       [es:esi+IDT], eax
     mov       [es:esi+ IDT + 4], eax
     mov       [es:esi+ IDT + 4], word 0x8E00
     mov       [es:esi+ IDT + 2], dx
     add       esi, 8
     ret
endp
}