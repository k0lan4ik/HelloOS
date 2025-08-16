include 'macro\proc32.inc'

use32

block(.text){
proc Interrupt.Void
     xchg bx, bx
     iretd
endp

proc Interrupt.FaultsInit
     mov       esi, 0x0E * 8
     mov       eax, Interrupt.PageFault
     mov       dx, cs
     call      Interrupt.Make 
     ret  
endp

proc Interrupt.PageFault
     cli
     xchg      bx, bx
     push      ebp
     mov       ebp, esp
     pusha

     mov       eax, cr2
     mov       ebx, [ebp + 4]
     mov       ecx, [ebp + 8]

     mov       esi, Interrupt.PanicMsgPf
     call      ScreenMode03.PrintString

     mov       esi, Interrupt.FaultAddrMsg
     call      ScreenMode03.PrintString
     xchg      ebx, eax
     call      HexPrint

     mov       esi, Interrupt.ErrorCodeMsg
     call      ScreenMode03.PrintString
     xchg      ebx, eax
     call      HexPrint          

     mov       esi, Interrupt.EIPMsg
     call      ScreenMode03.PrintString
     mov       ebx, ecx
     call      HexPrint

     popa 
     pop       ebp        
     add       esp, 4   
     sti
     
     ;временно
     cli
     hlt

     iret

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

block(.data){
     Interrupt.PanicMsgPf    db "KERNEL PANIC: PAGE FAULT!", 13, 10, 0
     Interrupt.FaultAddrMsg  db "  Faulting Address: 0x", 0
     Interrupt.ErrorCodeMsg  db 13, 10, "  Error Code:       0x", 0
     Interrupt.EIPMsg        db 13, 10, "  EIP:              0x", 0

}