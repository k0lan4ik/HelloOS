block(.structs){
     virtual at 0
          GS.Off      dd ?
          GS.CThread  dw ?
	     GS.CProcess dw ?
	     GS.Quantum  db ?
          GS.XhTable  dd 32 dup ?
     end virtual
}

block(.text){
proc GS.Init
     mov       eax, 0xFF000000
	mov       [eax + GS.Off], eax 
     xor       edx, edx
     mov       [eax + GS.CThread], dx
     mov       [eax + GS.CProcess], dx
     inc       edx
     mov       [eax + GS.Quantum], dl
     ret
endp

proc GS.Base
     ;xor       edx, edx
     mov       eax, 0xFF000000
     mov       eax, [eax]
     push      eax
     mov     edi, $f00B8010
     shr     eax, 16
     xchg    eax, ebx     
     mov     cx, 4
@@:
    rol     bx, 4
    mov     ax, bx
    and     al, 0000'0000_0000'1111b

    cmp     al, $0A
    sbb     al, $69
    das
    mov     ah, $07
    
    stosw
    loop    @B


     pop       eax
     cmp       eax, 0xFF000000 
     sete      al
     add       al, 'A'
     mov       ah, $be 
     stosw
     cli
     hlt 
     ret
endp
}