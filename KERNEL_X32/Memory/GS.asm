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
     xor       edx, edx
     mov       eax, [gs:edx]
     ret
endp
}