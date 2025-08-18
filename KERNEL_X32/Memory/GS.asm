block(.structs){
     virtual at 0
          GS.Off      dd ?
          GS.CThread  dd ?
	     GS.CProcess dd ?
	     GS.Quantum  dd ?
          GS.XhTable  dd 32 dup ?
     end virtual
}

block(.text){
proc GS.Init
     mov       eax, 0xFF000000
	mov       [eax + GS.Off], eax 
     xor       edx, edx
     mov       [eax + GS.CThread], edx
     mov       [eax + GS.CProcess], edx
     inc       edx
     mov       [eax + GS.Quantum], edx
     ret
endp

proc GS.Base
     xor       edx, edx
     mov       eax, [gs:edx]
     ret
endp
}