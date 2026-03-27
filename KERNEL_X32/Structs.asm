block(.structs){
virtual at $
align     1000h
TSS:
     .LINK       dw ?
     .Res1       dw ?
     .ESP0       dd ?
     .SS0        dw ?
     .Res2       dw ? 
     .ESP1       dd ?
     .SS1        dw ?
     .Res3       dw ?
     .ESP2       dd ?
     .SS2        dw ?
     .Res4       dw ?
     .CR3        dd ?
     .EIP        dd ?
     .EFLAGS     dd ?
     .EDI	       dd ?
     .ESI	       dd ?
     .EBP	       dd ?
     .ESP	       dd ?
     .EBX	       dd ?
     .EDX	       dd ?
     .ECX	       dd ?
     .EAX        dd ?
     .ES         dw ?
     .Res5       dw ?
     .CS         dw ?
     .Res6       dw ?
     .SS         dw ?
     .Res7       dw ?
     .DS         dw ?
     .Res8       dw ?
     .FS         dw ?
     .Res9       dw ?
     .GS         dw ?
     .Res10      dw ?
     .LDTR       dw ?
     .Res11      dd ?
     .IOPB       dw ?
     .SSP        dd ?
TSSend:
align 1000h
PageDirectory:
     db        1000h dup ?    
PageTable1:
     db        1000h dup ?  
PageTable2:
     db        1000h dup ?  
end virtual
     
}

