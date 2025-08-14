block(.consts){
Real.GDT       = GDT  - Options.Kernel.HierHalf
Real.GDTend    = GDTend - Options.Kernel.HierHalf
Real.GDTptr    = GDTptr - Options.Kernel.HierHalf
Real.IDT       = IDT - Options.Kernel.HierHalf
Real.IDTend    = IDTend - Options.Kernel.HierHalf
Real.IDTptr    = IDTptr - Options.Kernel.HierHalf
Real.PageDirectory = PageDirectory - Options.Kernel.HierHalf
Real.PageTable1 = PageTable1 - Options.Kernel.HierHalf
}

block(.structs){
     times (16 - ($ mod 16)) db ?


GDT:
     times 6 dq ?
GDTend:
GDTptr:
    dw ?  
    dd ?               
     times (16 - ($ mod 16)) db ?
IDT:
    times 256 dq ? 
IDTend:
IDTptr:
    dw ?   
    dd ?                
     times (16 - ($ mod 16)) db ?
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

     times  (4096 - ($ mod 4096)) db ?


PageDirectory:
     times  4096 db ?
PageTable1:
     times  4096 db ?

}

