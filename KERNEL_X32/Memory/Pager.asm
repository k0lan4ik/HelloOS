block(.consts){
    AL_FL_WRITABLE  equ 0x01
    AL_FL_USERACC   equ 0x02
    AL_FL_NOEXEC    equ 0x04
    AL_FL_GLOBAL    equ 0x08
    AL_FL_PAT       equ 0x10
    AL_FL_PCD       equ 0x20
    AL_FL_PWT       equ 0x40

    X86_PTE_PRESENT  equ 0x001
    X86_PTE_WRITABLE equ 0x002
    X86_PTE_USER     equ 0x004
    X86_PTE_PWT      equ 0x008
    X86_PTE_PCD      equ 0x010
    X86_PTE_ACCESSED equ 0x020
    X86_PTE_DIRTY    equ 0x040
    X86_PTE_PAT      equ 0x080
    X86_PTE_GLOBAL   equ 0x100

    PML1BASE        equ 0xFFC00000
    PML2BASE        equ 0xFFFFF000
    PAGE_SHIFT      equ 10
    PAGE_MASK       equ 0xFFFFF000

;virtual at 0
;     PageFault.Handler   dd ?
;     PageFault.MinVirt   dd ?
;     PageFault.MaxVirt   dd ?
;     PageFault.MinEIP    dd ?
;     PageFault.MaxEIP    dd ?
;     PageFault.Size:
;end virtual
;     PageFaults     equ 0xFF120000
}

block(.text){

proc Pager.Unmap virt
     mov       eax, [virt]
     and       eax, PAGE_MASK 
     shr       eax, PAGE_SHIFT
     
     push      eax
     shl       eax, 2
     add       eax, PML2BASE 
     ;IDE.Write PML2BASE
     test byte [eax], 1
     pop       eax
     jnz       @F
     push      -1
     jmp       .EndProc  
@@:
     push      eax
     mov       eax, [virt]
     and       eax, PAGE_MASK
     shl       eax, 2
     add       eax, PML1BASE
     test byte [eax], 1
     pop       eax
     jnz       @F
     push      -1
     jmp       .EndProc  
@@:
     push      eax
     mov       eax, [virt]
     and       eax, PAGE_MASK
     shl       eax, 2
     add       eax, PML1BASE
     and  byte [eax], 0xFE
     
     mov       edx, [eax]
     pop       eax
     shr       edx, 12
     push      edx

     mov       edx, [virt]
     and       edx, (PAGE_MASK and (not ((1 shl PAGE_SHIFT) - 1)))
     shl       edx, 2
     mov       ecx, 1024
@@:
     push      edx
     add       edx, PML1BASE
     test byte [edx + ecx * 4 - 4], 1
     pop       edx
     jnz       .EndProc
     
     loop      @B        

     mov       eax, [virt]
     and       eax, PAGE_MASK
     shr       eax, PAGE_SHIFT
     shl       eax, 2
     add       eax, PML2BASE
     mov       edx, [eax]
     shr       edx, 12

     stdcall   Pager.Unmap, edx
     cmp       eax, -1
     je        .EndProc
     stdcall   FramePool.FreePage, eax
    
.EndProc:
     mov       eax, [virt]
     shl       eax, 12
     invlpg    [eax]
     pop       eax
     ret
endp

proc Pager.MapPage uses ebx, todovirt, todophys, flags:DWORD
     ;STOP_POINT
     mov       ebx, [todovirt]
     shr       ebx, 12
     shr       ebx, PAGE_SHIFT

     push      ebx
     shl       ebx, 2
     add       ebx, PML2BASE
     test byte [ebx], 1
     pop       ebx
     jnz       @F
     xor       eax, eax
     stdcall   PMM.GetFreePage, eax
     shl       eax, 12
     or        eax, 11111b
     push      ebx
     shl       ebx, 2
     add       ebx, PML2BASE
     mov       [ebx], eax
     pop       ebx
@@:
     mov       ebx, [todovirt]
     shr       ebx, 12
     
     push      ebx
     shl       ebx, 2
     add       ebx, PML1BASE
     test byte [ebx], 1
     pop       ebx
     jnz       .EndProc

     mov       eax, [todophys]
     and       eax, PAGE_MASK
     mov edx, [flags]
  
     or  eax, X86_PTE_PRESENT 

     test edx, AL_FL_WRITABLE
     jz @F
     or eax, X86_PTE_WRITABLE
@@:
     test edx, AL_FL_USERACC
     jz @F
     or eax, X86_PTE_USER
@@:
     test edx, AL_FL_PWT
     jz @F
     or eax, X86_PTE_PWT
@@:
     test edx, AL_FL_PCD
     jz @F
     or eax, X86_PTE_PCD
@@:
     test edx, AL_FL_PAT
     jz @F
     or eax, X86_PTE_PAT
@@:
     test edx, AL_FL_GLOBAL
     jz @F
     or eax, X86_PTE_GLOBAL
@@:
     test edx, AL_FL_NOEXEC
     jz @F
@@:
     
     shl       ebx, 2
     add       ebx, PML1BASE
     mov       [ebx], eax
     mov       eax, [todovirt]
     invlpg    [eax]  
.EndProc:
     
     ret
endp


;proc Pager.Init uses ebx
;     mov       ecx, 16
;     mov       ebx, PageFaults
;@@:
;     mov       eax, PageFault.Size
;     mul       ecx
;     mov       [eax + PageFault.MinVirt], edx
;     mov       [eax + PageFault.MaxVirt], edx
;     mov       [eax + PageFault.MinEIP], edx
;     mov       [eax + PageFault.MaxEIP], edx
;     mov       [eax + PageFault.Handler], edx
;     loop      @B
;endp
;
;proc Pager.AddPFHandler uses ebx, handler, minvirt, maxvirt, mineip, maxeip
;     xor       ecx, ecx
;     dec       ecx
;     mov       ebx, PageFaults
; @@:    
;     inc       ecx
;     mov       eax, PageFault.Size
;     mul       ecx
;     cmp       [ebx + eax + PageFault.Handler], 0
;     jnz       @B
;
;     mov       edx, [handler]
;     mov       [ebx + eax + PageFault.Handler], edx 
;     mov       edx, [minvirt]
;     mov       [ebx + eax + PageFault.MinVirt], edx
;     mov       edx, [maxvirt]
;     mov       [ebx + eax + PageFault.MaxVirt], edx
;     mov       edx, [mineip]
;     mov       [ebx + eax + PageFault.MinEIP], edx
;     mov       edx, [maxeip]
;     mov       [ebx + eax + PageFault.MaxEIP], edx
;
;     ret
;endp
;
;proc Pager.DeletePFHandler uses ebx, handler, minvirt, maxvirt, mineip, maxeip
;     xor       ecx, ecx
;     dec       ecx
;     mov       ebx, PageFaults
; @@:    
;     inc       ecx
;     mov       eax, PageFault.Size
;     mul       ecx
;     mov       edx, [handler]
;     cmp       [ebx + eax + PageFault.Handler], edx
;     jz        @F
;     cmp       ecx, 8
;     jb        @B
;@@:
;     cmp       ecx, 8
;     jae       @F
;     xor       edx, edx
;     mov       [ebx + eax + PageFault.Handler], edx 
;     mov       [ebx + eax + PageFault.MinVirt], edx
;     mov       [ebx + eax + PageFault.MaxVirt], edx
;     mov       [ebx + eax + PageFault.MinEIP], edx
;     mov       [ebx + eax + PageFault.MaxEIP], edx
;@@:
;     ret
;endp
;
;proc Pager.HandlePF uses ebx edi, error, eeip
;     mov       edi, cr2
;     mov       ecx, 8
;     mov       ebx, PageFaults
;@@:
;     push      ecx
;     mov       eax, PageFault.Size
;     mul       ecx
;     cmp       [ebx + eax + PageFault.MinVirt], edi
;     jae       .SkipLoop  
;     cmp       [ebx + eax + PageFault.MaxVirt], edi
;     jbe       .SkipLoop  
;     mov       ecx, [eeip]
;     cmp       [ebx + eax + PageFault.MinEIP], ecx
;     jae       .SkipLoop  
;     cmp       [ebx + eax + PageFault.MaxEIP], ecx
;     jbe       .SkipLoop  
;     cmp       [ebx + eax + PageFault.Handler], 0
;     jz        .SkipLoop 
;     stdcall   [ebx + eax + PageFault.Handler], [error], ecx, edi
;.SkipLoop:
;     pop       ecx
;     loop      @B
;     ret
;endp
}
