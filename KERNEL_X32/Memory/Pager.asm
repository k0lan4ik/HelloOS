block(.consts){
    AL_FL_WRITABLE  equ 0x01
    AL_FL_USERACC   equ 0x02
    AL_FL_NOEXEC    equ 0x04
    AL_FL_GLOBAL    equ 0x08
    AL_FL_PAT       equ 0x10
    AL_FL_PCD       equ 0x20
    AL_FL_PWT       equ 0x40

    PML1BASE        equ 0xFFC00000
    PML2BASE        equ 0xFFFFF000
    PAGE_SHIFT      equ 10
    PAGE_MASK       equ 0xFFFFF
}

block(.text){

proc Pager.Unmap virt
     mov       eax, [virt]
     shr       eax, PAGE_SHIFT
     and       eax, PAGE_MASK shr PAGE_SHIFT
     push      eax
     add       eax, PML2BASE 
     ;IDE.Write PML2BASE
     test byte [eax], 1
     pop       eax
     jnz       @F
     push      -1
     jmp       .EndProc  
@@:
     mov       eax, [virt]
     and       eax, PAGE_MASK
     
     test byte [eax], 1
     pop       eax
     jnz       @F
     push      -1
     jmp       .EndProc  
@@:
     push      eax
     add       eax, PML1BASE
     and  byte [eax], 0xFD
     
     mov       edx, [eax]
     pop       eax
     shr       edx, 12
     push      edx

     mov       edx, [virt]
     and       edx, (PAGE_MASK and (not ((1 shl PAGE_SHIFT) - 1)))
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
     add       eax, PML2BASE
     mov       edx, [eax]
     shr       edx, 12

     stdcall   Pager.Unmap, edx
     cmp       eax, -1
     je        .EndProc
     stdcall   FramePool.FreePage, eax

.EndProc:
     pop       eax
     ret
endp

proc Pager.MapPage uses ebx, todovirt, todophys, flags:DWORD
     ;STOP_POINT
     mov       ebx, [todovirt]
     and       ebx, PAGE_MASK
     shr       ebx, PAGE_SHIFT

     push      ebx
     shl       ebx, 2
     add       ebx, PML2BASE
     test byte [ebx], 1
     pop       ebx
     jnz       @F
     stdcall   FramePool.GetFreePage
     shl       eax, 12
     or        eax, 11111b
     push      ebx
     shl       ebx, 2
     add       ebx, PML2BASE
     mov       [ebx], eax
     pop       ebx
@@:
     mov       ebx, [todovirt]
     and       ebx, PAGE_MASK
     
     push      ebx
     shl       ebx, 2
     add       ebx, PML1BASE
     test byte [ebx], 1
     pop       ebx
     jnz       .EndProc

     mov       eax, [todophys]
     shl       eax, 12
     or        eax, 0x0001
     test      [flags], AL_FL_WRITABLE
     jz        @F
     or        eax, AL_FL_WRITABLE shl 1
@@:
     test      [flags], AL_FL_USERACC
     jz        @F
     or        eax, AL_FL_USERACC shl 1 
@@:     
     
     shl       ebx, 2
     add       ebx, PML1BASE
     mov       [ebx], eax
         
.EndProc:
     ret
endp

}
