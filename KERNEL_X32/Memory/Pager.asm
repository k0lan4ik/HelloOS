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
     test byte [eax + PML2BASE], 1
     jnz       @F
     push      -1
     jmp       .EndProc  
@@:
     mov       eax, [virt]
     and       eax, PAGE_MASK
     test byte [eax + PML1BASE], 1
     jnz       @F
     push      -1
     jmp       .EndProc  
@@:
     
     and  byte [eax + PML1BASE], 0xFD
     
     mov       edx, [eax + PML1BASE]
     shr       edx, 12
     push      edx

     mov       edx, [virt]
     and       edx, (PAGE_MASK and (not ((1 shl PAGE_SHIFT) - 1)))
     mov       ecx, 1024
@@:
     test byte [PML1BASE + edx + ecx * 4 - 4], 1
     jnz       .EndProc
     
     loop      @B        

     mov       eax, [virt]
     and       eax, PAGE_MASK
     shr       eax, PAGE_SHIFT
     mov       edx, [PML2BASE + eax]
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
     mov       ebx, [todovirt]
     and       ebx, PAGE_MASK
     shr       ebx, PAGE_SHIFT

     test byte [PML2BASE + ebx], 1
     jnz       @F
     ;stdcall   FramePool.GetFreePage
     shl       eax, 12
     and       eax, 11111b
     mov       [PML2BASE + ebx], eax
@@:
     mov       ebx, [todovirt]
     
     test byte [PML2BASE + ebx], 1
     jnz       .EndProc

     mov       eax, [todophys]
     shl       eax, 12
     test      [flags], AL_FL_WRITABLE
     jz        @F
     or        eax, AL_FL_WRITABLE shl 1
@@:
     test      [flags], AL_FL_USERACC
     jz        @F
     or        eax, AL_FL_USERACC shl 1 
@@:     
     mov       [PML2BASE + ebx], eax
         
.EndProc:
     ret
endp

}
