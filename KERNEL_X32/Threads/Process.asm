block(.consts){
        PROCESS_FREE    equ 0
        PROCESS_ACTIVE  equ 1
        PROCESS_DEAD    equ 2

virtual at 0
        Process.CR3         dd ?
        Process.FileTab     dd ?
        Process.SigHandler  dd ?
        Process.Accounting  dd ?
        Process.Procname    dd ?
        Process.Status      db ?
        Process.Pad         db 11 dup ?
        ProcessSize:
end virtual
}

block(.text){
proc Process.Create uses edi esi ebx
PT.Old equ 0xFFFFF000
PT.New equ 0xFF300000
     stdcall FramePool.GetFreePage
     xchg      ebx, eax 
     STOP_POINT
     stdcall Mutex.Wait, Process.ProcessMutex
     stdcall Pager.MapPage, PT.New shr 12, ebx, AL_FL_WRITABLE

     mov       edi, PT.New + 0x3C0 * 4
     mov       esi, PT.Old + 0x3C0 * 4

     mov       ecx, 0x3FF - 0x3C0
     
     pushf
     cld
     rep movsd
     popf

     mov       eax, ebx
     shl       eax, 12
     and       dword[PT.New + 0x3FF * 4], 0x00000FFF
     or        [PT.New + 0x3FF * 4], eax
     or        byte [PT.New + 0x3FF * 4], 0000_0011
     
     stdcall Pager.Unmap, PT.New shr 12

     stdcall Mutex.Release, Process.ProcessMutex

     stdcall Mutex.Wait, Process.Mutex
     
     xor       ecx, ecx
     mov       edi, [Process.Process]
@@:
     inc       ecx
     imul      eax, ecx, ProcessSize
     cmp       [edi + eax + Process.CR3], 0
     jnz       @B

     shl       ebx, 12
     mov       [edi + eax + Process.CR3], ebx 
     mov       ebx, ecx

     stdcall   Mutex.Release, Process.Mutex

     xchg      eax, ebx
.EndProc:     
     ret
endp


proc Process.Kill process:WORD
     stdcall   Mutex.Wait, Process.Mutex

     movzx     edx, [process]
     imul      eax, edx, ProcessSize 
     add       eax, [Process.Process]

     cmp       [eax + Process.Status], PROCESS_ACTIVE
     je        @F
     stdcall   Mutex.Release, Process.Mutex
     jmp       .EndProc
 @@:    
     mov       [eax + Process.Status], PROCESS_DEAD
     stdcall   Mutex.Release, Process.Mutex

     int       31h
     stdcall   Sched.Signal, ProcessManager.TerminatorThread 

.EndProc:     
     ret
endp

}

block(.initData){
     Process.Process dd 0xFF102000
}

block(.data){
     Process.ProcessMutex db ?
     Process.Mutex db ? 
}