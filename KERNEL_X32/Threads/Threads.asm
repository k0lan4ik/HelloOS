block(.consts){
THREAD_NONE         equ 0 
THREAD_RUNNING      equ 1
THREAD_AVAILABLE    equ 2
THREAD_BLOCKED      equ 3
THREAD_SWAPPED      equ 4
THREAD_SWAPPEDBLOCKED equ 5
THREAD_DEAD         equ 6
THREAD_NEW          equ 7


virtual at 0
     Thread.EAX               dd ?
     Thread.ECX               dd ?
     Thread.EDX               dd ?
     Thread.EBX               dd ?
     Thread.ESi               dd ?
     Thread.EDi               dd ?
     Thread.ESP               dd ?
     Thread.EBP               dd ?
     
     
     Thread.KernelStackBase   dd ?
     Thread.KernelStack       dd ?
     Thread.StackBase         dd ?
     Thread.Stack             dd ?
     Thread.FsBase            dd ?

     Thread.Pid               dw ?
     Thread.Priority          db ?
     Thread.Quantum           db ?
     Thread.State             db ?
     Thread.Unblock           db ?
     Thread.Killed:                     ; 1
     Thread.Type:                       ; 3
     Thread.SignalWaiting:              ; 1
     Thread.Pad1              dw ?      ; 11

     Thread.Next              dw ?
     Thread.Previous          dw ?

     Thread.Pad              db 448 dup ?

     Thread.XmmStateArea      db 512 dup ?
     ThreadSize:
end virtual


}

block(.text){

proc Threads.Create  uses ebx edi esi, process:WORD, pentry
     stdcall   Mutex.Wait, Threads.Mutex
     mov       ebx, [Threads.Threads]
     mov       ecx, -1
@@:
     inc       ecx
     mov       eax, ThreadSize
     mul       ecx

     cmp       [ebx + eax + Thread.State], THREAD_NONE
     jne       @B

     mov       [ebx + eax + Thread.State], THREAD_NEW
     and       byte[ebx + eax + Thread.Type], 10001111b
     
     cmp       [process], 0
     jne  @F
     or        byte[ebx + eax + Thread.Type], 00110000b
@@:     

     movzx     edx, byte[ebx + eax + Thread.Type]
     mov       esi, Sched.P
     cmp       word[esi + edx * 4], 0
     jne       @F
     
     mov       word[ebx + eax + Thread.Next], 0
     mov       esi, Sched.P
     mov       [esi + edx * 4], cx

     mov       esi, Sched.End
     mov       [esi + edx * 4], cx
     
     jmp       .EndIfSh
@@:
     mov       esi, Sched.P
     movzx     edi, word[esi + edx * 4]
     mov       word[ebx + eax + Thread.Next], di
     add       edi, [Threads.Threads]
     mov       [edi + Thread.Previous], cx
     mov       [esi + edx * 4], cx
.EndIfSh:
     push      eax ecx
     stdcall   Mutex.Release, Threads.Mutex
     pop       ecx eax
     
     mov       di, [process]
     mov       word[ebx + eax + Thread.Pid], di
     mov       byte[ebx + eax + Thread.Priority], 20
     mov       byte[ebx + eax + Thread.Quantum], 20

     mov       byte[ebx + eax + Thread.Unblock], 20
     and       word[ebx + eax + Thread.Killed], 01110111_11111111b
     mov       word[ebx + eax + Thread.Previous], 0 
     
     xchg      eax, edi
     cmp       [pentry], 0xF0000000
     jb        @F
     
     push      ecx
     stdcall   KernelMemManager.Malloc, 8192
     pop       ecx

     mov       [ebx + edi + Thread.StackBase], eax
     add       eax, 8168
     mov       [ebx + edi + Thread.Stack], eax
     
     
     mov       [ebx + edi + Thread.ESP], eax


     mov       [ebx + edi + Thread.KernelStackBase], 0
     mov       [ebx + edi + Thread.KernelStack], 0

     mov       dword[eax], 0
     mov       dword[eax + 4], 0x30
     mov       edx, [pentry]
     mov       [eax + 8], edx
     mov       dword[eax + 12], 0x8
     mov       dword[eax + 16], 0x40200

     jmp       .EndIfPR
@@:
     push      ecx
     stdcall   KernelMemManager.Malloc, 8192
     pop       ecx
     
     mov       [ebx + edi + Thread.KernelStackBase], eax
     add       eax, 8160
     mov       [ebx + edi + Thread.KernelStack], eax

     mov       dword[eax], 0
     mov       dword[eax + 4], 0x30
     mov       edx, [pentry]
     mov       dword[eax + 8], edx
     mov       dword[eax + 12], 0x20
     mov       dword[eax + 16], 0x40200
     mov       edx, [ebx + edi + Thread.Stack]
     mov       dword[eax + 20], edx
     mov       dword[eax + 24], 0x28

.EndIfPR:
     
     mov       [ebx + edi + Thread.State], THREAD_AVAILABLE
     xchg      eax, ecx
     ret
endp

proc Threads.Kill thread:WORD
     cmp       word[thread], 0
     je        .EndProc

     stdcall   Mutex.Wait, Threads.Mutex

     movzx     eax, word[thread]
     mov       edx, ThreadSize
     mul       edx

     add       eax, [Threads.Threads]

     cmp       byte [eax + Thread.State], THREAD_NONE
     jne       .EndIfSt
     cmp       byte [eax + Thread.State], THREAD_DEAD
     jne       .EndIfSt
     test      byte [eax + Thread.Killed], 1000_0000b
     jne       .EndIfSt
     stdcall   Mutex.Release, Threads.Mutex
     jmp       .EndProc
.EndIfSt:

     or        byte [eax + Thread.Killed], 1000_0000b
     
     cmp       word [eax + Thread.Next], 0
     je        @F
     movzx     edx, word [eax + Thread.Next]
     imul      edx, edx, ThreadSize
     add       edx, [Threads.Threads]
     mov       cx,  word [eax + Thread.Previous]
     mov       [edx + Thread.Previous], cx
     jmp       .EndifNext
@@:     
     mov       edx, [eax + Thread.Type] 
     and       edx, 01110000_00000000b
     shr       edx, 12

     mov       cx, [thread]
     cmp       word [Sched.End + edx * 2], cx
     jne       .EndifNext
     mov       cx, [eax + Thread.Previous]
     mov       word [Sched.End + edx * 2], cx

.EndifNext:

     cmp       byte [eax + Thread.Previous], 0
     je        @F
     movzx     edx, byte [eax + Thread.Next]
     imul      edx, edx, ThreadSize
     add       edx, [Threads.Threads]
     mov       cx,  word [eax + Thread.Next]
     mov       [edx + Thread.Next], cx
     jmp       .EndifPrev
@@:     
     mov       edx, [eax + Thread.Type] 
     and       edx, 01110000_00000000b
     shr       edx, 12

     mov       cx, [thread]
     cmp       word [Sched.P + edx * 2], cx
     jne       .EndifPrev
     mov       cx, [eax + Thread.Next]
     mov       word [Sched.P + edx * 2], cx

.EndifPrev:

     mov       [eax + Thread.Next], 0
     mov       [eax + Thread.Previous], 0

     stdcall   Mutex.Release, Threads.Mutex

     int       31h

     stdcall   Sched.Signal, ProcessManager.TerminatorThread  

.EndProc:     
     ret
endp

}

block(.initData){
     Threads.Threads dd 0xFE000000
}

block(.data){
     Threads.Mutex db ?
     Threads.MaxThreads dw ?
}