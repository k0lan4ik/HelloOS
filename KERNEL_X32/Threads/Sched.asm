block(.consts){
     SCHED_FREE               equ 0
     SCHED_RUNNING            equ 1
     SCHED_AVAILABLE          equ 2
     SCHED_BLOCKED            equ 3
     SCHED_SWAPPED            equ 4
     SCHED_SWAPPED_BLOCKED    equ 5
     SCHED_NEW                equ 6
     SCHED_DEAD               equ 7

     
     TID equ 2
     PID equ 2
    
}

blok(.text){

proc Sched.HandlerInt
     push      gs
     push      eax
     
     mov       ax, 0x38
     mov       gs, ax

     mov       eax, 4

     mov       [gs:eax], ecx
     add       eax, 4
     mov       [gs:eax], edx
     add       eax, 4
     mov       [gs:eax], ebx
     add       eax, 4
     mov       [gs:eax], esi
     add       eax, 4
     mov       [gs:eax], edi
     add       eax, 4
     mov       [gs:eax], esp
     add       eax, 4
     mov       [gs:eax], ebp
     xor       eax, eax
     pop       ecx
     mov       [gs:eax], ecx

     mov       eax, 0x200
     fwait
     fxsave    [gs:eax]

     mov       ax, 0x30
     mov       gs, ax


     stdcall   Mutex.Wait Threads.Mutex

     stdcall   GS.Base
     mov       edx, eax

     imul      ebx, eax, ThreadSize
     add       ebx, [Threads.Threads]

     test      dword [ebx + Thread.Killed], 10000000:00000000b
     jnz       .EndIfKill
     cmp       byte  [ebx + Thread.State], THREAD_RUNNING
     jnz       .EndIfKill

     mov       byte [ebx + Thread.State], THREAD_AVAILABLE
     mov       al, [edx + GS.Quantum]
     mov       byte [ebx + Thread.Quantum], al

     movzx     ecx, word[ebx + Thread.Type]
     and        cl, 01110000b
     shr       cx, 12

     movzx     edi, word[Sched.End + ecx]
     test      edi, edi 
     jz        .ElseIfEnd

     mov       esi, edi
     add       esi, [Threads.Threads]  
     mov       ax, [edx + GS.CThread] 
     mov       [esi + Thread.Next], ax
     mov       [ebx + Thread.Previous], di
     mov       word [ebx + Thread.Next], 0

     mov       ax, [edx + GS.CThread]
     mov       [Sched.End + ecx], ax

.ElseIfEnd:
     mov       ax, [edx + GS.CThread] 
     mov       [Sched.P + ecx], ax
     mov       [Sched.End + ecx], ax
      
     mov       word [ebx + Thread.Next], 0
     mov       word [ebx + Thread.Previous], 0

.EndIfKill:

     mov       ecx, 4
.LoopIfZero:     
     mov       ebx, [Sched.P + ecx * 2 - 2]
     add       ebx, [Threads.Threads]
     cmp       [ebx + Thread.Quantum], 0
     jne       .NotZero
     loop      .LoopIfZero
     stdcall   Sched.Refill
.NotZero:

     mov       ecx, 4

.LoopFountThread:
     mov       ebx, [Sched.P + ecx * 2 - 2]
     test      ebx, ebx
     je        .SkipFound
     add       ebx, [Threads.Threads]
     cmp       [ebx + Thread.Quantum], 0
     jbe       .SkipFound

     mov       ax, [ebx + Thread.Next]
     mov       [Sched.P + ecx * 2 - 2], ax
     mov       word [eax + Thread.Previous], 0
     mov       word [ebx + Thread.Next], 0
     mov       byte [ebx + Thread.State], THREAD_RUNNING

     mov       al, [ebx + Thread.Quantum]
     mov       [edx + GS.Quantum], al
     sub       ebx, [Threads.Threads]
     jmp       .EndFountThread
.SkipFound:
     loop      .LoopFountThread
     mov       ebx, [Sched.Idle]
.EndFountThread:

     stdcall   Mutex.Release Threads.Mutex

     mov       [edx + GS.CThread], ebx
     mov       edi, ebx
     add       edi, [Threads.Threads]
     movzx      eax, word[edi + Thread.Pid] 
     mov       [edx + GS.CProcess], eax

     stdcall   Sched.LoadThread edi

     mov       edx, cr3
     add       eax, [Procces.Procces]
     cmp       [eax + Procces.CR3], 0
     jz       .NotNewProcc
     cmp       [eax + Procces.CR3], edx
     je       .NotNewProcc

     stdcall   Sched.SwitchTo [eax + Procces.CR3]

.NotNewProcc:

     mov       ax, 0x38
     mov       gs, ax

     mov       eax, 0x200
     fxrstor   [gs:eax]
     
     mov       eax, 0x1C
     mov       ebp, [gs:eax]
     sub       eax, 4
     mov       esp, [gs:eax]
     sub       eax, 4
     mov       edi, [gs:eax]
     sub       eax, 4
     mov       esi, [gs:eax]
     sub       eax, 4
     mov       ebx, [gs:eax]
     sub       eax, 4
     mov       edx, [gs:eax]
     sub       eax, 4
     mov       ecx, [gs:eax]
     sub       eax, 4
     mov       eax, [gs:eax]
     
     pop       eax
     pop       gs
     iret
endp

proc Sched.Refill uses ecx edx
     mov       ecx, [Sched.Maxthr]
.AddToQueue:
     imul      edx, ecx, ThreadSize  
     add       edx, [Threads.Threads]
     mov       eax, [edx + Threads.State]
     cmp       eax, THREAD_DEAD
     je        .SkipThread
     cmp       eax, THREAD_NEW
     je        .SkipThread
     cmp       eax, THREAD_NONE
     je        .SkipThread
     
     shr       [edx + Threads.State], 1
     mov       eax, [edx + Threads.Priority]
     add       [edx + Threads.State], eax
.SkipThread:
     loop      .AddToQueue
     ret
endp

proc Sched.Init
     ;инициализация прервыаний
     ret
endp

proc Sched.SwitchTo creg
     mov  cr3, [creg]
     ret
endp


proc Sched.LoadThread uses edx, thread 
     mov       edx, [thread]
     mov       [GDT + 0x38 + 2], dl
     mov       [GDT + 0x38 + 3], dh
     shr       edx, 16
     mov       [GDT + 0x38 + 4], dl
     mov       [GDT + 0x38 + 7], dh 
      
     ret 
endp

proc Sched.Check
     stdcall GS.Base
     mov       edx, [eax + GS.CProcess]
     add       edx, [Process.Process]
     cmp       [edx + Process.Status], PROCESS_DEAD 
     je        .Do
     mov       edx, [eax + GS.CThread]
     add       edx, [Threads.Threads]
     test      word[edx + Threads.Killed], 10000000:00000000b 
     je        .Do
     ret
.Do:     
     int 30h
     ret  
endp

}

block(.initData){
     Sched.Idle     dw 0  
     
}

block(.data){
     Sched.Maxthr   dw ?
     Sched.P        dw 4 dup ?
     Sched.End      dw 4 dup ?
}