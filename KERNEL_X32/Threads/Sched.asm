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

block(.text){

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


     stdcall   Mutex.Wait, Threads.Mutex

     stdcall   GS.Base
     mov       edx, eax

     imul      ebx, eax, ThreadSize
     add       ebx, [Threads.Threads]

     test      dword [ebx + Thread.Killed], 10000000_00000000b
     jnz       .EndIfKill
     cmp       byte  [ebx + Thread.State], THREAD_RUNNING
     jnz       .EndIfKill

     mov       byte [ebx + Thread.State], THREAD_AVAILABLE
     mov       al, [edx + GS.Quantum]
     mov       byte [ebx + Thread.Quantum], al

     movzx     ecx, word[ebx + Thread.Type]
     and        cl, 01110000b
     shr       cx, 12

     mov       edi, Sched.End
     movzx     edi, word[edi + ecx]
     test      edi, edi 
     jz        .ElseIfEnd

     mov       esi, edi
     add       esi, [Threads.Threads]  
     mov       ax, [edx + GS.CThread] 
     mov       [esi + Thread.Next], ax
     mov       [ebx + Thread.Previous], di
     mov       word [ebx + Thread.Next], 0

     mov       ax, [edx + GS.CThread]
     mov       edi, Sched.End
     mov       [edi + ecx], ax

.ElseIfEnd:
     mov       ax, [edx + GS.CThread]
      
     mov       edi, Sched.P
     mov       [edi + ecx], ax
     mov       edi, Sched.End
     mov       [edi + ecx], ax
      
     mov       word [ebx + Thread.Next], 0
     mov       word [ebx + Thread.Previous], 0

.EndIfKill:

     mov       ecx, 4
.LoopIfZero:  
     mov       edi, Sched.P
     movzx     ebx, word[edi + ecx * 2 - 2]
     add       ebx, [Threads.Threads]
     cmp       [ebx + Thread.Quantum], 0
     jne       .NotZero
     loop      .LoopIfZero
     stdcall   Sched.Refill
.NotZero:

     mov       ecx, 4

.LoopFountThread:
     mov       edi, Sched.P
     movzx     ebx, word[edi + ecx * 2 - 2]
     test      ebx, ebx
     je        .SkipFound
     add       ebx, [Threads.Threads]
     cmp       [ebx + Thread.Quantum], 0
     jbe       .SkipFound

     mov       ax, [ebx + Thread.Next]
     mov       edi, Sched.P
     mov       [edi + ecx * 2 - 2], ax
     mov       word [eax + Thread.Previous], 0
     mov       word [ebx + Thread.Next], 0
     mov       byte [ebx + Thread.State], THREAD_RUNNING

     mov       al, [ebx + Thread.Quantum]
     mov       [edx + GS.Quantum], al
     sub       ebx, [Threads.Threads]
     jmp       .EndFountThread
.SkipFound:
     loop      .LoopFountThread
     movzx     ebx, [Sched.Idle]
.EndFountThread:

     stdcall   Mutex.Release, Threads.Mutex

     mov       [edx + GS.CThread], bx
     mov       edi, ebx
     add       edi, [Threads.Threads]
     movzx      eax, word[edi + Thread.Pid] 
     mov       [edx + GS.CProcess], ax

     stdcall   Sched.LoadThread, edi

     mov       edx, cr3
     add       eax, [Process.Process]
     cmp       [eax + Process.CR3], 0
     jz       .NotNewProcc
     cmp       [eax + Process.CR3], edx
     je       .NotNewProcc

     stdcall   Sched.SwitchTo, [eax + Process.CR3]

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
     movzx     ecx, [Sched.Maxthr]
.AddToQueue:
     imul      edx, ecx, ThreadSize  
     add       edx, [Threads.Threads]
     movzx     eax, [edx + Thread.State]
     cmp       eax, THREAD_DEAD
     je        .SkipThread
     cmp       eax, THREAD_NEW
     je        .SkipThread
     cmp       eax, THREAD_NONE
     je        .SkipThread
     
     shr       [edx + Thread.State], 1
     mov       al, [edx + Thread.Priority]
     add       [edx + Thread.State], al
.SkipThread:
     loop      .AddToQueue
     ret
endp

proc Sched.Init
     stdcall IntHeand.SetIntSave, 0x30, Sched.HandlerInt
	stdcall IntHeand.SetIntSave, 0x31, Sched.Check
     ret
endp

proc Sched.SwitchTo creg
     mov  eax, [creg] 
     mov  cr3, eax
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
     movzx     edx, [eax + GS.CProcess]
     add       edx, [Process.Process]
     cmp       [edx + Process.Status], PROCESS_DEAD 
     je        .Do
     movzx     edx, [eax + GS.CThread]
     add       edx, [Threads.Threads]
     test      word[edx + Thread.Killed], 10000000_00000000b 
     je        .Do
     ret
.Do:     
     int 30h
     ret  
endp


proc Sched.Signal uses edi, thread:WORD
     stdcall   Mutex.Wait, Threads.Mutex
        
     movzx     eax, [thread]
     mov       edx,  ThreadSize
     mul       edx
     add       eax, [Threads.Threads] 


     cmp       [Thread.State + eax], THREAD_BLOCKED
     jne       @F

     mov       [Thread.State + eax], THREAD_AVAILABLE
     mov       edx, [eax + Thread.Type] 
     and       edx, 01110000_00000000b
     shr       edx, 12  
     mov       edi, Sched.P 
     movzx     ecx, word [edi + edx * 2]
     mov       [Thread.Next + eax], cx
     imul      ecx, ecx, ThreadSize
     add       ecx, [Threads.Threads]
     mov       ax, [thread]     
     mov       [ecx + Thread.Previous], ax
     mov       [Sched.P + edi * 2], ax
     jmp       .EndIF
@@:
     cmp       [Thread.State + eax], THREAD_SWAPPEDBLOCKED
     jne       @F
     mov       [Thread.State + eax], THREAD_SWAPPED
     jmp       .EndIF
@@:
     or       word[Thread.SignalWaiting + eax], 00001000_00000000b

.EndIF:
     

     stdcall   Mutex.Release, Threads.Mutex
     ret
endp

proc Sched.Block uses ebx
     stdcall   GS.Base
     xchg      eax, ebx
     stdcall   Mutex.Wait, Threads.Mutex
     movzx     eax, [ebx + GS.CThread]
     mov       ecx, ThreadSize
     mul       ecx
     add       eax, [Threads.Threads]
     test      word [eax + Thread.SignalWaiting], 00001000_00000000b
     jnz       .Else

     mov       byte [eax + Thread.State], THREAD_BLOCKED

     stdcall   Mutex.Release, Threads.Mutex
     
     int       30h

     jmp       .EndProc
 .Else: 

     or         word [eax + Thread.SignalWaiting], 00001000_00000000b
     
     stdcall   Mutex.Release, Threads.Mutex

.EndProc:
     ret
endp

proc Sched.Yield uses ebx edi, thread
     
     cmp       [thread], 0
     jz        .EndProc
     mov       eax, [thread]
     mov       ecx, ThreadSize
     mul       ecx
     mov       ebx, [Threads.Threads]
     add       ebx, eax
     cmp       [ebx + Thread.State], SCHED_AVAILABLE
     jne       .EndProc
     cmp       [ebx + Thread.Quantum], 0
     jbe       .EndProc

     stdcall   Mutex.Wait, Threads.Mutex
     cmp       [ebx + Thread.State], SCHED_AVAILABLE
     jne       .Release
     cmp       [ebx + Thread.Quantum], 0
     jbe       .Release     

     cmp       [ebx + Thread.Next], 0
     jnz       @F
     mov       ax, [ebx + Thread.Next]
     mov       ecx, ThreadSize
     mul       ecx
     mov       edx, [Threads.Threads]
     add       edx, eax
     mov       ax, [ebx + Thread.Previous]
     mov       [edx + Thread.Previous], ax
@@:      

     cmp       [ebx + Thread.Previous], 0
     jnz       @F
     mov       ax, [ebx + Thread.Previous]
     mov       ecx, ThreadSize
     mul       ecx
     mov       edx, [Threads.Threads]
     add       edx, eax
     mov       ax, [ebx + Thread.Next]
     mov       [edx + Thread.Next], ax
@@:      

     movzx     eax, word [ebx + Thread.Type]
     and       ah, 01110000b
     shr       ax, 12
     mov       edi, Sched.P
     movzx     edx, word [edi + eax * 2]
     mov       ecx, [thread]
     mov       [edi + eax * 2], cx
     mov       word [edx + Thread.Next], dx
     mov       word [edx + Thread.Previous], 0
    
     xchg      eax, edx
     mov       edx, ThreadSize
     mul       edx
     mov       ebx, [Threads.Threads]
     add       ebx, eax

     mov       word [edx + Thread.Previous], cx

.Release:     
     stdcall   Mutex.Release, Threads.Mutex

.EndProc:
     int       30h
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