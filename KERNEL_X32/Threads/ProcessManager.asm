block(.consts){
    MAX_PID equ 4095
    MAX_TID equ 16383

}

block(.text){

proc ProcessManager.GetCurrentThread uses edx
     stdcall   GS.Base
     xchg      eax, edx
     movzx     eax, [edx + GS.CThread]
     ret
endp

proc ProcessManager.GetProcess uses edx, thread
     mov       eax, [thread]
     imul      edx, eax, ThreadSize   
     add       edx, [Threads.Threads]
     movzx     eax, word[edx + Thread.Pid]   
     ret
endp

proc ProcessManager.GetName uses edx, process
     imul      edx, ProcessSize, [process]  
     add       edx, [Process.Process]
     mov       eax, [edx + Process.Procname]
     ret
endp

proc ProcessManager.Init
     stdcall   Mutex.Start, Threads.Mutex
     stdcall   Mutex.Start, Process.Mutex
     stdcall   Mutex.Start, Process.ProcessMutex

     stdcall   Threads.Create, 0, ProcessManager.Idle
     stdcall   Threads.Create, 0, ProcessManager.Terminator
     ret
endp

proc ProcessManager.Idle
@@:     
     hlt
     jmp @B
endp

proc ProcessManager.Terminator
@@:
    stdcall     Sched.Block
    jmp @B
    ret
endp

}

block(.data){
    ProcessManager.TerminatorThread dw ?
}
