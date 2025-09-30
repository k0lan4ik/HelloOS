block(.consts){
    MAX_PID 4095
    MAX_TID 16383

}

block(.text){

proc ProessManager.GetCurrentThread uses edx
     stdcall   GS.Base
     xchg      eax, edx
     mov       eax, [edx + GS.CThread]
     ret
endp

proc ProcessManager.GetProcess uses edx, thread
     imul      edx, ThreadSize, [thread]  
     add       edx, [Threads.Threads]
     movzx     eax, word[edx + Threads.Pid]   
     ret
endp

proc ProcessManager.GetName uses edx, process
     imul      edx, ProccesSize, [process]  
     add       edx, [Process.Process]
     mov       eax, [edx + Process.Procname]
     ret
endp

proc ProcessManager.Init
     stdcall   Mutex.Start Threads.Mutex
     stdcall   Mutex.Start Process.Mutex
     stdcall   Mutex.Start Threads.Mutex

     stdcall   Threads.CreateThread 0, ProcessManager.Idle
     stdcall   Threads.CreateThread 0, ProcessManager.Terminator
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



block(.data){
    ProcessManager.TerminatorThread dw ?
}
