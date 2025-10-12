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
proc Process.Create
PT.Old = 0xFFFFF000
PT.New = 0xFF300000



.EndProc:     
     ret
endp


proc Process.Kill process
     stdcall   Mutex.Wait Process.Mutex

     imul      eax, ProcessSize, [process]
     add       eax, [Process.Process]

     cmp       [eax + Process.Status], PROCESS_ACTIVE
     je        @F
     stdcall   Mutex.Relase Process.Mutex
     jmp       .EndProc
 @@:    
     mov       [eax + Process.Status], PROCESS_DEAD
     stdcall   Mutex.Relase Process.Mutex

     int       31h
     stdcall   Sched.Signal ProcessManager.TerminatorThread 

.EndProc:     
     ret
endp

}

block(.initData){
     Process.Process dd 0xFF102000
}

block(.data){
     Process.Mutex db ?
}