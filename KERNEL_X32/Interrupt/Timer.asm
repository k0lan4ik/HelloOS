block(.consts){
     virtual at 0
          Timer.TimerEnt.Delay dw ? 
          Timer.TimerEnt.Tread dw ?
          Timer.TimerEnt.Next  dd ?
     end virtual
}

block(.text) {
proc Timer.Handle uses esi edi
     cmp       [Timer.Timers], 0
     jz        .ProcData
     mov       edi, [Timer.Timers]
     dec       [edi + Timer.TimerEnt.Delay]
     cmp       [edi + Timer.TimerEnt.Delay], 0       
     jnz       .ProcData
.FreeLoop:     
     cmp       [edi + Timer.TimerEnt.Delay], 0
     jnz       .ProcData
     mov       esi, edi
     mov       edi, [edi + Timer.TimerEnt.Next]
     stdcall   Sched.Signal, dword[edi + Timer.TimerEnt.Tread]
     stdcall   KernelMemManager.Free, edi
     jmp       .FreeLoop

.ProcData:
     stdcall   GS.Base
     dec       [eax + GS.Quantum]
     cmp       [eax + GS.Quantum], 0
     jnz       .EndProc
     int       30h
.EndProc:
     ret
endp
}

block(.data) {
     Timer.Timers dd ?
}
