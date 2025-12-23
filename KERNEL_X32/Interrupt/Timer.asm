block(.consts){
     virtual at 0
          Timer.TimerEnt.Delay dw ? 
          Timer.TimerEnt.Tread dw ?
          Timer.TimerEnt.Next  dd ?
          Timer.TimerEnt.Size:
     end virtual

     PIT_MODE_INTERRUPT_ON_TERMINAL_COUNT = 0
     PIT_MODE_ONE_SHOT                    = 1
     PIT_MODE_RATE_GENERATOR              = 2 
     PIT_MODE_SQUARE_WAVE                 = 3
}

block(.text) {
proc Timer.TimerInit uses ebx, Hz
     
     mov [Timer.Timers], 0
     stdcall Mutex.Start, Timer.Mutex

     mov       ebx, [Hz]

     mov       eax, 0x10000 
     cmp       ebx, 18
     jbe       .GotReloadValue

     mov       eax,2                        
     cmp       ebx,1193181             
     jae       .GotReloadValue

     mov       eax, 3579545
     xor       edx, edx
     div       ebx
     cmp       edx, 3579545 / 2
     jb        @F
     inc       eax 
@@:   
     mov       ebx, 3
     xor       edx, edx    
     div       ebx
     cmp       edx, 3 / 2
     jb        @F
     inc       eax
@@:     

.GotReloadValue:
     push      eax
     mov       [Timer.PITReloadValue], ax
     mov       ebx, eax

     mov       eax, 3579545
     xor       edx, edx
     div       ebx      
     cmp       edx, 3579545 / 2 
     jb        @F
     inc       eax
@@:
     mov       ebx, 3
     xor       edx, edx                        
     div       ebx                          
     cmp       edx, 3 / 2                   
     jb        @F                            
     inc       eax                            
@@:
     mov       [Timer.IRQ0Frequency], eax


     pop       ebx
     mov       eax, (3000 shl 42) / 3579545
     mul       ebx 
     shrd      eax, edx, 10
     shr       edx, 10

     mov       [Timer.IRQ0Ms], edx
     mov       [Timer.IRQ0Fractions], eax

     pushfd
     cli

     mov       al,00110100b       ;channel 0, lobyte/hibyte, rate generator
     out       0x43, al

     mov       ax, [Timer.PITReloadValue]      
     out       0x40, al                      
     mov       al, ah                        
     out       0x40, al
     popfd
     ret
endp


proc Timer.Handle uses esi edi ebx
     
     mov       eax, [Timer.IRQ0Fractions]
     mov       ebx, [Timer.IRQ0Ms]
     add       [Timer.TimerFractions], eax
     adc       ebx, 0
     add       [Timer.TimerMs], ebx  
    
     cmp       [Timer.Timers], 0
     jz        .ProcData
     stdcall   Mutex.TryWait, Timer.Mutex
     jnz       .ProcData
     mov       edi, [Timer.Timers]
     xor       edx, edx
.ProcessTimerList: 
     test      edi, edi
     jz        .EndTimerList
     sub       [edi + Timer.TimerEnt.Delay], bx
     jg        .NextTimer
     
     mov       esi, edi
     mov       edi, [edi + Timer.TimerEnt.Next]
     test      edx, edx
     jz        @F
     mov [edx + Timer.TimerEnt.Next], edi
     jmp       .Del
@@:
     mov [Timer.Timers], edi
.Del:
     STOP_POINT
     movzx eax, word[esi + Timer.TimerEnt.Tread]
     stdcall Sched.Signal, eax

     stdcall KernelMemManager.Free, esi
    
     jmp .ProcessTimerList
    
.NextTimer:
     mov edx, edi
     mov edi, [edi + Timer.TimerEnt.Next]
     jmp .ProcessTimerList
     
.EndTimerList:
     stdcall   Mutex.Release, Timer.Mutex
     
.ProcData:
     stdcall   GS.Base
     cmp       [eax + GS.Quantum], 0
     jnz       @F
     STOP_POINT
@@:     
     dec       [eax + GS.Quantum]
     cmp       [eax + GS.Quantum], 0
     ;jnz       .EndProc
     int       30h
.EndProc:
     ret
endp

proc Timer.RegisterDelay uses esi, thread:WORD, delay:WORD
     stdcall KernelMemManager.Malloc, Timer.TimerEnt.Size
     ;STOP_POINT
     mov esi, eax
     mov ax, [thread]
     mov [esi + Timer.TimerEnt.Tread], ax
     mov ax, [delay]
     mov [esi + Timer.TimerEnt.Delay], ax
     
     stdcall Mutex.Wait, Timer.Mutex
     mov eax, [Timer.Timers]
     mov [esi + Timer.TimerEnt.Next], eax
     mov [Timer.Timers], esi
     stdcall Mutex.Release, Timer.Mutex
 
     ret
endp

proc Timer.Sleep, milliseconds
    stdcall ProcessManager.GetCurrentThread
    stdcall Timer.RegisterDelay, eax, [milliseconds]
    stdcall Sched.Block
    ret
endp

proc Timer.GetTimeMs 
     mov       eax, [Timer.TimerMs]
     ret
endp


}

block(.data) {
     Timer.Timers dd ?
     Timer.Mutex db ?

     Timer.TimerFractions     dd ?
     Timer.TimerMs            dd ?
     Timer.IRQ0Fractions      dd ?
     Timer.IRQ0Ms             dd ?
     Timer.IRQ0Frequency      dd ?
     Timer.PITReloadValue     dw ?
}

