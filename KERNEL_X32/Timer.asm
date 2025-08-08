include 'macro\proc32.inc'


 ;Input
 ; ebx   Desired PIT frequency in Hz
proc Timer.Init
     pushad
     xchg      bx,bx

     mov       eax, 0x10000 
     cmp       ebx, 18
     jbe       .GotReloadValue

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
     xchg      bx, bx     
     out       0x40, al                      
     mov       al, ah                        
     out       0x40, al

     mov       ax, 0x28
     mov       es, ax
     mov       esi, 0x20 * 8
     mov       eax, Timer.IRQ0Handler
     mov       dx, cs
     call      Interrupt.Make   

     popfd
     popad

     ret
endp

proc Timer.IRQ0Handler
     push      eax ebx

     mov       eax, [Timer.IRQ0Fractions]
     mov       ebx, [Timer.IRQ0Ms]
     add       [Timer.TimerFractions], eax
     adc       [Timer.TimerMs], ebx

     mov       al, 0x20
	out       0x20, al
     pop       ebx eax
     iretd
endp

Timer.TimerFractions     dd 0
Timer.TimerMs            dd 0
Timer.IRQ0Fractions      dd 0
Timer.IRQ0Ms             dd 0
Timer.IRQ0Frequency      dd 0
Timer.PITReloadValue     dw 0
