

block(.text){
proc Mutex.Start uses ebx, mutex 
     mov       ebx, [mutex]
     mov  byte [ebx], 1
     ret
endp

proc Mutex.Stop mutex
     stdcall   Mutex.Wait, [mutex]
endp

proc Mutex.Wait uses ebx, mutex
     mov       ebx, [mutex]
     xor       al, al
@@:
     lock xchg al, [ebx]
     test      al, al
     jz        @B
     ret
endp 

proc Mutex.TryWait uses ebx, mutex
     mov       ebx, [mutex]
     xor       al, al
@@:
     lock xchg al, [ebx]
     ret
endp  

proc Mutex.WaitLong uses ebx, mutex
.Retry:
     mov       ebx, [mutex]
     xor       al, al
     lock xchg al, [ebx]
     test      al, al
     jz        @F
     ret
@@:
     call      Sched.Yield
     jmp       .Retry
endp

proc Mutex.Release uses ebx, mutex 
     mov       ebx, [mutex]
     mov  byte [ebx], 1
     ret
endp
}