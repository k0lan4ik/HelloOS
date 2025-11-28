block(.consts) {
virtual at 0
     HardwInt.Hand.Tid    dw ?
     HardwInt.Hand.Mutex  db ?
     HardwInt.Hand.Next   db ?
     HardwInt.Hand.Size:
end virtual
}

block(.text) {
proc HardwInt.Init uses edi
     mov  ecx, 0x10
     xor  eax, eax
     mov  edi, HardwInt.Hand
     cld
     rep stosd

     stdcall   IntHeand.SetIntSave, 32, HardwInt.0
     stdcall   IntHeand.SetIntSave, 33, HardwInt.1
     stdcall   IntHeand.SetIntSave, 34, HardwInt.2
     stdcall   IntHeand.SetIntSave, 35, HardwInt.3
     stdcall   IntHeand.SetIntSave, 36, HardwInt.4
     stdcall   IntHeand.SetIntSave, 37, HardwInt.5
     stdcall   IntHeand.SetIntSave, 38, HardwInt.6
     stdcall   IntHeand.SetIntSave, 39, HardwInt.7
     stdcall   IntHeand.SetIntSave, 40, HardwInt.8
     stdcall   IntHeand.SetIntSave, 41, HardwInt.9
     stdcall   IntHeand.SetIntSave, 42, HardwInt.10
     stdcall   IntHeand.SetIntSave, 43, HardwInt.11
     stdcall   IntHeand.SetIntSave, 44, HardwInt.12
     stdcall   IntHeand.SetIntSave, 45, HardwInt.13
     stdcall   IntHeand.SetIntSave, 46, HardwInt.14
     stdcall   IntHeand.SetIntSave, 47, HardwInt.15

     stdcall   Mutex.Start, HardwInt.Handler.Mutex

     ret
endp 

proc HardwInt.WhaitForInt, hwint 
     stdcall HardwInt.RegInt, hwint
     stdcall Sched.Block
     ret
endp

proc HardwInt.RegInt, hwint
     stdcall   Mutex.Wait, HardwInt.Handler.Mutex
     mov       eax, [hwint]
     mov       edx, HardwInt.Hand
@@:
     cmp       [eax * 4 + edx + HardwInt.Hand.Next], 0
     jz        .EndList
     movzx      eax, [eax * 4 + edx + HardwInt.Hand.Next]
     jmp       @B
.EndList:

     mov       ecx, 15
@@:    
     inc       ecx
     cmp       [ecx * 4 + edx + HardwInt.Hand.Mutex], 1
     je        @B

     test      eax, [hwint]
     jnz       @F
     cmp       [eax * 4 + edx + HardwInt.Hand.Mutex], 0
     jne       @F
     xchg      eax, ecx
     jmp       .EndWrite
@@:
     mov       [eax * 4 + edx + HardwInt.Hand.Next], cl

.EndWrite:
     push      ecx
     stdcall   ProcessManager.GetCurrentThread
     pop       ecx

     mov       edx, HardwInt.Hand
     mov       [ecx * 4 + edx + HardwInt.Hand.Tid], ax
     mov       [ecx * 4 + edx + HardwInt.Hand.Mutex], 1
     mov       [ecx * 4 + edx + HardwInt.Hand.Next], 0

     stdcall   Mutex.Release, HardwInt.Handler.Mutex

     ret
endp

proc HardwInt.Fini
     stdcall   Mutex.Stop, HardwInt.Handler.Mutex

     mov       edx, 32
     mov       ecx, 48 - 32
@@:
     push      edx, ecx
     stdcall   IntHeand.SetIntSave,edx ,0
     pop       ecx, edx
     inc       edx
     loop @B

     cli
     ret
endp

HardwInt.0:
     push      0
     jmp       HardwInt.GenHandler

HardwInt.1:
     push      1
     jmp       HardwInt.GenHandler

HardwInt.2:
     push      2
     jmp       HardwInt.GenHandler

HardwInt.3:
     push      3
     jmp       HardwInt.GenHandler

HardwInt.4:
     push      4
     jmp       HardwInt.GenHandler

HardwInt.5:
     push      5
     jmp       HardwInt.GenHandler

HardwInt.6:
     push      6
     jmp       HardwInt.GenHandler

HardwInt.7:
     push      7
     jmp       HardwInt.GenHandler

HardwInt.8:
     push      8
     jmp       HardwInt.GenHandler

HardwInt.9:
     push      9
     jmp       HardwInt.GenHandler

HardwInt.10:
     push      10
     jmp       HardwInt.GenHandler

HardwInt.11:
     push      11
     jmp       HardwInt.GenHandler

HardwInt.12:
     push      12
     jmp       HardwInt.GenHandler

HardwInt.13:
     push      13
     jmp       HardwInt.GenHandler

HardwInt.14:
     push      14
     jmp       HardwInt.GenHandler

HardwInt.15:
     push      15
     jmp       HardwInt.GenHandler

HardwInt.GenHandler:
     pushf
     pusha
     push      ds es fs gs ss ax

     mov       ebp, esp
     mov       eax, [ebp + 48]

     stdcall   Mutex.Wait, HardwInt.Handler.Mutex
     
     mov       edx, HardwInt.Hand
     cmp       [eax + edx + HardwInt.Hand.Mutex], 1
     jnz       @F
     mov       [eax + edx + HardwInt.Hand.Mutex], 0
     push      eax
     stdcall   Sched.Signal, dword[eax + edx + HardwInt.Hand.Tid]
     pop       eax
@@:
     cmp       [eax + edx + HardwInt.Hand.Next], 0
     jz        .EndLoop
     movzx     eax, [eax + edx + HardwInt.Hand.Next]
     mov       [eax + edx + HardwInt.Hand.Mutex], 0
     push      eax
     stdcall   Sched.Signal, dword[eax + edx + HardwInt.Hand.Tid]
     pop       eax
     jmp       @B
.EndLoop:

     push      eax
     stdcall   Mutex.Release, HardwInt.Handler.Mutex
     push      eax
     
     test      eax, eax
     jnz       @F
     stdcall   Timer.Handle
@@:

     pop       ax ss gs fs es ds
     popa
     popf
     add       esp, 4
     iret

}




block(.data) {
    HardwInt.Hand       db (HardwInt.Hand.Size * 64) dup ? 
    HardwInt.Handler.Mutex db ?   
}

