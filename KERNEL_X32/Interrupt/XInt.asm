block(.consts) {
    
}

block(.text) {

proc XInt.Init
     stdcall   GS.Base
     
     add       eax, GS.XhTable
     mov       [XInt.ExceptHandler], eax
     PRINT_STOP
     mov       ecx, 32
@@:
     mov       dword[ecx * 4 + eax-4], XInt.Empty
     loop      @B
     
     stdcall   IntHeand.SetIntSave, 0, XInt.Ex0
     stdcall   IntHeand.SetIntSave, 1, XInt.Ex1
     stdcall   IntHeand.SetIntSave, 2, XInt.Nmi
     stdcall   IntHeand.SetIntSave, 3, XInt.Ex3
     stdcall   IntHeand.SetIntSave, 4, XInt.Ex4
     stdcall   IntHeand.SetIntSave, 5, XInt.Ex5
     stdcall   IntHeand.SetIntSave, 6, XInt.Ex6
     stdcall   IntHeand.SetIntSave, 7, XInt.Ex7
     stdcall   IntHeand.SetIntSave, 8, XInt.Ex8
     stdcall   IntHeand.SetIntSave, 10, XInt.Ex10
     stdcall   IntHeand.SetIntSave, 11, XInt.Ex11
     stdcall   IntHeand.SetIntSave, 12, XInt.Ex12
     stdcall   IntHeand.SetIntSave, 13, XInt.Ex13
     stdcall   IntHeand.SetIntSave, 14, XInt.Ex14
     stdcall   IntHeand.SetIntSave, 16, XInt.Ex16
     stdcall   IntHeand.SetIntSave, 17, XInt.Ex17
     stdcall   IntHeand.SetIntSave, 18, XInt.Ex18
     stdcall   IntHeand.SetIntSave, 19, XInt.Ex19

    

     stdcall   XInt.Register, 0, XInt.DoDivideError
     stdcall   XInt.Register, 6, XInt.DoInvalidOpcode
     stdcall   XInt.Register, 7, XInt.DoDeviceNotAvailable
     stdcall   XInt.Register, 12, XInt.DoStackError
     stdcall   XInt.Register, 13, XInt.DoGeneralProtection
     stdcall   XInt.Register, 14, Pager.HandlePF
     stdcall   XInt.Register, 16, XInt.DoFpeOutstanding
     stdcall   XInt.Register, 17, XInt.DoAlignmentCheck

     stdcall   XInt.Register, 1, XInt.DoDebug
     stdcall   XInt.Register, 3, XInt.DoBreakpoint
     stdcall   XInt.Register, 4, XInt.DoOverflow
     stdcall   XInt.Register, 5, XInt.DoBoundRange
     stdcall   XInt.Register, 19, XInt.DoSimdFault
     ret
endp

proc XInt.Register, exception, handler
     cmp       [handler], 0
     jz        @F
     mov       eax, [exception]
     mov       edx, [XInt.ExceptHandler]
     mov       ecx, [handler]
     mov       [edx + eax * 4], ecx
     jmp       .EndProc
@@:    
     mov       eax, [exception]
     mov       edx, [XInt.ExceptHandler]
     mov       dword[edx + eax * 4], XInt.Empty
.EndProc:
     ret
endp


proc XInt.Empty, error, eip
     STOP_POINT
     ret
endp

proc XInt.DoDebug, error, eip
     STOP_POINT
     ret
endp

proc XInt.DoBreakpoint, error, eip
     ret
endp

proc XInt.DoOverflow, error, eip
     ret
endp

proc XInt.DoBoundRange, error, eip
     ret
endp


proc XInt.DoSimdFault, error, eip
     ret
endp

proc XInt.DoInvalidOpcode, error, eip
     STOP_POINT
     stdcall ProcessManager.GetCurrentThread
     stdcall ProcessManager.GetProcess, eax
     stdcall Process.Kill, eax
     ret
endp

proc XInt.DoDeviceNotAvailable, error, eip
     STOP_POINT
     ret
endp

proc XInt.DoGeneralProtection, error, eip
     STOP_POINT
     stdcall ProcessManager.GetCurrentThread
     stdcall ProcessManager.GetProcess, eax
     stdcall Process.Kill, eax
     ret
endp

proc XInt.DoDivideError, error, eip
     STOP_POINT
     ret
endp


proc XInt.DoStackError, error, eip
     STOP_POINT
     ret
endp

proc XInt.DoFpeOutstanding, error, eip
     STOP_POINT
     ret
endp

proc XInt.DoAlignmentCheck, error, eip
     STOP_POINT
     ret
endp

proc XInt.CpuCrash
     STOP_POINT
     ret
endp

proc XInt.DoNmi
     ret
endp

XInt.Ex0:
     push      0
     STOP_POINT
     push      0
     jmp       XInt.Normal

XInt.Ex3:
     push      0
     STOP_POINT
     push      3
     jmp       XInt.Normal

XInt.Ex4:
     push      0
     STOP_POINT
     push      4
     jmp       XInt.Normal

XInt.Ex5:
     push      0
     STOP_POINT
     push      5
     jmp       XInt.Normal

XInt.Ex6:
     push      0
     STOP_POINT
     push      6
     jmp       XInt.Normal

XInt.Ex7:
     push      0
     STOP_POINT
     push      7
     jmp       XInt.Normal

XInt.Ex13:
STOP_POINT
     push      13
     jmp       XInt.Normal

XInt.Ex14:
STOP_POINT
     push      14
     jmp       XInt.Normal

XInt.Ex17:
STOP_POINT
     push      17
     jmp       XInt.Normal

XInt.Ex19:
     push      0
     STOP_POINT
     push      19
     jmp       XInt.Normal

XInt.Normal:
     pushf
     pusha
     pushw      ds es fs gs ss ax
     ;STOP_POINT
     mov       ebp, esp
     mov       eax, [ebp + 48]
     mov       ebx, [XInt.ExceptHandler]
     mov       ebx, [ebx + eax * 4]
     mov       eax, [ebp + 56]
     push      eax
     mov       eax, [ebp + 52]
     push      eax
     call      ebx

     popw       ax ss gs fs es ds
     popa
     popf
     add       esp, 8
     iret

XInt.Ex8:
     mov       eax, 8
     jmp       XInt.AbortToOS

XInt.Ex10:
     mov       eax, 10
     jmp       XInt.AbortToOS

XInt.Ex11:
     mov       eax, 11
     jmp       XInt.AbortToOS

XInt.Ex12:
     mov       eax, 12
     jmp       XInt.AbortToOS

XInt.Ex16:
     mov       eax, 16
     jmp       XInt.AbortToOS

XInt.Ex18:
     mov       eax, 18
     jmp       XInt.AbortToOS

XInt.AbortToOS:
     cmp       eax, 16
     je        XInt.HandleNormal
     cmp       eax, 12
     je        XInt.HandleNormal
     jmp       XInt.CpuCrash

XInt.HandleNormal:
    mov        ebx, [XInt.ExceptHandler]
    mov        ebx, [ebx + eax * 4]
    jmp        ebx

XInt.Nmi:

    pushf
    pusha 
    push ds es fs gs ss ax

    call  XInt.DoNmi

    pop ax ss gs fs es ds
    popa
    popf
    add esp, 4
    iret

XInt.Ex1:
     STOP_POINT
    jmp XInt.Ex1

}

block(.data) {
    
    XInt.ExceptHandler        dd ?    
}

