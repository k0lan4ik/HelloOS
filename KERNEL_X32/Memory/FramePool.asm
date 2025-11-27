block(.consts){
E820.Count equ 0xC0000600
E820.Entry equ 0xC0000604
virtual at E820.Entry
     E820.Entry.Start    dq ?
     E820.Entry.Length   dq ?
     E820.Entry.Type     dd ?
     E820.Entry.Pad      dd ?
end virtual
}

block(.text){
proc FramePool.Init1 
     mov       ecx, 2047
     mov       eax, 0x100
     mov       edi, FramePool.ZeroTable 
@@:
     stosd
     inc       eax
     loop      @B
     xor       ax, ax
     
     mov       [FramePool.ZeroRead], ax
     mov       [FramePool.ZeroWrite], 2047

     mov       [FramePool.FreeRead], ax
     mov       [FramePool.FreeWrite], ax

     stdcall   Mutex.Start, FramePool.ZeroMutex
     stdcall   Mutex.Start, FramePool.FreeMutex
     ret
endp

proc FramePool.Init2
     locals
          cpr dw ?
     endl
     mov       [FramePool.Backlog], 0x1000
     mov       [FramePool.BacklogPointer], 0

     stdcall   Mutex.Start, FramePool.BacklogMutex

     stdcall   Process.Create
     mov       [cpr], ax

     stdcall   Threads.Create, dword[cpr], FramePool.ZeroPageThread
     mov       [FramePool.ZeroThread], ax

     stdcall   Threads.Create, dword[cpr], FramePool.FreePageThread
     mov       [FramePool.FreeThread], ax
     
     stdcall    Threads.Create, dword [cpr] FramePool.InitialFiller
     ret
endp

proc FramePool.GetFreePage
     movzx     eax, [FramePool.ZeroWrite]
     sub       ax, [FramePool.ZeroRead]
     xor       edx, edx
     mov       ecx, 2048
     div       ecx
     cmp       edx, 128
     jg        @F
     stdcall   Sched.Signal, dword[FramePool.ZeroThread]
@@:   
     stdcall   Mutex.WaitLong, FramePool.ZeroMutex

@@:
     movzx     eax, [FramePool.ZeroWrite]
     sub       ax, [FramePool.ZeroRead]
     xor       edx, edx
     mov       ecx, 2048
     div       ecx
     test      edx, edx
     jnz       .EndYeld
     stdcall   Sched.Yield, dword[FramePool.ZeroThread]
     jmp       @B
.EndYeld:
     movzx     eax, [FramePool.ZeroRead]
     inc       [FramePool.ZeroRead]
     shl       eax, 2
     add       eax, FramePool.ZeroTable  
     push dword [eax]

     stdcall   Mutex.Release, FramePool.ZeroMutex 

     pop       eax
     ret
endp

proc FramePool.FreePage phys
     movzx     eax, [FramePool.FreeWrite]
     sub       ax, [FramePool.FreeRead]
     xor       edx, edx
     mov       ecx, 2048
     div       ecx
     cmp       edx, 1920
     jg        @F
     stdcall   Sched.Signal, dword[FramePool.FreeThread]
@@:    
     stdcall   Mutex.WaitLong, FramePool.FreeMutex

@@:
     movzx     eax, [FramePool.FreeWrite]
     sub       ax, [FramePool.FreeRead]
     xor       edx, edx
     mov       ecx, 2048
     div       ecx
     cmp       edx, 2047
     jne       .EndYeld
     stdcall   Sched.Yield, dword[FramePool.FreeThread]
     jmp       @B
.EndYeld:     

     movzx     eax, [FramePool.FreeWrite]
     inc       [FramePool.FreeWrite]     
     mov       edx, [phys]
     shl       eax, 2
     add       eax, FramePool.FreeTable 
     mov       [eax], edx

     stdcall   Mutex.Release, FramePool.FreeMutex
     ret
endp

proc FramePool.InitialFiller uses ebx
     KernelPages = 32
     mov       ecx, [E820.Count]
     xor       ebx, ebx
.MemLoop:
     cmp       [E820.Entry.Type + ebx], 1
     jne       .NotPage
     mov       eax, dword[E820.Entry.Start + ebx]
     add       eax, 0xFFF         
     and       eax, 0xFFFFF000   
     shr       eax, 12   
     push      ecx   
     mov       ecx, dword[E820.Entry.Start + ebx]
     add       ecx, dword[E820.Entry.Length + ebx]
     add       ecx, 0xFFF         
     and       ecx, 0xFFFFF000   
     shr       ecx, 12    
     sub       ecx, eax 
.FreePLoop:
     mov       edx, eax
     add       edx, ecx
     dec       edx
     cmp       edx, KernelPages
     jl        @F
     push      ecx
     stdcall   FramePool.FreePage, edx
     pop       ecx          
@@:
     loop      .FreePLoop
     pop       ecx
.NotPage:
     add       ebx, 24
     loop      .MemLoop

     stdcall   ProcessManager.GetCurrentThread
     stdcall   Threads.Kill, eax
endp

proc FramePool.FreePageThread
.InfLoop:
.MapLoop:
     movzx     eax, [FramePool.FreeWrite]
     sub       ax, [FramePool.FreeRead]
     xor       edx, edx
     mov       ecx, 2048
     div       ecx
     cmp       edx, 16
     jle       .EndMap
     
     stdcall   Mutex.Wait, FramePool.BacklogMutex
     
     mov       ecx, 16
@@:
     movzx     edx, [FramePool.FreeRead]
     inc       [FramePool.FreeRead]
     mov       ebx, [FramePool.FreeTable + edx]  

     cmp       [FramePool.BacklogPointer], 0x4000
     jge       .Skip
     push      ecx
     stdcall   Pager.MapPage, [FramePool.BacklogPointer], ebx, AL_FL_WRITABLE
     pop       ecx
.Skip:    
     mov       eax, [FramePool.Backlog]
     mov       edx, [FramePool.BacklogPointer]
     shl       edx, 2
     add       eax, edx
     inc       [FramePool.BacklogPointer]

     mov       [eax], ebx

     loop      @B
     stdcall   Mutex.Release, FramePool.BacklogMutex
     jmp       .MapLoop
.EndMap:
     stdcall   Sched.Block
     jmp       .InfLoop
endp

proc FramePool.ZeroPageThread
     locals
          phys dd 16 dup ?
     endl
.InfLoop:
.MapLoop:
     movzx     eax, [FramePool.ZeroWrite]
     sub       ax, [FramePool.ZeroRead]
     xor       edx, edx
     mov       ecx, 2048
     div       ecx
     cmp       edx, 2032
     jge       .EndMap
     
     cmp       [FramePool.BacklogPointer], 16
     jl        .MapLoop

     stdcall   Mutex.Wait, FramePool.BacklogMutex

     mov       ecx, 16
@@:
     dec       [FramePool.BacklogPointer]
     mov       eax, [FramePool.BacklogPointer]
     shl       eax, 2
     add       eax, [FramePool.Backlog]
     mov       edx, [eax]
     neg       ecx
     mov       [phys + 16 + ecx], edx
     neg       ecx

     cmp       [FramePool.BacklogPointer], 0x4000
     jge       .Skip
     push      ecx
     stdcall   Pager.Unmap,    [FramePool.BacklogPointer]
     pop       ecx
.Skip:
     loop      @B
     stdcall   Mutex.Release, FramePool.BacklogMutex

     mov       ecx, 16
     mov       eax, 0x10000
@@: 
     push      ecx
     neg       ecx
     stdcall   Pager.MapPage, eax, [phys + 16 + ecx], AL_FL_WRITABLE
     pop       ecx
     inc       eax
     loop      @B     

     mov       edi, 0x10000000
     mov       ecx, 0x10000 / 8
     xor       eax, eax
     rep stosd

     mov       ecx, 16
     movzx     ebx, [FramePool.ZeroWrite]
@@: 
     mov       eax, 0x10000+16 
     sub       eax, ecx
     push      ecx
     stdcall   Pager.Unmap, eax
     pop       ecx
     mov       [FramePool.ZeroTable + ebx], eax
     inc       ebx
     loop      @B      
     mov       [FramePool.ZeroWrite], bx

     jmp       .MapLoop
.EndMap:
     stdcall   Sched.Block
     jmp       .InfLoop  
endp
}

block(.data){
    FramePool.ZeroTable         dd 2048 dup ?
    FramePool.FreeTable         dd 2048 dup ?  
    FramePool.BacklogPointer    dd ?
    FramePool.Backlog           dd ?
    FramePool.ZeroThread        dw ?
    FramePool.FreeThread        dw ?
    FramePool.ZeroWrite         dw ?
    FramePool.ZeroRead          dw ?
    FramePool.FreeWrite         dw ?
    FramePool.FreeRead          dw ?
    FramePool.ZeroMutex         db ?
    FramePool.BacklogMutex      db ?
    FramePool.FreeMutex         db ?
}