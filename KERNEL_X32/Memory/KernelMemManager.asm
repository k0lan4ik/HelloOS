block(.structs){
virtual at 0
    MemBlock.Size   dd ?
    Memblock.Offset dd ?
end virtual
}

block(.text){
proc KernelMemManager uses ebx
     add       [Kernel.MaxMem], 16
     mov       ebx, [Kernel.MaxMem]
     add       ebx, 4095
     shr       ebx, 12

     mov       ecx, 8
@@:
     push      ecx
     stdcall   FramePool.GetFreePage
     stdcall   Pager.MapPage ebx, eax, AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     pop       ecx
     inc       ebx
     loop      ecx

     mov       edx, [Kernel.MaxMem]
     mov       [KernelMemManager.FreeList], edx
     add       edx, 8 * 128
     mov       [KernelMemManager.UsedList], edx

     mov       eax, 128
     mov       [KernelMemManager.FreeSize], eax
     mov       [KernelMemManager.UsedSize], eax

     xor       eax, eax
     inc       eax
     mov       [KernelMemManager.FreeCount], eax
     inc       eax
     mov       [KernelMemManager.UsedCount], eax

     add       [Kernel.MaxMem], 32768

     mov       eax, [KernelMemManager.FreeList]
     mov       [eax + Memblock.Offset], eax
     add       [eax + Memblock.Offset], 8 + 256

     mov       ecx, [Kernel.MaxMem]
     sub       ecx, [eax + Memblock.Offset]
     mov       [eax + MemBlock.Size], ecx

     mov       ecx, [KernelMemManager.FreeSize]
     shl       ecx, 3
     mov       [edx + MemBlock.Size], ecx
     mov       [edx + Memblock.Offset], eax

     mov       [edx + 1 + MemBlock.Size], ecx
     mov       [edx + 1 + Memblock.Offset], edx

     xor       eax, eax
     mov       [KernelMemManager.CurIndex], eax

     stdcall   Mutex.Start KernelMemManager.Mutex
     ret
endp    

proc KernelMemManager.Malloc amount
     stdcall   Mutex.Wait KernelMemManager.Mutex
     stdcall   KernelMemManager.RMalloc [amout]
     push      eax
     stdcall   Mutex.Release KernelMemManager.Mutex
     pop       eax
     ret
endp

proc KernelMemManager.RMalloc amount uses ebx edi
     add       [amout], 15
     and       [amout], not 15
     
     mov       edx, -1
     mov       eax, [KernelMemManager.CurIndex]
     mov       ecx, [KernelMemManager.FreeCount]
     sub       ecx, eax
     mov       ebx, [KernelMemManager.FreeList]
     mov       edi, [amount]
.StartSearch:
     cmp       [ebx + eax + MemBlock.Size], edi
     jl        @F
     mov       edx, eax
     jmp       .Found
@@:
     inc       eax
     loop      .StartSearch
     
     mov       ecx, [KernelMemManager.CurIndex]
.SecSearch:     
     cmp       [ebx + ecx - 1 + MemBlock.Size], edi
     jl        @F
     mov       edx, eax
     jmp       .Found
@@:
     loop      .SecSearch


     stdcall   KernelMemManager.MemRes [amout]
     stdcall   KernelMemManager.RMalloc [amout]
     jmp       .EndProc

.Found:

     mov       [KernelMemManager.CurIndex], edx
     sub       [ebx + edx + MemBlock.Size], edi
     push      [ebx + edx + MemBlock.Offset]
     add       [ebx + edx + MemBlock.Offset], edx

     cmp       [ebx + edx + MemBlock.Size], 0
     jne       @F
     mov       eax, [KernelMemManager.FreeCount]
     mov       ecx, [ebx + eax - 1 + MemBlock.Size]
     mov       [ebx + edx + MemBlock.Size], ecx
     mov       ecx, [ebx + eax - 1 + MemBlock.Offset]
     mov       [ebx + edx + MemBlock.Offset], ecx 
     dec       [KernelMemManager.FreeCount]
@@:

     mov       ebx, [KernelMemManager.UsedList]
     mov       edx, [KernelMemManager.UsedCount]
     
     mov       [ebx + edx + MemBlock.Size], edi
     pop       eax
     mov       [ebx + edx + MemBlock.Size], eax
     inc       [KernelMemManager.UsedCount]
     push      eax
     stdcall   KernelMemManager.Checkbounds
     pop

.EndProc:
     ret
endp

proc KernelMemManager.Free what
     mov       eax, [what]
     cmp       [KernelMemManager.FreeList], eax
     je        .EndProc
     cmp       [KernelMemManager.UsedList], eax
     je        .EndProc

     stdcall   Mutex.Wait KernelMemManager.Mutex 
     stdcall   KernelMemManager.RFree [what] 
     stdcall   Mutex.Release KernelMemManager.Mutex 
     
.EndProc:     
     ret     
endp

proc KernelMemManager.RFree what uses ebx edi esi
     
     mov       ecx, [KernelMemManager.UsedCount]
     mov       eax, [what]
     mov       ebx, [KernelMemManager.UsedList]
.StartSearch:
     cmp       [ebx + ecx - 1 + MemBlock.Offset], eax
     je        .Found
     loop      .StartSearch
     jmp       .EndProc

.Found:     
     mov       esi, [ebx + ecx - 1 + MemBlock.Size]
     mov       edi, [ebx + ecx - 1 + MemBlock.Offset]

     mov       edx, [KernelMemManager.UsedCount]
     
     mov       eax, [ebx + edx - 1 - MemBlock.Size]
     mov       [ebx + ecx - 1 + MemBlock.Size], eax
     
     mov       eax, [ebx + edx - 1 - MemBlock.Offset]
     mov       [ebx + ecx - 1 + MemBlock.Offset], eax

     dec       [KernelMemManager.UsedCount]

     mov       ebx, [KernelMemManager.FreeList]
     mov       ecx, [KernelMemManager.FreeCount]
     mov       edx, [KernelMemManager.FreeCount]

.Seek:
     mov       eax, [ebx + ecx - 1 + MemBlock.Offset]
     add       eax, [ebx + ecx - 1 + MemBlock.Size]
     cmp       eax, edi
     jne       @F
     add       esi, [ebx + ecx - 1 + MemBlock.Size]
     sub       edi, [ebx + ecx - 1 + MemBlock.Size]

     mov       eax, [ebx + edx - 1 + MemBlock.Offset]
     mov       [ebx + ecx - 1 + MemBlock.Offset], eax

     mov       eax, [ebx + edx - 1 + MemBlock.Size]
     mov       [ebx + ecx - 1 + MemBlock.Size], eax
     dec       edx
@@:
     mov       eax, edi
     add       eax, esi
     cmp       [ebx + ecx - 1 + MemBlock.Offset], eax
     jne       @F
     
     add       esi, [ebx + ecx - 1 + MemBlock.Size]

     mov       eax, [ebx + edx - 1 + MemBlock.Offset]
     mov       [ebx + ecx - 1 + MemBlock.Offset], eax

     mov       eax, [ebx + edx - 1 + MemBlock.Size]
     mov       [ebx + ecx - 1 + MemBlock.Size], eax
     dec       edx 
@@:

     loop      .Seek

     mov       [edx + edx + MemBlock.Size], esi
     mov       [edx + edx + MemBlock.Offset], edi
     inc       edx

     mov       [KernelMemManager.FreeCount], edx

.EndProc:     
     ret     
endp

proc KernelMemManager.MemRes sizeInc uses edi
     mov       edi, [Kernel.MaxMem]
     mov       ecx, edi
     dec       edi
     and       edi, 0xFFFFF000
     shr       edi, 12
     inc       edi

     add       ecx, [sizeInc]
     dec       ecx
     and       ecx, 0xFFFFF000
     shr       ecx, 12
     inc       ecx
     sub       ecx, edi
@@:
     push      ecx
     stdcall   FramePoolGetFreePage
     stdcall   Pager.MapPage edi, eax, AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     pop       ecx
     inc       edi
     loop      @B
     
     mov       ecx, [KernelMemManager.UsedList]
     mov       edx, [KernelMemManager.UsedCount]
     
     mov       edi, [sizeInc]
     mov       [ecx + edx + MemBlock.Size], edi
     
     mov       eax, [Kernel.MaxMem]
     mov       [ecx + edx + MemBlock.Offset], eax
     
     inc       [KernelMemManager.UsedCount]

     stdcall   KernelMemManager.RFree eax

     add       [Kernel.MaxMem], edi 
     ret
endp

proc KernelMemManager.Checkbounds uses edi esi ebx
     mov       edi, [KernelMemManager.FreeSize]
     sub       edi, [KernelMemManager.FreeCount]

     mov       ebx, [KernelMemManager.UsedSize]
     sub       ebx, [KernelMemManager.UsedCount]

     cmp       edi, 5
     jge       @F
     mov       eax, [KernelMemManager.FreeSize]
     shl       eax, 4
           
     stdcall   KernelMemManager.RMalloc eax

     mov       edi, eax
     mov       esi, [KernelMemManager.FreeList]
     mov       ecx, [KernelMemManager.FreeSize]
     shr       ecx, 2

     rep       movsd

     mov       edi, eax
     stdcall   KernelMemManager.RFree   [KernelMemManager.FreeList]
     mov       [KernelMemManager.FreeList], edi

     shl       [KernelMemManager.FreeSize], 1     
     jmp       .EndFree
@@:
     mov       eax, [KernelMemManager.FreeSize]
     shr       eax, 2
     add       eax, 16
     cmp       edi, eax
     jl        .EndFree
     mov       eax, [KernelMemManager.FreeSize]
     cmp       eax, 128
     jle       .EndFree

     mov       eax, [KernelMemManager.FreeSize]
     shl       eax, 2
           
     stdcall   KernelMemManager.RMalloc eax

     mov       edi, eax
     mov       esi, [KernelMemManager.FreeList]
     mov       ecx, [KernelMemManager.FreeSize]
     shr       ecx, 3

     rep       movsd
     
     mov       edi, eax
     stdcall   KernelMemManager.RFree   [KernelMemManager.FreeList]
     mov       [KernelMemManager.FreeList], edi

     shr       [KernelMemManager.FreeSize], 1     
.EndFree:
     cmp       ebx, 5
     jge       @F

     mov       eax, [KernelMemManager.UsedSize]
     shl       eax, 4
           
     stdcall   KernelMemManager.RMalloc eax

     mov       edi, eax
     mov       esi, [KernelMemManager.UsedList]
     mov       ecx, [KernelMemManager.UsedSize]
     shr       ecx, 2

     rep       movsd

     mov       edi, eax
     stdcall   KernelMemManager.RFree   [KernelMemManager.UsedList]
     mov       [KernelMemManager.UsedList], edi

     shl       [KernelMemManager.FreeSize], 1     
     jmp       .EndProc
@@:
     mov       eax, [KernelMemManager.UsedSize]
     shr       eax, 2
     add       eax, 16
     cmp       ebx, eax
     jl        .EndProc
     mov       eax, [KernelMemManager.UsedSize]
     cmp       eax, 128
     jle       .EndProc  

     mov       eax, [KernelMemManager.UsedSize]
     shl       eax, 2
           
     stdcall   KernelMemManager.RMalloc eax

     mov       edi, eax
     mov       esi, [KernelMemManager.UsedList]
     mov       ecx, [KernelMemManager.UsedSize]
     shr       ecx, 3

     rep       movsd
     
     mov       edi, eax
     stdcall   KernelMemManager.RFree   [KernelMemManager.UsedList]
     mov       [KernelMemManager.UsedList], edi

     shr       [KernelMemManager.UsedSize], 1    
.EndProc:
     ret
endp

}
block(.data){
KernelMemManager.Mutex      db ?

KernelMemManager.UsedList   dd ?
KernelMemManager.FreeList   dd ?

KernelMemManager.CurIndex   dd ?
KernelMemManager.FreeCount  dd ?
KernelMemManager.FreeSize   dd ?
KernelMemManager.UsedCount  dd ?
KernelMemManager.UsedSize   dd ?
}