block(.structs){
virtual at 0
    MemBlock.Size   dd ?
    MemBlock.Offset dd ?
    MemBlock.StrucSize:
end virtual
}

block(.text){
proc KernelMemManager.Init uses ebx
     add       [Kernel.MaxMem], 16
     mov       ebx, [Kernel.MaxMem]
     add       ebx, 4095
     shr       ebx, 12

     mov       ecx, 8
@@:
     push      ecx
     stdcall   FramePool.GetFreePage
     stdcall   Pager.MapPage, ebx, eax, AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     pop       ecx
     inc       ebx
     loop      @B

     mov       edx, [Kernel.MaxMem]
     mov       [KernelMemManager.FreeList], edx
     add       edx,  MemBlock.StrucSize * 128
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
     mov       [eax + MemBlock.Offset], eax
     add       [eax + MemBlock.Offset], 8 + 256

     mov       ecx, [Kernel.MaxMem]
     sub       ecx, [eax + MemBlock.Offset]
     mov       [eax + MemBlock.Size], ecx

     mov       ecx, [KernelMemManager.FreeSize]
     shl       ecx, 3
     mov       [edx + MemBlock.Size], ecx
     mov       [edx + MemBlock.Offset], eax

     mov       [edx + 1 * MemBlock.StrucSize + MemBlock.Size], ecx
     mov       [edx + 1 * MemBlock.StrucSize + MemBlock.Offset], edx

     xor       eax, eax
     mov       [KernelMemManager.CurIndex], eax

     stdcall   Mutex.Start, KernelMemManager.Mutex
     ret
endp    

proc KernelMemManager.Malloc amount
     stdcall   Mutex.Wait, KernelMemManager.Mutex
     stdcall   KernelMemManager.RMalloc, [amount]
     push      eax
     stdcall   Mutex.Release, KernelMemManager.Mutex
     pop       eax
     ret
endp

proc KernelMemManager.RMalloc uses ebx edi, amount
     STOP_POINT
     add       [amount], 15
     and       [amount], not 15
     


     mov       eax, [KernelMemManager.FreeCount]
     mov       edx,  MemBlock.StrucSize
     imul      ecx,  eax, MemBlock.StrucSize


     mov       eax, [KernelMemManager.CurIndex]
     mov       edx,  MemBlock.StrucSize
     imul      edx,  eax, MemBlock.StrucSize

     mov       edi, [ammout]
.StartSearch:
     cmp       edx, ecx
     jae       @F
     cmp       [ebx + edx + MemBlock.Size], edi
     jge       .Found

     add       edx, MemBlock.StrucSize
     jmp       .StartSearch

@@:

     mov       eax, [KernelMemManager.CurIndex]
     mov       edx,  MemBlock.StrucSize
     imul      eax,  eax, MemBlock.StrucSize
     xor       edx, edx
.SecSearch: 
     cmp       edx, eax
     jge       @F
     cmp       [ebx + edx + MemBlock.Size], edi
     jge       .Found

     add       edx, MemBlock.StrucSize
     jmp       .Found
@@:
 


     stdcall   KernelMemManager.MemRes, [amount]
     stdcall   KernelMemManager.RMalloc, [amount]
     jmp       .EndProc

.Found:

     push      edx
     xor       eax, eax
     xchg      eax, edx
     mov       ecx, MemBlock.StrucSize
     div       ecx
     pop       edx
     mov       [KernelMemManager.CurIndex], eax
     sub       [ebx + edx + MemBlock.Size], edi
     push      [ebx + edx + MemBlock.Offset]
     add       [ebx + edx + MemBlock.Offset], edi

     cmp       [ebx + edx + MemBlock.Size], 0
     jne       @F
     mov       eax, [KernelMemManager.FreeCount]
     dec       eax
     imul      eax, eax, MemBlock.StrucSize
     mov       ecx, [ebx + eax + MemBlock.Size]
     mov       [ebx + edx + MemBlock.Size], ecx
     mov       ecx, [ebx + eax + MemBlock.Offset]
     mov       [ebx + edx + MemBlock.Offset], ecx 
     dec       [KernelMemManager.FreeCount]
@@:

     mov       ebx, [KernelMemManager.UsedList]
     mov       edx, [KernelMemManager.UsedCount]
     imul      edx, edx, MemBlock.StrucSize
     
     mov       [ebx + edx + MemBlock.Size], edi
     pop       eax
     mov       [ebx + edx + MemBlock.Size], eax
     inc       [KernelMemManager.UsedCount]
     push      eax
     stdcall   KernelMemManager.Checkbounds
     pop       eax
     STOP_POINT
.EndProc:
     ret
endp

proc KernelMemManager.Free what
     mov       eax, [what]
     cmp       [KernelMemManager.FreeList], eax
     je        .EndProc
     cmp       [KernelMemManager.UsedList], eax
     je        .EndProc

     stdcall   Mutex.Wait, KernelMemManager.Mutex 
     stdcall   KernelMemManager.RFree, [what] 
     stdcall   Mutex.Release, KernelMemManager.Mutex 
     
.EndProc:     
     ret     
endp

proc KernelMemManager.RFree uses ebx edi esi, what
     
     mov       ecx, [KernelMemManager.UsedCount]
     imul      ecx, ecx, MemBlock.StrucSize
     
     mov       eax, [what]
     mov       ebx, [KernelMemManager.UsedList]
     xor       edx, edx
.StartSearch:
     cmp       edx, ecx
     jge       .EndProc
     
     cmp       [ebx + edx + MemBlock.Offset], eax
     jne       .StartSearch

.Found:     
     mov       esi, [ebx + edx + MemBlock.Size]
     mov       edi, [ebx + edx + MemBlock.Offset]

     sub       ecx, MemBlock.StrucSize
     
     mov       eax, [ebx + ecx - MemBlock.Size]
     mov       [ebx + edx + MemBlock.Size], eax
     
     mov       eax, [ebx + ecx - MemBlock.Offset]
     mov       [ebx + edx + MemBlock.Offset], eax

     dec       [KernelMemManager.UsedCount]

     mov       ebx, [KernelMemManager.FreeList]
     
     mov       ecx, [KernelMemManager.FreeCount]
     imul      ecx, ecx, MemBlock.StrucSize

     xor       edx, edx
.Seek:
     cmp       edx, ecx
     jae       .EndSeek
     
     mov       eax, [ebx + edx + MemBlock.Offset]
     add       eax, [ebx + edx + MemBlock.Size]
     cmp       eax, edi
     jne       @F
     add       esi, [ebx + edx + MemBlock.Size]
     sub       edi, [ebx + edx + MemBlock.Size]

     mov       eax, [ebx + ecx - MemBlock.StrucSize + MemBlock.Offset]
     mov       [ebx + edx + MemBlock.Offset], eax

     mov       eax, [ebx + ecx - MemBlock.StrucSize + MemBlock.Size]
     mov       [ebx + edx + MemBlock.Size], eax
     sub       ecx, MemBlock.StrucSize      
@@:
     
     lea       eax, [esi + edi]
     cmp       [ebx + edx + MemBlock.Offset], eax
     jne       @F
     
     add       esi, [ebx + edx + MemBlock.Size]

     mov       eax, [ebx + ecx - MemBlock.StrucSize + MemBlock.Offset]
     mov       [ebx + edx + MemBlock.Offset], eax

     mov       eax, [ebx + ecx - MemBlock.StrucSize + MemBlock.Size]
     mov       [ebx + edx + MemBlock.Size], eax
     sub       ecx, MemBlock.StrucSize 
@@:

     jmp       .Seek

.EndSeek:

     mov       eax, MemBlock.StrucSize
     xchg      eax, ecx
     xor       edx, edx
     div       ecx
     mov       [KernelMemManager.FreeCount]
     


.EndProc: 

     mov       [edx + edx + MemBlock.Size], esi
     mov       [edx + edx + MemBlock.Offset], edi

     inc       [KernelMemManager.FreeCount]

     stdcall   KernelMemManager.Checkbounds
    
     ret     
endp

proc KernelMemManager.MemRes uses edi, sizeInc
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
     stdcall   FramePool.GetFreePage
     stdcall   Pager.MapPage, edi, eax, AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     pop       ecx
     inc       edi
     loop      @B
     
     mov       ecx, [KernelMemManager.UsedList]
     mov       edx, [KernelMemManager.UsedCount]
     imul      edx, edx, MemBlock.StrucSize

     mov       edi, [sizeInc]
     mov       [ecx + edx + MemBlock.Size], edi
     
     mov       eax, [Kernel.MaxMem]
     mov       [ecx + edx + MemBlock.Offset], eax
     
     inc       [KernelMemManager.UsedCount]

     stdcall   KernelMemManager.RFree, eax

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
           
     stdcall   KernelMemManager.RMalloc, eax

     mov       edi, eax
     mov       esi, [KernelMemManager.FreeList]
     mov       ecx, [KernelMemManager.FreeSize]
     shr       ecx, 2

     rep       movsd

     mov       edi, eax
     stdcall   KernelMemManager.RFree,   [KernelMemManager.FreeList]
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
           
     stdcall   KernelMemManager.RMalloc, eax

     mov       edi, eax
     mov       esi, [KernelMemManager.FreeList]
     mov       ecx, [KernelMemManager.FreeSize]
     shr       ecx, 3

     rep       movsd
     
     mov       edi, eax
     stdcall   KernelMemManager.RFree,   [KernelMemManager.FreeList]
     mov       [KernelMemManager.FreeList], edi

     shr       [KernelMemManager.FreeSize], 1     
.EndFree:
     cmp       ebx, 5
     jge       @F

     mov       eax, [KernelMemManager.UsedSize]
     shl       eax, 4
           
     stdcall   KernelMemManager.RMalloc, eax

     mov       edi, eax
     mov       esi, [KernelMemManager.UsedList]
     mov       ecx, [KernelMemManager.UsedSize]
     shr       ecx, 2

     rep       movsd

     mov       edi, eax
     stdcall   KernelMemManager.RFree,   [KernelMemManager.UsedList]
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
           
     stdcall   KernelMemManager.RMalloc, eax

     mov       edi, eax
     mov       esi, [KernelMemManager.UsedList]
     mov       ecx, [KernelMemManager.UsedSize]
     shr       ecx, 3

     rep       movsd
     
     mov       edi, eax
     stdcall   KernelMemManager.RFree,   [KernelMemManager.UsedList]
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