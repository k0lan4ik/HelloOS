block(.consts){
    ; DMA Constraints
    DMA_MAX_PHYSICAL    = 0x00FFFFFF  ; Maximum physical address (16MB)
    DMA_MIN_PHYSICAL    = 0x00001000  ; Minimum physical address (above 4KB)
    DMA_REGION_SIZE     = 0x00F00000  ; 15MB available for DMA (1MB-16MB)
    
    ; Buffer alignment requirements
    DMA_ALIGN_ISA       = 64 * 1024   ; 64KB alignment for ISA DMA
    DMA_ALIGN_FLOPPY    = 2           ; Word alignment for floppy
    DMA_ALIGN_ATA       = 4           ; Dword alignment for ATA
    
    ; Memory block status
    DMA_BLOCK_FREE      = 0
    DMA_BLOCK_ALLOCATED = 1
    DMA_BLOCK_RESERVED  = 2
    DMA_BLOCK_SYSTEM    = 3
    
    ; Maximum number of DMA buffers
    DMA_MAX_BUFFERS     = 32
}

block(.structs){
virtual at 0
    DMABlock.PhysStart   dd ?     ; Physical start address
    DMABlock.VirtStart   dd ?     ; Virtual start address (kernel mapped)
    DMABlock.Size        dd ?     ; Size in bytes
    DMABlock.Status      db ?     ; Block status
    DMABlock.Owner       dd ?     ; Pointer to owner structure
    DMABlock.Alignment   dd ?     ; Alignment requirement
    DMABlock.Flags       dd ?     ; Additional flags
    DMABlock.Pad         db 12 dup ? ; Padding for 32-byte structure
    DMABlockSize:
end virtual

virtual at 0
    DMARequest.Size      dd ?     ; Requested size in bytes
    DMARequest.Alignment dd ?     ; Alignment requirement
    DMARequest.Flags     dd ?     ; Request flags
    DMARequest.Owner     dd ?     ; Owner identifier
    DMARequest.Callback  dd ?     ; Callback for asynchronous allocation
    DMARequest.UserData  dd ?     ; User data for callback
    DMARequestSize:
end virtual
}

block(.text){

proc DMA.Init
    stdcall     Mutex.Start, DMA.GlobalMutex
    
    stdcall     DMA.InitMemoryRegion
    
    ;stdcall     DMA.InitFreeBlocks
    
    mov         edi, DMA.AllocatedBlocks
    mov         ecx, DMA_MAX_BUFFERS
    xor         eax, eax
    rep         stosd
    
    mov         [DMA.AllocatedCount], 0
    
    ;call        DMA.RegisterWithKMM
    
    stdcall     VGA.PutString, Str.DMAInit
    
    mov         eax, [DMA.TotalFree]
    shr         eax, 10  
    stdcall     VGA.PrintDec, eax
    stdcall     VGA.PutString, Str.KBAvailable
    
    ret
endp

proc DMA.InitMemoryRegion uses ebx
    
    mov     [DMA.RegionStart], 0x00100000
    mov     [DMA.RegionEnd], 0x01000000  ; 16MB
    
    mov     [DMA.TotalMemory], DMA_REGION_SIZE
    mov     [DMA.TotalFree], DMA_REGION_SIZE
    mov     [DMA.LargestFree], DMA_REGION_SIZE
    
    mov     eax, [DMA.RegionStart]
    mov     dword [DMA.FreeBlocks + DMABlock.PhysStart], eax
    mov     dword [DMA.FreeBlocks + DMABlock.Size], DMA_REGION_SIZE
    mov     byte [DMA.FreeBlocks + DMABlock.Status], DMA_BLOCK_FREE

    mov     eax, [DMA.RegionStart]
    mov     ebx, [DMA.RegionEnd]
    sub     ebx, eax
    shr     eax, 12  
    shr     ebx, 12  

    mov     edx, DMA_VIRT_BASE shr 12
.MapLoop:
    push    eax edx
    stdcall Pager.MapPage, edx, eax, AL_FL_WRITABLE or AL_FL_GLOBAL
    pop     edx eax
    
    inc     eax
    inc     edx
    dec     ebx
    jnz     .MapLoop
        
    mov     dword [DMA.FreeBlocks + DMABlock.VirtStart], DMA_VIRT_BASE
        
    mov     [DMA.FreeBlockCount], 1
        
    ret 
endp    


proc DMA.AllocateBuffer uses ebx esi edi, size: DWORD, alignment: DWORD, flags: DWORD, owner: DWORD
    locals
        TotalSize    dd ?
        PhysAddr     dd ?
        VirtAddr     dd ?
    endl

    stdcall Mutex.Wait, DMA.GlobalMutex
    
    cmp     [size], 0
    je      .Invalid
    
   
    mov     eax, [size]
    add     eax, [alignment]
    dec     eax
    mov     [TotalSize], eax

    mov     esi, DMA.FreeBlocks
    mov     ecx, [DMA.FreeBlockCount]
    
.SearchLoop:
    test    ecx, ecx
    jz      .NoMemory
    
    cmp     byte [esi + DMABlock.Status], DMA_BLOCK_FREE
    jne     .NextBlock
    

    mov      eax, [esi + DMABlock.Size]
    cmp      eax, [TotalSize]
    jb      .NextBlock

    mov      eax, [esi + DMABlock.PhysStart]
    mov      ebx, [alignment]
    test     ebx, ebx
    jz      .AlignmentOk

    add     eax, ebx
    dec     eax
    not     ebx
    and     eax, ebx

    mov     edx, [esi + DMABlock.PhysStart]
    sub     eax, edx
    add     eax, [size]
    cmp     eax, [esi + DMABlock.Size]
    ja      .NextBlock
    
.AlignmentOk:
    jmp     .FoundBlock
    
.NextBlock:
    add     esi, DMABlockSize  
    dec     ecx
    jmp     .SearchLoop
    
.FoundBlock:

    mov     eax, [esi + DMABlock.PhysStart]
    mov     ebx, [alignment]
    
    add     eax, ebx
    
    mov     [PhysAddr], eax

     
    sub     eax, [DMA.RegionStart]
    add     eax, DMA_VIRT_BASE
    
    mov     [VirtAddr], eax
    
    mov     eax, [esi + DMABlock.Size]
    sub     eax, [TotalSize]
    cmp     eax, DMABlockSize
    jb      .NoSplit
    
    mov     edi, DMA.FreeBlocks
    mov     ecx, [DMA.FreeBlockCount]
    imul    ecx, ecx, DMABlockSize
    add     edi, ecx
    
    push    esi
    push    edi
    mov     ecx, DMABlockSize / 4
    rep movsd
    pop     edi
    pop     esi
    
    mov     eax, [PhysAddr]
    add     eax, [TotalSize]
    mov     [edi + DMABlock.PhysStart], eax
    mov     eax, [esi + DMABlock.VirtStart]
    add     eax, [TotalSize]
    mov     [edi + DMABlock.VirtStart], eax
    mov     eax, [esi + DMABlock.Size]
    sub     eax, [TotalSize]
    mov     [edi + DMABlock.Size], eax

    inc     [DMA.FreeBlockCount]
    
.NoSplit:
    
    mov     eax, [TotalSize]
    mov     [esi + DMABlock.Size], eax
    mov     byte [esi + DMABlock.Status], DMA_BLOCK_ALLOCATED
    mov     eax, [owner]
    mov     [esi + DMABlock.Owner], eax
    mov     eax, [alignment]
    mov     [esi + DMABlock.Alignment], eax
    mov     eax, [flags]
    mov     [esi + DMABlock.Flags], eax

    stdcall DMA.AddToAllocatedList, esi
    

    mov     eax, [TotalSize]
    sub     [DMA.TotalFree], eax
    
    mov     eax, [VirtAddr]
    mov     edx, [PhysAddr]
    
    stdcall Mutex.Release, DMA.GlobalMutex
    ret
    
.NoMemory:
.Invalid:
    stdcall Mutex.Release, DMA.GlobalMutex
    xor     eax, eax
    xor     edx, edx
    ret
    

endp

proc DMA.FreeBuffer uses ebx esi edi, virtAddr: DWORD
    stdcall Mutex.Wait, DMA.GlobalMutex
    
    
    stdcall DMA.FindBlockByVirtAddr, [virtAddr]
    test    eax, eax
    jz      .NotFound
    
    mov     esi, eax 
    
    cmp     byte [esi + DMABlock.Status], DMA_BLOCK_ALLOCATED
    jne     .Invalid
    

    stdcall DMA.RemoveFromAllocatedList, esi
    
    mov     byte [esi + DMABlock.Status], DMA_BLOCK_FREE
    mov     dword [esi + DMABlock.Owner], 0
    
    mov     eax, [esi + DMABlock.Size]
    add     [DMA.TotalFree], eax
    

    stdcall DMA.MergeFreeBlocks
    
    stdcall Mutex.Release, DMA.GlobalMutex
    mov     eax, 1
    ret
    
.NotFound:
.Invalid:
    stdcall Mutex.Release, DMA.GlobalMutex
    xor     eax, eax
    ret
endp

proc DMA.AllocateBufferEx uses ebx, request: DWORD
    mov     ebx, [request]

    mov     eax, [ebx + DMARequest.Size]
    mov     ecx, [ebx + DMARequest.Alignment]
    mov     edx, [ebx + DMARequest.Flags]
    mov     esi, [ebx + DMARequest.Owner]
    
    stdcall DMA.AllocateBuffer, eax, ecx, edx, esi
    test    eax, eax
    jz      .Failed
    
    mov     [ebx + DMARequest.PhysAddr], edx
    mov     [ebx + DMARequest.VirtAddr], eax
    
    cmp     [ebx + DMARequest.Callback], 0
    je      .NoCallback
    
    push    eax edx
    mov     eax, [ebx + DMARequest.Callback]
    mov     edx, [ebx + DMARequest.UserData]
    stdcall eax, edx
    pop     edx eax
    
.NoCallback:
    mov     eax, 1
    ret
    
.Failed:
    xor     eax, eax
    ret
endp

proc DMA.GetPhysicalAddress virtAddr: DWORD
    mov     eax, [virtAddr]
    cmp     eax, DMA_VIRT_BASE
    jb      .NotDMA
    cmp     eax, DMA_VIRT_BASE + DMA_REGION_SIZE
    jae     .NotDMA
    
    sub     eax, DMA_VIRT_BASE
    add     eax, [DMA.RegionStart]
    ret
    
.NotDMA:
    xor     eax, eax
    ret
endp

proc DMA.GetVirtualAddress physAddr: DWORD
    mov     eax, [physAddr]
    cmp     eax, [DMA.RegionStart]
    jb      .NotDMA
    cmp     eax, [DMA.RegionEnd]
    jae     .NotDMA

    sub     eax, [DMA.RegionStart]
    add     eax, DMA_VIRT_BASE
    ret
    
.NotDMA:
    xor     eax, eax
    ret
endp

proc DMA.QueryFreeMemory
    mov     eax, [DMA.TotalFree]
    mov     edx, [DMA.LargestFree]
    ret
endp

proc DMA.FlushCache uses eax ecx edx, virtAddr: DWORD, size: DWORD
    mov     eax, [virtAddr]
    mov     ecx, [size]
    
    stdcall DMA.GetPhysicalAddress, eax
    test    eax, eax
    jz      .Done
    

    add     ecx, 63
    shr     ecx, 6 
    
.FlushLoop:
    clflush [eax]
    add     eax, 64
    loop    .FlushLoop
    
    mfence
    
.Done:
    ret
endp


proc DMA.FindBlockByVirtAddr uses edi, virtAddr: DWORD
    mov     edi, DMA.FreeBlocks
    mov     ecx, [DMA.FreeBlockCount]
    
.Search:
    test    ecx, ecx
    jz      .NotFound
    
    mov     eax, [edi + DMABlock.VirtStart]
    cmp     eax, [virtAddr]
    ja      .Next
    
    add     eax, [edi + DMABlock.Size]
    cmp     [virtAddr], eax
    jb      .Found
    
.Next:
    add     edi, DMABlockSize
    dec     ecx
    jmp     .Search
    
.Found:
    mov     eax, edi
    ret
    
.NotFound:
    xor     eax, eax
    ret
endp

proc DMA.AddToAllocatedList uses edi, block: DWORD
    mov     edi, DMA.AllocatedBlocks
    mov     ecx, [DMA.AllocatedCount]
    
    cmp     ecx, DMA_MAX_BUFFERS
    jae     .Full
    
    mov     eax, [block]
    mov     [edi + ecx * 4], eax
    inc     [DMA.AllocatedCount]
    
.Full:
    ret
endp

proc DMA.RemoveFromAllocatedList uses ebx edi esi, block: DWORD
    mov     esi, DMA.AllocatedBlocks
    mov     ecx, [DMA.AllocatedCount]
    
.Search:
    test    ecx, ecx
    jz      .NotFound
    
    mov     eax, [esi + ecx * 4 - 4]
    cmp     eax, [block]
    je      .Found
    
    dec     ecx
    jmp     .Search
    
.Found:
    
    mov     ebx, ecx
    dec     ebx
.ShiftLoop:
    cmp     ebx, [DMA.AllocatedCount]
    jae     .DoneShift
    
    mov     eax, [esi + ebx * 4 + 4]
    mov     [esi + ebx * 4], eax
    inc     ebx
    jmp     .ShiftLoop
    
.DoneShift:
    dec     [DMA.AllocatedCount]
    
.NotFound:
    ret
endp


proc DMA.MergeFreeBlocks uses ebx esi edi
    mov     esi, DMA.FreeBlocks
    mov     ecx, [DMA.FreeBlockCount]
    
    push    ecx    
    stdcall DMA.SortFreeBlocks
    pop     ecx
    
    mov     edi, esi
    mov     ebx, 1  
    
.MergeLoop:
    cmp     ebx, ecx
    jae     .Done
    
    mov     eax, [edi + DMABlock.PhysStart]
    add     eax, [edi + DMABlock.Size]
    
    cmp     eax, [edi + DMABlockSize + DMABlock.PhysStart]
    jne     .NextBlock
    
    cmp     byte [edi + DMABlock.Status], DMA_BLOCK_FREE
    jne     .NextBlock
    cmp     byte [edi + DMABlockSize + DMABlock.Status], DMA_BLOCK_FREE
    jne     .NextBlock
    
    mov     eax, [edi + DMABlockSize + DMABlock.Size]
    add     [edi + DMABlock.Size], eax
    

    push    ecx edi
    imul    edx, ecx, DMABlockSize
    add     edx, edi
    add     edi, DMABlockSize
.Shift:
    cmp     edi, edx
    jae     .ShiftDone
    
    mov     eax, [edi + DMABlockSize]
    mov     [edi], eax
    add     edi, 4
    jmp     .Shift
    
.ShiftDone:
    pop     edi ecx
    dec     ecx
    dec     [DMA.FreeBlockCount]
    jmp     .MergeLoop  
    
.NextBlock:
    add     edi, DMABlockSize
    inc     ebx
    jmp     .MergeLoop
    
.done:

    stdcall DMA.UpdateLargestFree
    ret
endp

proc DMA.SortFreeBlocks uses ebx esi edi
    mov     esi, DMA.FreeBlocks
    mov     ecx, [DMA.FreeBlockCount]
    dec     ecx 
    
    test    ecx, ecx
    jle     .Done
    
.OuterLoop:
    mov     edi, esi
    mov     ebx, ecx
    
.InnerLoop:
    mov     eax, [edi + DMABlock.PhysStart]
    mov     edx, [edi + DMABlockSize + DMABlock.PhysStart]
    cmp     eax, edx
    jbe     .NoSwap
    
    push    ecx
    mov     ecx, DMABlockSize / 4
.SwapLoop:
    mov     eax, [edi]
    xchg    eax, [edi + DMABlockSize]
    mov     [edi], eax
    add     edi, 4
    loop    .SwapLoop
    pop     ecx
    
.NoSwap:
    add     edi, DMABlock.Size
    dec     ebx
    jnz     .InnerLoop
    
    loop    .OuterLoop
    
.Done:
    ret
endp


proc DMA.UpdateLargestFree uses ecx esi
    mov     esi, DMA.FreeBlocks
    mov     ecx, [DMA.FreeBlockCount]
    mov     dword [DMA.LargestFree], 0
    

    test    ecx, ecx
    jz      .Done
.Search:    
    cmp     byte [esi + DMABlock.Status], DMA_BLOCK_FREE
    jne     .Next
    
    mov     eax, [esi + DMABlock.Size]
    cmp     eax, [DMA.LargestFree]
    jbe     .Next
    
    mov     [DMA.LargestFree], eax
    
.Next:
    add     esi, DMABlock.Size
    loop    .Search
    
.Done:
    ret
endp


proc DMA.RegisterWithKMM
    
    mov     eax, [DMA.RegionStart]
    mov     ebx, [DMA.RegionEnd]
    sub     ebx, eax

    shr     eax, 12
    shr     ebx, 12
    
.MarkPages:
    push    eax
    stdcall FramePool.MarkPageReserved, eax
    pop     eax
    
    inc     eax
    dec     ebx
    jnz     .MarkPages
    
    ret
endp

proc DMA.DumpInfo
    stdcall VGA.PutString, Str.DMAInfoHeader
    
    stdcall VGA.PutString, Str.RegionStart
    mov     eax, [DMA.RegionStart]
    stdcall VGA.PrintHex, eax
    
    stdcall VGA.PutString, Str.RegionEnd
    mov     eax, [DMA.RegionEnd]
    stdcall VGA.PrintHex, eax

    stdcall VGA.PutString, Str.TotalMemory
    mov     eax, [DMA.TotalMemory]
    stdcall VGA.PrintDec, eax

    stdcall VGA.PutString, Str.TotalFree
    mov     eax, [DMA.TotalFree]
    stdcall VGA.PrintDec, eax

    stdcall VGA.PutString, Str.LargestFree
    mov     eax, [DMA.LargestFree]
    stdcall VGA.PrintDec, eax
    
    stdcall VGA.PutString, Str.BlockInfo
    mov     esi, DMA.FreeBlocks
    mov     ecx, [DMA.FreeBlockCount]
    
.BlockLoop:
    test    ecx, ecx
    jz      .Done
    
    push    ecx
    stdcall VGA.PutString, Str.BlockStart
    mov     eax, [esi + DMABlock.PhysStart]
    stdcall VGA.PrintHex, eax
    
    stdcall VGA.PutString, Str.BlockSize
    mov     eax, [esi + DMABlock.Size]
    stdcall VGA.PrintDec, eax
    
    stdcall VGA.PutString, Str.BlockStatus
    movzx   eax, byte [esi + DMABlock.Status]
    stdcall VGA.PrintDec, eax
    
    pop     ecx
    add     esi, DMABlock.Size
    dec     ecx
    jmp .BlockLoop
    
.Done:
    stdcall VGA.PutString, Str.NewLine
    ret
endp

}

block(.data){
    ; DMA Virtual Address Base
    DMA_VIRT_BASE = 0xFD000000
    
    ; Mutexes
    DMA.GlobalMutex db ?
    
    ; Memory region information
    DMA.RegionStart dd ?
    DMA.RegionEnd   dd ?
    
    ; Free blocks management
    DMA.FreeBlocks      db DMABlockSize * 64 dup ?  ; Up to 64 free blocks
    DMA.FreeBlockCount  dd ?
    
    ; Allocated blocks tracking
    DMA.AllocatedBlocks dd DMA_MAX_BUFFERS dup ?
    DMA.AllocatedCount  dd ?
    
    ; Statistics
    DMA.TotalMemory  dd ?
    DMA.TotalFree    dd ?
    DMA.LargestFree  dd ?
    
}

block(.initData){
    DMA.Allocations dd 0
    DMA.Frees      dd 0
    DMA.Failures   dd 0
}

block(.initData){
    Str.DMAInit         db "DMA Manager initialized", 13, 10, 0
    Str.KBAvailable     db " KB available for DMA", 13, 10, 0
    Str.DMAInfoHeader   db "=== DMA Memory Info ===", 13, 10, 0
    Str.RegionStart     db "Region: ", 0
    Str.RegionEnd       db " - ", 0
    Str.TotalMemory     db "Total: ", 0
    Str.TotalFree       db " bytes, Free: ", 0
    Str.LargestFree     db " bytes, Largest: ", 0
    Str.BlockInfo       db "Blocks:", 13, 10, 0
    Str.BlockStart      db "  Start: 0x", 0
    Str.BlockSize       db ", Size: ", 0
    Str.BlockStatus     db ", Status: ", 0
    Str.NewLine         db 13, 10, 0
}