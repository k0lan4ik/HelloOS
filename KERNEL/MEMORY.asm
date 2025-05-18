label   SDZ.segFirstMCZ word at $0000

label   MCZ.cpSize      word at $0000
label   MCZ.wFlags      word at $0002
label   MCZ.segPrevMCZ  word at $0004
label   MCZ.segOwner    word at $0006

MCZ_FLAG_LASTITEM       = $0001

;Parameters
;Return  Total memory size
proc Memory.GetTotalSize
     clc
     int        12h
     jnc        .EndProc
.Default:
     mov        ax, 32
.EndProc:
     ret
endp

;Parameters
;       bx - number of start segment os kernel
;|Initialize List Memory Manager|
;Return none
proc Memory.Initialize uses ds bx
     call       Memory.GetTotalSize
     mov        cl, 6
     shl        ax, cl
     sub        ax, bx
     sub        ax, [cs:Kernel.cpSize]
     dec        ax
     mov        cx, (Options.Kernel.SDZSegment)
     dec        bx
     mov        ds, cx
     mov        [SDZ.segFirstMCZ], bx
     mov        ds, bx
     inc        bx
     mov        cx, [cs:Kernel.cpSize]
     mov        [MCZ.cpSize], cx
     mov        [MCZ.wFlags], 0
     mov        [MCZ.segPrevMCZ], $0000
     mov        [MCZ.segOwner], bx
     add        bx, [cs:Kernel.cpSize]
     mov        cx, ds
     mov        ds, bx
     mov        [MCZ.cpSize], ax
     mov        [MCZ.wFlags], MCZ_FLAG_LASTITEM
     mov        [MCZ.segPrevMCZ], cx
     mov        [MCZ.segOwner], 0
     ret
endp

;Parameters
;       ax - Needed size of block memory (in byte)
;       bx - Identificator of program, that need memory
;Return in ax number of allocate segment
proc Memory.AllocFirst uses ds bx
     add        ax, 15
     shr        ax, 4
     mov        dx, (Options.Kernel.SDZSegment)
     mov        ds, dx
     mov        dx, [SDZ.segFirstMCZ]
     mov        ds, dx

.SearchLoop:
     mov        cx, [MCZ.cpSize]
     cmp        [MCZ.segOwner], 0
     jne        .Next
     cmp        cx, ax
     jae        .Found
     test       [MCZ.wFlags], MCZ_FLAG_LASTITEM
     jnz        .NotFound
.Next:
     add        dx, cx
     inc        dx
     mov        ds, dx
     jmp        .SearchLoop
.Found:
     mov        [MCZ.segOwner], bx
     sub        cx, ax 
     jz         .Done
     mov        [MCZ.cpSize], ax
     mov        bx, [MCZ.wFlags]
     and        [MCZ.wFlags], not(MCZ_FLAG_LASTITEM)
     add        dx, ax
     inc        dx
     dec        cx
     push       ds
     mov        ax, ds
     mov        ds, dx
     mov        [MCZ.cpSize], cx
     mov        [MCZ.wFlags], bx
     mov        [MCZ.segPrevMCZ], ax
     mov        [MCZ.segOwner], 0
     pop        ds

.Done:
     mov        ax, ds
     inc        ax
     clc
     jmp        .EndProc

.NotFound:
     xor        ax, ax
     stc
.EndProc:
     ret
endp


proc Memory.AllocFAT uses ds bx
     add        ax, 15
     adc        dx, 0
     shrd       ax, dx, 4
     mov        dx, (Options.Kernel.SDZSegment)
     mov        ds, dx
     mov        dx, [SDZ.segFirstMCZ]
     mov        ds, dx

.SearchLoop:
     mov        cx, [MCZ.cpSize]
     cmp        [MCZ.segOwner], 0
     jne        .Next
     cmp        cx, ax
     jae        .Found
     test       [MCZ.wFlags], MCZ_FLAG_LASTITEM
     jnz        .NotFound
.Next:
     add        dx, cx
     inc        dx
     mov        ds, dx
     jmp        .SearchLoop
.Found:
     mov        [MCZ.segOwner], bx
     sub        cx, ax 
     jz         .Done
     mov        [MCZ.cpSize], ax
     mov        bx, [MCZ.wFlags]
     and        [MCZ.wFlags], not(MCZ_FLAG_LASTITEM)
     add        dx, ax
     inc        dx
     dec        cx
     push       ds
     mov        ax, ds
     mov        ds, dx
     mov        [MCZ.cpSize], cx
     mov        [MCZ.wFlags], bx
     mov        [MCZ.segPrevMCZ], ax
     mov        [MCZ.segOwner], 0
     pop        ds

.Done:
     mov        ax, ds
     inc        ax
     clc
     jmp        .EndProc

.NotFound:
     xor        ax, ax
     stc
.EndProc:
     ret
endp

;Parameters
;       ax - number allocate segment
;Return
proc Memory.Free uses ds bx si
     dec        ax
     mov        ds, ax
     mov        cx, [MCZ.cpSize]
     mov        bx, ax
     mov        si, [MCZ.wFlags]
     and        si, MCZ_FLAG_LASTITEM
     jnz        .NextMergeDone
     mov        dx, ax
     add        dx, cx
     inc        dx
     mov        ds, dx
     inc        dx
     cmp        [MCZ.segOwner], 0
     jne        .NextMergeDone
     mov        dx, [MCZ.wFlags]
     and        dx, MCZ_FLAG_LASTITEM
     or         si, dx
     add        cx, [MCZ.cpSize]
     inc        cx
.NextMergeDone:
     mov        ds, ax
     mov        dx, [MCZ.segPrevMCZ]
     test       dx, dx
     jz         .PrevMergeDone
     mov        ds, dx
     cmp        [MCZ.segOwner], 0
     jne        .PrevMergeDone
     add        cx, [MCZ.cpSize]
     inc        cx
     mov        bx, dx
.PrevMergeDone:
     mov        ds, bx
     mov        [MCZ.cpSize], cx
     mov        [MCZ.wFlags], si
     mov        [MCZ.segOwner], 0
.EndProc:
     ret
endp
