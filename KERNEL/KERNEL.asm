        format binary as 'SYS'
        include 'proc16.inc'

;define DEBUG

macro DEBUGPRINT From*
{
  match =DEBUG , DEBUG
  \{ rept 0 \\{ \}
  match ,
  \{
     pusha
     mov     bx, From
     stdcall HexPrint
     xor     ax, ax
     int     16h
     popa
  \}
}

Options.Kernel.SDZSegment        equ     $0060

EntryPoint:

        shrd    ax, dx, 4
        inc     ax
        mov     [cs:Kernel.cpSize], ax
        xor     ax, ax
        mov     ss, ax
        mov     sp, (Options.Kernel.SDZSegment) shl 4

        push    cs
        pop     ds

        mov     si, Kernel.StartWork
        call    print_string

        mov     si, Kernel.isMemoryError
        mov     bx, cs
        stdcall Memory.Initialize
        jc      .Exit

        mov     si, Kernel.isMemoryLoad
        call    print_string

        mov     si, Kernel.isFileSysError
        stdcall FileSys.Initialize
        jc      .Exit

        mov     si, Kernel.isFileSysLoad
        call    print_string

        stdcall Interput.Initialize


        push    cs
        pop     ds
        mov     si, Kernel.Explorer
        mov     bx, cs
        stdcall Kernel.StartProgram
        mov     si, Kernel.isFileError
        jnc     .NotErr
        cmp     ah, $04
        jne     @F
        mov     si, Kernel.Explorer
        jmp     .Exit
@@:
        cmp     ah, $03
        jne     @F
        mov     si, Kernel.NotAlloc
        jmp     .Exit
@@:
        cmp     ah, $05
        jne     @F
        mov     si, Kernel.NotLoad
        jmp     .Exit
@@:
        jmp     .Exit
.NotErr:
        mov     si, Kernel.isAllOk
.Exit:

        call    print_string
        mov     ax, $5307
        mov     bx, $0001
        mov     cx, $0003
        int     15h
        int     19h
;Parameters
;       ds:si - name of file
;       bx - loader prog segment (parent handle)
;Return in cf is error
proc Kernel.StartProgram
locals
  ProgSeg dw ?
endl
     mov     di, bx
     stdcall FileSys.SearchFile
     jnc     @F
     mov     ah, $04
     jmp     .EndProc
@@:
     push    bx
     xor     ax, ax
     mov     dx, $0001
     mov     bx, di
     stdcall Memory.AllocFAT
     pop     bx
     jnc     @F
     mov     ah, $03
     jmp     .EndProc
@@:

     mov     [ProgSeg], ax
     add     ax, $10
     xchg    ax, bx

     stdcall FileSys.LoadClusFile
     jnc     @F
     mov     ax, [ProgSeg]
     stdcall Memory.Free
     mov     ah, $05
     stc
     jmp     .EndProc
@@:

     push    bp
     mov     ds, [ProgSeg]
     mov     es, [ProgSeg]



     mov     word[ds:$0000], $20CD

     mov     [ds:$0002], sp
     mov     [ds:$0004], ss
     mov     ss, [ProgSeg]
     mov     sp, $0000



     push    0
     push    es
     push    $0100
     retf

.ReturnFromProgram:

     pop     es
     mov     ss, [es:$0004]
     mov     sp, [es:$0002]
     mov     ax, es
     stdcall Memory.Free
     jc      @F
@@:
     xor     ah, ah
     pop     bp
.EndProc:
     ret
endp

proc HexPrint
    mov     cx, 4
@@:
    rol     bx, 4
    mov     ax, bx
    and     al, 0000'0000_0000'1111b

    cmp     al, $0A
    sbb     al, $69
    das
    push    bx
    mov     ah, $0E
    mov     bx, $0007
    int     10h
    pop     bx
    loop    @B
    ret
endp

print_string:

.repeat:
        lodsb
        test    al, al
        jz      .return
        mov     ah, $0E
        mov     bx, $0007
        int     10h
        jmp     .repeat
.return:
        ret

        include 'MEMORY.asm'
        include 'FILESYS.asm'
        include 'INTERPUT.asm'

Kernel.StartWork        db 'Start initialize',13,10,0
Kernel.isMemoryError    db 'Memory manager error',13,10,0
Kernel.isFileSysError   db 'File System error',13,10,0
Kernel.isMemoryLoad     db 'Memory manager initialized',13,10,0
Kernel.isFileSysLoad    db 'File System initialized',13,10,0
Kernel.isFileError      db 'File Load error',13,10,0
Kernel.isAllOk          db 'All init',0
Kernel.Explorer         db 'EXPLORERCAM'
Kernel.NotFounExplorer  db ' not found',13,10,0
Kernel.NotAlloc         db 'Not Alloc mem',13,10,0
Kernel.NotLoad          db 'Not Load',13,10,0
Kernel.cpSize           dw 0