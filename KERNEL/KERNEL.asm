        format binary as 'SYS'
        include 'proc16.inc'

define DEBUG

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

.Exit:

        call    System.shutDown
        int     19h

APM_LOST_CODE = $86
System.shutDown:
        mov ax, $53_00
        xor bx, bx
        int 15h
        jc .oldVersion
.ApmVersion:
        mov ax, $53_04
        xor bx, bx
        int 15h
        jc .firstCheck
.continue_1:
        mov ax, $53_01 ;connect all devices to APM
        xor bx, bx
        int 15h
        jc .oldVersion

        mov ax, $53_08 ;set all of them managed by APM
        mov bx, $FFFF
        mov cx, 1
        int 15h
        jc .oldVersion

        mov ax, $53_07
        mov bx, $00_01
        mov cx, 3
        int 15h
.firstCheck:
        cmp ah, 03
        jne .oldVersion
        jmp .continue_1

.oldVersion:
        push  cs
        pop   ds
        mov ah, 09h
        mov dx, Statements.powerOff
        int 21h

..infinityLoop:
        cli
        hlt

ret

Statements.powerOff db "Power off PC manually...$"

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

;Parameters
;       ds:si - name of file
;Return in cf is error
proc LoadFont uses es bx di bp
     cmp  [Font.Segment], 0
     je   .First
     mov  ax, [Font.Segment]
     stdcall Memory.Free
.First:
     mov  bx, cs
     call FileSys.LoadFile
     jnc  @F
     DEBUGPRINT ax
     jmp  .EndProc
@@:
     test dx, dx
     jne  .SizeFalse
     cmp  ax, 4096
     jne  .SizeFalse
     mov [Font.Segment], di
     mov ax, 0x1100
     mov bx, 0x1000
     mov cx, 256
     xor dx,dx
     mov bp, dx
     mov es,di
     int 10h
     jmp .EndProc
.SizeFalse:
     xchg ax, di
     stdcall Memory.Free
     mov  ah, $08
.EndProc:
     ret
endp
Font.Segment dw 0

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