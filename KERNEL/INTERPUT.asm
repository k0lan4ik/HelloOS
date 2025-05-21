
proc Interput.Initialize uses es

     push    0
     pop     es
     pushf
     cli
     mov     word [es:00h*4], INT00h
     mov     word [es:00h*4 + 2], cs

     mov     word [es:06h*4], INT06h
     mov     word [es:06h*4 + 2], cs


     mov     word [es:20h*4], INT20h
     mov     word [es:20h*4 + 2], cs

     mov     word [es:21h*4], INT21h
     mov     word [es:21h*4 + 2], cs

     mov     word [es:42h*4], INT42h
     mov     word [es:42h*4 + 2], cs
     sti
     popf
     ret
endp

INT00h:
     push    cs
     pop     ds
     mov     si, DivisionError
     call    print_string
     jmp     INT20h
INT06h:
     push    cs
     pop     ds
     mov     si, OpcodeError
     call    print_string

     pop     di
     pop     es
     push    es
     push    di
     mov     bx, es
     stdcall HexPrint
     mov     si, OpcodeError.IP
     call    print_string

     mov     bx, di
     stdcall HexPrint
     mov     si, OpcodeError.OP
     call    print_string

     mov     bx, [es:di]
     stdcall HexPrint
     add     di, 2
     mov     bx, [es:di]
     stdcall HexPrint


INT20h:

     pop     ax
     pop     cx
     pop     dx
     push    cx
     push    dx
     push    cs
     push    Kernel.StartProgram.ReturnFromProgram

     iret

INT42h:
     sti
     cmp     ch, $09
     jne     @F
     call    print_string
     jmp     .EndInt
     nop
@@:
     cmp     ch, $01
     jne     @F
     push    bp
     mov     bp, sp
     mov     bx, [bp + 4]
     pop     bp
     stdcall Memory.AllocFirst
     jnc     .EndInt
     xor     ax, ax
     jmp     .EndInt
@@:
     cmp     ch, $02
     jne     @F
     push    bp
     mov     bp, sp
     mov     bx, [bp + 4]
     pop     bp
     stdcall Memory.AllocFAT
     jnc     .EndInt
     xor     ax, ax
     jmp     .EndInt
@@:
     cmp     ch, $03
     jne     @F
     push    ds ax
     dec     ax
     mov     ds, ax
     push    bp
     mov     bp, sp
     mov     bx, [bp + 4]
     pop     bp
     cmp     bx, [ds:$0006]
     jne     .Bed
     pop     ax ds
     stdcall Memory.Free
     jnc     .EndInt
.Bed:
     pop     ax ds
     xor     ax, ax
     jmp     .EndInt
@@:
     cmp     ch, $04
     jne     @F
     push    bp
     mov     bp, sp
     mov     bx, [bp + 4]
     pop     bp
     stdcall FileSys.LoadFile
     jmp     .EndInt
@@:
     cmp     ch, $05
     jne     @F
     push    ds es
     stdcall Kernel.StartProgram
     pop     es ds
     jmp     .EndInt
@@:
     cmp     ch, $06
     jne     @F
     call    FileSys.ScanDir
     jmp     .EndInt
@@:
     cmp     ch, $07
     jne     @F
     call    LoadFont
     jmp     .EndInt
@@:
.EndInt:
     iret

INT21h:
     cmp     ah, $09
     jne     @F
     call    .print_string
     jmp     .EndInt
@@:
     cmp     ah, $02
     jne     @F
     call    .print_char
     jmp     .EndInt
@@:
     cmp     ah, $01
     jne     @F
     call    .read_char
     jmp     .EndInt
@@:
     cmp     ah, $0A
     jne     @F
     call    .read_string
     jmp     .EndInt
@@:
     cmp     ah, $08
     jne     @F
     call    .read_char_silent
     jmp     .EndInt
@@:
     cmp     ah, $0B
     jne     @F
     call    .check_key
     jmp     .EndInt
@@:
     cmp     ah, $0C
     jne     @F
     call    .ClearBuf
     jmp     .EndInt
@@:

.EndInt:
     iret

.check_key:
     mov ah, 01h
     int 16h
     mov al, $ff
     jnz  @F
     xor ax, ax
@@:
     ret

.print_string:
    pusha
    mov si, dx
..loop:
    lodsb
    cmp al, '$'
    je ..done
    push  si
    mov ah, 0x0E
    int 10h
    pop  si
    jmp ..loop
..done:
    popa
    ret

.print_char:
    pusha
    mov ah, 0x0E
    mov al, dl
    int 10h
    popa
    ret

.read_char:
    mov ah, 0x10
    int 16h
    mov ah, 0x0E
    int 10h
    ret

.read_string:
    pusha
    push es
    mov di, dx

    mov cl, [di]
    mov ch, 0
    inc di
    inc di

..read_loop:
    push cx
    mov ah, 00h
    int 16h
    cmp al, 0Dh
    je .done

    cmp al, 08h
    je ..backspace

    pop cx
    cmp ch, cl
    jae ..read_loop


    mov [di], al
    inc di
    inc ch

    push cx
    mov ah, 0Eh
    int 10h
    pop cx
    jmp ..read_loop

..backspace:
    pop cx
    cmp ch, 0
    je ..read_loop

    dec di
    dec ch

    push cx
    push dx

    mov ah, 03h
    mov bh, 0
    int 10h

    cmp dl, 0
    jne ..normal_backspace

    mov dl, 79
    dec dh
    cmp dh, 0xFF
    jne ..set_cursor
    mov dh, 0
    jmp ..set_cursor

..normal_backspace:
    dec dl

..set_cursor:
    push dx

    mov ah, 02h
    int 10h

    mov al, ' '
    mov ah, 0Eh
    int 10h

    pop dx
    mov ah, 02h
    int 10h

    pop dx
    pop cx
    jmp ..read_loop

.done:
    pop cx
    mov al, 0Dh
    mov [di], al
    movzx cx, ch
    sub di, cx
    mov [di - 1], cl

    pop es
    popa
    ret

.read_char_silent:
    cmp [BufExtend], 0
    jz  .Good
    mov al,[BufExtend]
    mov [BufExtend], 0
    jmp .EndProcChar
.Good:
    mov ah, 10h
    int 16h
    test al, al
    jz  @F
    cmp al, 0eh
    je  @F
    jmp .EndProcChar
@@:
    mov [BufExtend], ah
.EndProcChar:
    ret

.ClearBuf:
    push  ax
.clear:
    mov ah, 01h
    int 16h
    jz .endClear
    xor ax, ax
    int 16h
    jmp .clear
.endClear:
    pop ax
    xchg ah,al
    int 21h
    ret
BufExtend     db 0
DivisionError db 'Program terminate with division by 0 error!',0
OpcodeError db 'Program terminate with invalid Opcode cs: ',0
.IP db ' ip: ',0
.OP db ' op: ',0