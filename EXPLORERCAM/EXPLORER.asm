format binary as 'CAM'

org 100h
.EntryPoint:
    mov   ah, $4A
    mov   bx, 256
    int   21h
    mov   dx, HelloStr
    mov   ah, $09
    int   21h
.workLoop:
    mov   dx, StringRoad
    mov   ah, $09
    int   21h
    mov   dx, ComandStr
    mov   ah, $0A
    int   21h
    mov   dx, EnterStr
    mov   ah, $09
    int   21h


    cmp   [ComandStr + 1], 0
    je    .workLoop

    mov   al, ' '
    mov   di, Comand
    movzx cx, [Comand - 1]
    repe  scasb
    sub   cl, [Comand - 1]
    neg   cl
    mov   bx, cx

    movzx cx, [Comand - 1]
    mov   di, Comand
    test  cx, cx
    je    .NotNeed
.UpCase:
    cmp  byte[di], 'a'
    jb   .Skip
    cmp  byte[di], 'z'
    ja   .Skip
    sub  byte[di], $20
.Skip:
    inc   di
    loop  .UpCase
.NotNeed:

    cld

    mov   si, Comand
    mov   di, CommandH
    mov   cx, 4
    repe  cmpsb
    jne   @F
    mov   dx, HelpStr
    mov   ah, $09
    int   21h
    jmp   .Ender
@@:
    mov   si, Comand
    mov   di, CommandE
    mov   cx, 4
    repe  cmpsb
    jne   @F
    ret
@@:
    mov   si, Comand
    mov   di, CommandF
    mov   cx, 4
    repe  cmpsb
    jne   @F
    mov   al, 16
    cmp   al, [ComandStr + 1]
    jae   .OkFont
    mov   dx, ErrLengStr
    mov   ah, $09
    int   21h
    jmp   .Ender
.OkFont:
    call  FileNameConvert
    mov   ch, $07
    mov   si, FileName
    int   42h
    test  ah, ah
    je    .Ender
    cmp   ah, $08
    jne   .Err
    mov   dx, ErrInvFontFile
    mov   ah, $09
    int   21h
    jmp   .Ender
.Err:
    mov   dx, ErrStr
    mov   ah, $09
    int   21h
    jmp   .Ender
@@:
    mov   si, Comand
    mov   di, CommandEXE
    mov   cx, 7
    repe  cmpsb
    jne   @F
    mov   al, 20
    cmp   al, [ComandStr + 1]
    jae   .OkExecute
    mov   dx, ErrLengStr
    mov   ah, $09
    int   21h
    jmp   .Ender
.OkExecute:
    call  FileNameConvert
    mov   ch, $05
    mov   si, FileName
    int   42h
    cmp   ah, 0
    je    .ExeCompl
    cmp   ah, $03
    je    .NotAlloc
    cmp   ah, $04
    jne   .Found
    mov   dx,ErrFNF
    mov   ah, $09
    int   21h
    jmp   .Ender
.NotAlloc:
    mov   dx,ErrMem
    mov   ah, $09
    int   21h
.Found:

.ExeCompl:
    jmp  .EntryPoint
@@:

    mov   si, Comand
    mov   di, CommandL
    mov   cx, 4
    repe  cmpsb
    jne   @F
    mov   ch, $08
    int   42h
    jmp   .Ender
@@:
    mov   dx, ErrInvOp
    mov   ah, $09
    int   21h
.Ender:
    jmp  .workLoop

FileNameConvert:
    mov   di, FileName
    inc   si
    mov   cx, 8
.loopFileName:
    mov   al, '.'
    cmp   al, [si]
    jne   .Add
    mov   byte[di], ' '
    inc   di
    jmp   .endloop
.Add:
    movsb
.endloop:
    loop  .loopFileName
    inc   si
    mov   cx, 3
    rep   movsb
    ret

HexPrint:
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


CommandH   db 'HELP'
CommandL   db 'LIST'
CommandE   db 'EXIT'
CommandF   db 'FONT'
CommandEXE db 'EXECUTE'
HelpStr    db 'Comands:',13,10,'  help - help about comands'\
                         ,13,10,'  exit - exit from this program'\
                         ,13,10,'  list - show all in current directory'\
                         ,13,10,'  execute [file name] - execute program'\
                         ,13,10,'  font [file name] - switch font to font in file',13,10,'$'
ErrLengStr db 'Lenght of filename need lower or equel 8 (12 whis .CAM)',13,10,'$'
ErrFNF     db 'File Not Found', 13, 10, '$'
ErrMem     db 'Not have memory', 13, 10, '$'
ErrInvOp   db 'Comand not exist', 13, 10, '$'
ErrStr     db 'Error!', 13,10,'$'
ErrInvFontFile  db 'This file not needeble size', 13, 10, '$'
HelloStr   db 13,10,'Hello OS v1 Explorer', 13,10,'$'
EnterStr   db 13, 10, '$'
StringRoad db 'disk >$',0
StringLS   db '  '
StringLS.FileName db 12 dup ' '
           db '  '
StringLS.Size db 10 dup ' ', 'b', 13, 10, '$'
FileName   db 11 dup ? , '$'
ComandStr  db 255, 0
Comand     db 255 dup ?
