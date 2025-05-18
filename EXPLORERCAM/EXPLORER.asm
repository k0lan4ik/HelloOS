format binary as 'CAM'

org 100h
.EntryPoint:
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
    mov   dx, EnterStr
    mov   ah, $09
    int   21h
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
    mov   di, CommandEXE
    mov   cx, 7
    repe  cmpsb
    jne   @F
    mov   al, 20
    cmp   al, [ComandStr + 1]
    jae   .Ok
    mov   dx, ErrLengStr
    mov   ah, $09
    int   21h
    jmp   .Ender
.Ok:
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
    mov   ch, $05
    mov   si, FileName
    int   42h
    cmp   ah, 0
    je    .ExeCompl
    cmp   ah, $04
    jne   .Found
    mov   dx,ErrFNF
    mov   ah, $09
    int   21h
    jmp   .Ender
.Found:
.ExeCompl:
    jmp  .EntryPoint
@@:

    mov   si, Comand
    mov   di, CommandL
    mov   cx, 4
    repe  cmpsb
    jne   @F
    mov   cx, 20
.lsloop:
    push  cx
    mov   ax, 20
    sub   ax, cx
    mov   di, FileName
    mov   ch, $06
    int   42h
    cmp   cl, $F0
    je    .lsskip
    cmp   cl, $10
    je    .Fold
    mov    si, 11
    mov    di, 11
.lsLoopName:
    cmp   byte[si + FileName],' '
    je    .lssklpn
    mov   cl, [si + FileName]
    mov   [di + StringLS.FileName], cl
    dec   di

.lssklpn:
    dec   si
    cmp   si, 8
    jne   .lsndot
    mov   [di + StringLS.FileName], '.'
    dec   di
.lsndot:
    test  si, si
    jnz   .lsLoopName



    jmp   .lss

.Fold:
    mov   cx, 11
    mov   di, FileName
    add   di, 11
    mov   ax, ' '
    std
    repe  scasb
    inc   di
    inc   cx
    mov   si, StringLS.FileName
    add   si, 11
    rep   movsb

.lss:
    mov   ah, $09
    mov   dx, StringLS
    int   21h
    mov   al, ' '
    mov   di, StringLS.FileName
    mov   cx, 12
    rep   stosb
    mov   di, StringLS.Size
    mov   cx, 10
    rep   stosb

.lsskip:
    pop   cx
    loop  .lsloop
@@:
.Ender:
    jmp  .workLoop

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
CommandEXE db 'EXECUTE'
HelpStr    db 'Commands:',13,10,'  help - help about commands'\
                         ,13,10,'  exit - exit from this program'\
                         ,13,10,'  execute [file name] - execute program',13,10,'$'
ErrLengStr db 'Lenght of filename need lower or equel 8 (12 whis .CAM)',13,10,'$'
ErrFNF     db 'File Not Found', 13, 10, '$'
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
