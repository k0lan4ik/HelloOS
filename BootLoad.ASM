        use16
        format binary as 'SYS'
        org     0

StartOS:
        mov     ax, $0070
        mov     ds, ax
        mov     es, ax
        mov     sp, $06FE
        call    InitInterputs
        call    Scene8025.InitScene

        mov     dl,  36      ;координаты по X
        mov     dh,  10      ;координаты по Y
        mov     ah, $01
        int     42h

        mov     dx,  Scene8025.HelloStr ;Адрес строки
        mov     ah, $03
        int     42h

.Load:

        mov     ah, $05
        int     42h
        cmp     al, $0D
        je      .StartProgram
        cmp     al, 27
        je      .PowOff
        jmp     .Load


.StartProgram:
        call    Scene8025.InitScene

        mov     ax, $0800
        mov     es, ax
        mov     bx, $0100      ; Адрес загрузки
        call    LoadDiskSector

        ; Запуск программы
        mov     ah, $04
        mov     dx, $0100      ; Адрес программы
        int     42h
        ;jmp     StartOS
.PowOff:
        call    PowerOff

LoadDiskSector:
    mov     ah, 00h         ; Сброс состояния диска
    mov     dl, $00        ; Дискета (A:)
    int     13h
    mov     ah, 02h         ; Чтение сектора
    mov     al, 1           ; 1 сектор
    mov     dh, 0           ; Головка 0
    mov     ch, 0          ; Цилиндр
    mov     cl, 4          ; Сектор
    mov     dl, $00        ; Дискета (A:)
    int     13h
    ret

PowerOff:
;---------------------------------
; INT 15 (5301h) - Advanced Power Management v1.0+ - CONNECT REAL-MODE INTERFACE
;        --[in]--        BX = device(ID) of system BIOS (0000h)
;---------------------------------
    mov   ax,5301h
    xor   bx,bx
    int   15h
 
;---------------------------------
; INT 15 (530Eh) - Advanced Power Management v1.1+ - SET DRIVER VERSION
;        --[in]--        BX = 0, CX = APM driver version(BCD)
;---------------------------------
    mov   ax,530Eh
    xor   bx,bx
    mov   cx,0102h    ; CH = major, CL = minor
    int   15h
 
;---------------------------------
; INT 15 (5307h) - Advanced Power Management v1.2 - TURN OFF SYSTEM
;        --[in]--        CX = 3, BX = 1 (for all power-managed devices)
;---------------------------------
    mov   ax,5307h
    mov   bx,1
    mov   cx,3
    int   15h
    ret

InitInterputs:
        cli
        push    0
        pop     es
        mov word [es:42h*4], InterputFunction
        mov word [es:42h*4 + 2], cs
        mov word [es:52h*4], InterputTerminate
        mov word [es:52h*4 + 2], cs
        sti
        ret


Scene8025.InitScene:
        pusha
        mov     ah, $00 ;Установка видеорежима
        mov     al, $03 ;Видеорежим
        int     10h

        mov     ah, $05 ;Выбираем видеостраницу
        xor     al, al
        int     10h

        xor     bx, bx  ;Номер видеостраницы (0)
        xor     dl, dl  ;Координата по Х (0)
        mov     dh, 25  ; установить курсор на строку 25 - сделать невидимым
        mov     ah, $02
        int     10h

        xor     di, di   ;Для перебора адресов видеопамяти
        mov     cx, 2000;Для счёта заполняемых символов
        mov     al, ' ' ;Символ для вывода
        mov     ah, [Scene8025.Background] ;Атрибуты экрана

        mov     dx, $0B800 ;Настраиваем регистр ES на начало видеопамяти
        mov     es, dx

        rep stosw
        popa
        ret

;Вывод символа
;Параметры
;dl параметр Символ
;dh параметры Вывода (цвет и т.д.)
Scene8025.WriteChar:
        push    bx
        push    ax
        push    dx
        push    di


        mov     bx, dx

        cmp     bl, 10
        jnz     @F
        xor     dl, dl
        mov     dh, [CS:Scene8025.CursorY]
        call    Scene8025.SetCursor
        jmp     .Exit
@@:
        cmp     bl, 13
        jnz     @F
        xor     dl, dl
        mov     dh, [CS:Scene8025.CursorY]
        inc     dh
        call    Scene8025.SetCursor
        jmp     .Exit
@@:
        cmp     bl, ' '
        jb      .Exit

        mov     ax, 0B800h      ; Адрес видеопамяти
        mov     es, ax
        mov     dl, [CS:Scene8025.CursorX]
        mov     dh, [CS:Scene8025.CursorY]
        inc     dl
        call    Scene8025.SetCursor
        dec     dl

        shl     dl, 1
        mov     ax, 160
        mul     dh
        xor     dh, dh
        add     ax, dx
        mov     di, ax
        mov     [es:di], bx

.Exit:
        pop     di
        pop     dx
        pop     ax
        pop     bx
        ret

;Вывод строки
;Параметры
;dx параметр Адрес Строки (должна кончаться на 0)
Scene8025.WriteStr:
        push    bx

        mov     bx, dx
        mov     dh, [CS:Scene8025.Background]
.WriteSumbol:
        mov     dl, [ds:bx]
        test    dl, dl
        jz      @F
        call    Scene8025.WriteChar
        inc     bx
        jmp     .WriteSumbol
@@:

        pop     bx
        ret

;Установить курсор в позицию
;Параметры
;dl параметр X
;dh параметр Y
Scene8025.SetCursor:
        push    cx
        push    ax

        movzx   ax, dl
        mov     cl, 80
        div     cl
        mov     [CS:Scene8025.CursorX], ah


        add     al, dh
        xor     ah, ah
        mov     cl, 25
        div     cl
        mov     [CS:Scene8025.CursorY], ah

        pop     ax
        pop     cx
        ret

ExecuteCAM:
        ;Сохраняем контекст ОС
        mov     [CS:OSBackAdr], sp
        mov     [CS:OSBackAdr+2], ss

        ; Настраиваем сегменты для новой программы
        mov     ax, es       ; PSP segment
        mov     ds, ax
        mov     ss, ax
        mov     sp, $FFFE    ; Ставим стек в конец сегмента


        ; Передаём управление
        mov     di, dx
        mov     word [ds:$0000], $52CD ; записываем прирывание PSP:0000
        push    0
        mov     word[CS:.jump_ptr], dx      ;
        mov     word[CS:.jump_ptr + 2], es  ;
        cli
        jmp     [CS:.jump_ptr]

        ret
.jump_ptr dd 0

ReadKey:
        xor     ah, ah        ; Функция чтения клавиши
        int     16h           ; Возвращает:
                   ; AL = ASCII-код (если есть)
                   ; AH = скан-код
        ret

ReadStr:
        mov     si, dx
        mov     cx, [ds:si]
        mov     di, si
        add     si, 2
        xor     bx, bx  ;Номер видеостраницы (0)
        mov     dl, [CS:Scene8025.CursorX]  ;Координата по Х (0)
        mov     dh, [CS:Scene8025.CursorY]  ;Координата по Y (0)
        mov     ah, $02
        int     10h
.WriteStr:

        call    ReadKey

        cmp     ah, $0e
        jne     .NotSp

        cmp     [ds:di], cx
        jbe      .WriteStr

        dec     si
        call    DecCur

        mov     dl, ' '
        mov     dh, [CS:Scene8025.Background]
        mov     ah, $02
        int     42h

        call    DecCur
        inc     cx
        dec     si
.SetCursor:

        xor     bx, bx  ;Номер видеостраницы (0)
        mov     dl, [CS:Scene8025.CursorX]  ;Координата по Х (0)
        mov     dh, [CS:Scene8025.CursorY]  ;Координата по Y (0)
        mov     ah, $02
        int     10h
        jmp     .WriteStr
.NotSp:

        cmp     al, 13
        je      .Exit

        cmp     cx, 0
        je      .WriteStr


        cmp     al, 32
        jb      .WriteStr



        cld
        stosb

        mov     dl, al
        mov     dh, [CS:Scene8025.Background]
        mov     ah, $02
        int     42h
        xor     bx, bx  ;Номер видеостраницы (0)
        mov     dl, [CS:Scene8025.CursorX]  ;Координата по Х (0)
        mov     dh, [CS:Scene8025.CursorY]  ;Координата по Y (0)
        mov     ah, $02
        int     10h
        dec     cx
        jmp    .WriteStr
.Exit:
        xor     bx, bx
        xor     dl, dl
        mov     dh, 25
        mov     ah, $02
        int     10h
        mov     byte[ds:si], $00
        ;pop     si
        sub     [ds:di], cx
        ret

DecCur:
        mov     dl, [CS:Scene8025.CursorX]  ;Координата по Х (0)
        mov     dh, [CS:Scene8025.CursorY]  ;Координата по Y (0)
        cmp     [CS:Scene8025.CursorX], 0
        jnz      @F
        dec     [CS:Scene8025.CursorY]
        mov     [CS:Scene8025.CursorX], 79
        ret
@@:
        dec     [CS:Scene8025.CursorX]
        ret

InterputFunction:
        push    bx
        push    cx
        push    dx
        push    bp
        push    di
        push    si
        push    es
        push    ds

        cmp     ah, $01
        jne     @F
        call    Scene8025.SetCursor
        jmp     .Exit
@@:
        cmp     ah, $02
        jne     @F
        call    Scene8025.WriteChar
        jmp     .Exit
@@:
        cmp     ah, $03
        jne     @F
        call    Scene8025.WriteStr
        jmp     .Exit
@@:
        cmp     ah, $04
        jne     @F
        call    ExecuteCAM
        jmp     .Exit

@@:
        cmp     ah, $05
        jne     @F
        call    ReadKey
        jmp     .Exit

@@:
        cmp     ah, $06
        jne     @F
        call    ReadStr
        jmp     .Exit
@@:

.Exit:
        pop     ds
        pop     es
        pop     si
        pop     di
        pop     bp
        pop     dx
        pop     cx
        pop     bx
        iret

InterputTerminate:
        sti
        mov     ss, [CS:OSBackAdr+2]
        mov     sp, [CS:OSBackAdr]

        pop     ds
        pop     es

        pop     si
        pop     di
        pop     bp
        pop     dx
        pop     cx
        pop     bx
        iret

Scene8025.CursorX       db 0
Scene8025.CursorY       db 0
Scene8025.Background    db $35
Scene8025.HelloStr      db 'Hello OS',0
OSBackAdr               dw 0,0    ; Для сохранения SS:SP ОС

times   ($400-($-StartOS)) db 144

       org     100h
Program:

EntryPoint:        
        mov     cx, 50
        mov     bl, 0        
        mov     bh, 0
Carloop:
        mov     dh, 0
        mov     dl, 0        
        mov     ah, 01h
        int     42h
        mov     dx, Spacestr        
        mov     ah, 03h
        int     42h
        mov     dh, 1        
        mov     dl, 0
        mov     ah, 01h        
        int     42h
        mov     dx, Spacestr
        mov     ah, 03h        
        int     42h

        mov     dh, 2        
        mov     dl, 0
        mov     ah, 01h        
        int     42h
        mov     dx, Spacestr
        mov     ah, 03h
        int     42h

        mov     dh, 0        
        mov     dl, bl
        mov     ah, 01h        
        int     42h
        mov     dx, Car1
        mov     ah, 03h        
        int     42h
                mov     dh, 1
        mov     dl, bl        
        mov     ah, 01h
        int     42h
        mov     dx, Car2        
        mov     ah, 03h
        int     42h
        mov     dh, 2        
        mov     dl, bl
        mov     ah, 01h        
        int     42h
        mov     dx, Car3
        mov     ah, 03h        
        int     42h
        inc     bl
        mov     dx, Strf
        mov     ah, $06
        int     42h
        mov     dx, Strf
        mov     ah, $03
        int     42h
        cmp     al, 'q'
        je      Exitpoint        
        loop    Carloop
        jmp     EntryPoint
Exitpoint:
        ret
Car1:
        db      '_____    ', 0
Car2:        
        db      '|    \___', 0
Car3:
        db      '\-O---O-/', 0
Spacestr:        db      '                                                                             ', 0
Strf             dw $000A



