include 'macro\proc32.inc'
include 'Blocks.inc'
use32

block(.consts) 
{
;Порты контроллера PS/2
PS2_DATA_PORT   equ 0x60
PS2_STATUS_PORT equ 0x64
PS2_CMD_PORT    equ 0x64

;Константы состояний для конечного автомата скан-кодов
PS2_STATE_NORMAL equ 0
PS2_STATE_GOT_E0 equ 1

;Константы для очереди
PS2_CMD_QUEUE_SIZE equ 16
KEY_BUFFER_SIZE    equ 64
}



block(.text) {

proc  PS2.Init;{ Инициализация PS2
     cli
     mov       al, 0xAD
     call      PS2.SendCommand
     mov       al, 0xA7
     call      PS2.SendCommand

.FlushLoop:
     in        al, PS2_STATUS_PORT
     test      al, 1
     jz        .FlushDone
     in        al, PS2_DATA_PORT
     jmp       .FlushLoop
.FlushDone:

     mov       al, 0x20
     call      PS2.SendCommand
     call      PS2.ReadData
     
     or        al, 1 or 2
     mov       ah, al

     mov       al, 0x60
     call      PS2.SendCommand
     
     mov       al, ah
     call      PS2.SendData

     
     mov       al, 0xAE
     call      PS2.SendCommand
     mov       al, 0xA8 
     call      PS2.SendCommand
     
     xor       eax, eax

     mov       [PS2.CmdQueue.Head], eax
     mov       [PS2.CmdQueue.Tail], eax
     mov       [PS2.KeyBufferHead], eax
     mov       [PS2.KeyBufferTail], eax
     mov       [PS2.CurrentCommand], al
     mov       [PS2.ScancodeState], PS2_STATE_NORMAL
     mov       [PS2.Mouse.Cycle], al


     mov       esi, 0x21 * 8
     mov       eax, PS2.IRQ1Handler
     mov       dx, cs
     call      Interrupt.Make      

     mov       esi, 0x2C * 8
     mov       eax, PS2.IRQ12Handler
     call      Interrupt.Make

     sti

     mov al, 0xD4 
     call PS2.QueueCommand
     mov al, 0xF4 
     call PS2.QueueCommand

     mov al, 0xF4
     call PS2.QueueCommand

     ret
endp

;       ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ВВОДА/ВЫВОДА

proc PS2.WaitForSend uses ecx eax;{ Ожидание, пока контроллер будет готов принять данные (бит 1 в порту 0x64 станет 0)
; ВНИМАНИЕ: Это блокирующая функция! Использовать только при инициализации.
    mov ecx, 100000 ; Таймаут
@@:
    in al, PS2_STATUS_PORT
    test al, 2 ; Проверяем бит 1 (Input buffer full)
    jz .ok     ; Если 0, буфер пуст, можно отправлять
    loop @B
    ; Здесь обработка ошибки таймаута
.ok:
    ret
endp


proc PS2.SendData;{ Отправка байта в порт данных
    ; al = байт для отправки
    call PS2.WaitForSend
    out PS2_DATA_PORT, al
    ret
endp


proc PS2.SendCommand;{ Отправка байта в порт команд
    ; al = байт для отправки
    push eax
    call PS2.WaitForSend
    pop eax
    out PS2_CMD_PORT, al
    ret
endp   


proc PS2.ReadData;{ Чтение байта из порта данных
    ; Здесь НЕ должно быть ожидания, т.к. функция вызывается из IRQ,
    ; который срабатывает, только когда данные уже есть.
    in al, PS2_DATA_PORT
    ret
endp


;            УПРАВЛЕНИЕ ОЧЕРЕДЬЮ КОМАНД

proc PS2.QueueCommand uses eax ecx edx;{ Поставить команду в очередь
     ; AL - команда
     mov       ecx, [PS2.CmdQueue.Head]
     mov       edx, ecx
     inc       edx
     and       edx, PS2_CMD_QUEUE_SIZE - 1

     cmp       edx, [PS2.CmdQueue.Tail]
     je        .QueueFull

     mov       [PS2.CmdQueue + ecx], al
     mov       [PS2.CmdQueue.Head], edx

     cmp       [PS2.CurrentCommand], 0
     jne       .EndProc
     call      PS2.SendNextFromQueue

.EndProc:
     clc
     ret

.QueueFull:
     stc
     ret

endp

proc PS2.SendNextFromQueue uses eax ecx ;{ Внутреняя функция для отправки следующей команды
     mov       ecx, [PS2.CmdQueue.Tail]
     cmp       ecx, [PS2.CmdQueue.Head]
     je        .EndProc

     mov       al, [PS2.CmdQueue + ecx]
     mov       [PS2.CurrentCommand], al
     mov       [PS2.RetryCount], 0

     inc       ecx
     and       ecx, PS2_CMD_QUEUE_SIZE - 1
     mov       [PS2.CmdQueue.Tail], ecx

     cmp       [PS2.SendNextFromQueue.IsPrevD4], 1
     je        .SendData
     cmp       al, 0xD4
     jne       @F
     xor       [PS2.SendNextFromQueue.IsPrevD4], 1
     mov       [PS2.CurrentCommand], 0
@@:
     call      PS2.SendCommand 
     jmp       .EndProc
.SendData: 
     call      PS2.SendData
     xor       [PS2.SendNextFromQueue.IsPrevD4], 1
.EndProc:
     ret   
     
PS2.SendNextFromQueue.IsPrevD4 db 0
endp

proc PS2.CommandSucceeded;{ Вызывается из IRQ при получении ACK (0xFA)
     mov byte  [PS2.CurrentCommand], 0
     call      PS2.SendNextFromQueue   
     ret
endp

proc PS2.ResendCurrentCommand
     inc       [PS2.RetryCount]
     cmp       [PS2.RetryCount], 3
     jae       .GiveUp

     mov       al, [PS2.CurrentCommand]
     call      PS2.SendData
     ret

.GiveUp:
     call      PS2.CommandSucceeded
     ret     
endp
}
block(.consts){
;                 ОБРАБОТЧИКИ ПРЕРЫВАНИЙ

;Флаги модификаторов (битовая маска)
MOD_SHIFT_FLAG      equ 1
MOD_CTRL_FLAG       equ 2
MOD_ALT_FLAG        equ 4
MOD_CAPS_LOCK_FLAG  equ 8

;Скан-коды клавиш-модификаторов 
SCANCODE_LSHIFT_PRESSED     equ 0x2A
SCANCODE_RSHIFT_PRESSED     equ 0x36
SCANCODE_LCTRL_PRESSED      equ 0x1D
SCANCODE_LALT_PRESSED       equ 0x38
SCANCODE_CAPSLOCK_PRESSED   equ 0x3A
}
block(.text){

proc PS2.IRQ1Handler 
     ;xchg bx, bx
     pusha
     call      PS2.ReadData

     cmp       al, 0xFA
     je        .ACK
     cmp       al, 0xFE
     je        .Resend

.Scancode:
     movzx     ecx, [PS2.ScancodeState]
     cmp       ecx, PS2_STATE_NORMAL
     je        .StateNormal
     cmp       ecx, PS2_STATE_GOT_E0
     je        .StateGotE0
     jmp       .EndProc

.StateNormal:
     cmp       al, 0xE0
     je        .HandleE0
     
     test      al, 0x80
     jnz       .Handle.BreakCode

.Handle.MakeCode:
     movzx     ebx, al

     cmp       bl, SCANCODE_LSHIFT_PRESSED
     je        .Handle.ShiftPress
     cmp       bl, SCANCODE_RSHIFT_PRESSED
     je        .Handle.ShiftPress
     cmp       bl, SCANCODE_LCTRL_PRESSED
     je        .Handle.CtrlPress
     cmp       bl, SCANCODE_LALT_PRESSED
     je        .Handle.AltPress
     cmp       bl, SCANCODE_CAPSLOCK_PRESSED
     je        .Handle.CapslockPress     
     
     ;Проверка на функциональные клавиши (F1-F12)
     cmp       bl, 0x3B ; F1
     jb        .Handle.ConvertToChar ; Не F-клавиша, обрабатываем как обычную
     cmp       bl, 0x44 ; F10
     jbe       .Handle.F1_F10
     cmp       bl, 0x57 ; F11
     je        .Handle.F11
     cmp       bl, 0x58 ; F12
     je        .Handle.F12
     jmp       .Handle.ConvertToChar ; Другие клавиши между F10 и F11
     
.Handle.F1_F10:
     sub       bl, 0x3B ; Индекс для таблицы (F1 -> 0)
     jmp       .FkeyLookup
.Handle.F11:
     mov       bl, 10 ; Индекс для F11
     jmp       .FkeyLookup
.Handle.F12:
     mov       bl, 11 ; Индекс для F12
.FkeyLookup:
     test      [PS2.ModifierState], MOD_ALT_FLAG
     jnz       .FkeyAlt
     test      [PS2.ModifierState], MOD_CTRL_FLAG
     jnz       .FkeyCtrl
     test      [PS2.ModifierState], MOD_SHIFT_FLAG
     jnz       .FkeyShift
.FkeyNormal:
     mov       al, [PS2.ScancodeMapFkeysNormal + ebx]
     jmp       .AddExtendedCharAndExit
.FkeyShift:
     mov       al, [PS2.ScancodeMapFkeysShift + ebx]
     jmp       .AddExtendedCharAndExit
.FkeyCtrl:
     mov       al, [PS2.ScancodeMapFkeysCtrl + ebx]
     jmp       .AddExtendedCharAndExit
.FkeyAlt:
     mov       al, [PS2.ScancodeMapFkeysAlt + ebx]
.AddExtendedCharAndExit:
     call      PS2.AddExtendedCharToKeyBuffer
     jmp       .EndProc

.Handle.ConvertToChar:
     cmp       bl, (PS2.ScancodeMapNormal.End - PS2.ScancodeMapNormal)
     jae       .EndProc

     ;Проверка на Alt + буквенно-цифровые клавиши
     test      [PS2.ModifierState], MOD_ALT_FLAG
     jnz       .Handle.AltChar

     ;Проверка на Ctrl + [a-z]
     test      [PS2.ModifierState], MOD_CTRL_FLAG
     jz        .CheckShiftAndCaps
     mov       al, [PS2.ScancodeMapNormal + ebx]
     cmp       al, 'a'
     jb        .CheckShiftAndCaps
     cmp       al, 'z'
     ja        .CheckShiftAndCaps
     and       al, 0x1F
     jmp       .AddToBuffer

.Handle.AltChar:
     cmp       bl, (PS2.ScancodeMapAltChars.End - PS2.ScancodeMapAltChars)
     jae       .EndProc
     mov       al, [PS2.ScancodeMapAltChars + ebx]
     test      al, al
     jz        .EndProc
     jmp       .AddExtendedCharAndExit

.CheckShiftAndCaps:
     mov       cl, [PS2.ModifierState]
     mov       ch, cl
     and       cl, MOD_SHIFT_FLAG
     and       ch, MOD_CAPS_LOCK_FLAG
     xor       cl, ch
     test      cl, cl
     jz        .UseNormalMap
.UseShiftedMap:
     mov       al, [PS2.ScancodeMapShifted + ebx]
     jmp       .AddToBuffer
.UseNormalMap:
     mov       al, [PS2.ScancodeMapNormal + ebx]
.AddToBuffer:
     test      al, al
     jz        .EndProc
     call      PS2.AddCharToKeyBuffer
     jmp       .EndProc     

.Handle.BreakCode:
     and       al, 0x7F
     movzx     ebx, al
     cmp       bl, SCANCODE_LSHIFT_PRESSED
     je        .Handle.ShiftRelease
     cmp       bl, SCANCODE_RSHIFT_PRESSED
     je        .Handle.ShiftRelease
     cmp       bl, SCANCODE_LCTRL_PRESSED
     je        .Handle.CtrlRelease
     cmp       bl, SCANCODE_LALT_PRESSED
     je        .Handle.AltRelease
     jmp       .EndProc

.Handle.ShiftPress: 
     or        [PS2.ModifierState], MOD_SHIFT_FLAG
     jmp       .EndProc
.Handle.ShiftRelease: 
     and       [PS2.ModifierState], not MOD_SHIFT_FLAG
     jmp       .EndProc
.Handle.CtrlPress: 
     or        [PS2.ModifierState], MOD_CTRL_FLAG 
     jmp .EndProc
.Handle.CtrlRelease: 
     and       [PS2.ModifierState], not MOD_CTRL_FLAG 
     jmp       .EndProc
.Handle.AltPress: 
     or        [PS2.ModifierState], MOD_ALT_FLAG
     jmp       .EndProc
.Handle.AltRelease: 
     and       [PS2.ModifierState], not MOD_ALT_FLAG 
     jmp       .EndProc
.Handle.CapslockPress: 
     xor       [PS2.ModifierState], MOD_CAPS_LOCK_FLAG
     jmp       .EndProc

.StateGotE0:
     test      al, 0x80
     jnz       .Handle.E0Break
.Handle.E0Make:
     movzx     ebx, al
     cmp       bl, SCANCODE_LCTRL_PRESSED
     je        .Handle.CtrlPress
     cmp       bl, SCANCODE_LALT_PRESSED
     je        .Handle.AltPress
     cmp       bl, (PS2.ScancodeMapExtended.End - PS2.ScancodeMapExtended)
     jae       .ResetStateAndExit
     mov       al, [PS2.ScancodeMapExtended + ebx]
     test      al, al
     jz        .ResetStateAndExit
     call      PS2.AddExtendedCharToKeyBuffer
     jmp       .ResetStateAndExit
.Handle.E0Break:
     and       al, 0x7F
     movzx     ebx, al
     cmp       bl, SCANCODE_LCTRL_PRESSED
     je        .Handle.CtrlRelease
     cmp       bl, SCANCODE_LALT_PRESSED
     je        .Handle.AltRelease
.ResetStateAndExit:
     mov       [PS2.ScancodeState], PS2_STATE_NORMAL
     jmp       .EndProc

.HandleE0:
    mov        [PS2.ScancodeState], PS2_STATE_GOT_E0
    jmp        .EndProc

.ACK:    
     call      PS2.CommandSucceeded
     jmp       .EndProc  

.Resend:
     call      PS2.ResendCurrentCommand

.EndProc:
     ; Посылаем сигнал End-of-Interrupt
     mov al, 0x20
     out 0x20, al     
     popa
     iretd
endp     

proc PS2.AddCharToKeyBuffer uses eax ecx edx
     mov       ecx, [PS2.KeyBufferHead]
     mov       edx, ecx
     inc       edx
     and       edx, KEY_BUFFER_SIZE - 1
     cmp       edx, [PS2.KeyBufferTail]
     je        .BufferFull
     mov       byte [PS2.KeyBuffer + ecx], al
     mov       [PS2.KeyBufferHead], edx
.BufferFull:
     ret
endp

proc PS2.AddExtendedCharToKeyBuffer uses ebx ecx edx
     mov       ecx, [PS2.KeyBufferHead]
     mov       edx, ecx
     add       edx, 2
     and       edx, KEY_BUFFER_SIZE - 1
     mov       ebx, [PS2.KeyBufferTail]
     cmp       ebx, ecx
     ja        .CheckWrap
.NoWrap:
     cmp       edx, ebx
     jbe       .BufferFull
     jmp       .DoWrite
.CheckWrap:
     cmp       edx, ebx
     jae       .BufferFull
.DoWrite:
     mov       [PS2.KeyBuffer + ecx], byte 0
     inc       ecx
     and       ecx, KEY_BUFFER_SIZE - 1
     mov       byte [PS2.KeyBuffer + ecx], al
     mov       [PS2.KeyBufferHead], edx
.BufferFull:
     ret
endp
}
block(.initData){
;Таблицы соответствия Скан-код -> ASCII
PS2.ScancodeMapNormal:
    db  0, 27, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', 8, 9
    db  'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', 13, 0
    db  'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', "'", '`', 0, '\'
    db  'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/', 0, '*', 0, ' ', 0
PS2.ScancodeMapNormal.End:

PS2.ScancodeMapShifted:
    db  0, 27, '!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '_', '+', 8, 9
    db  'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '{', '}', 13, 0
    db  'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':', '"', '~', 0, '|' ;"
    db  'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?', 0, '*', 0, ' ', 0
PS2.ScancodeMapShifted.End:

;Таблицы для расширенных клавиш 
PS2.ScancodeMapExtended:
    times 0x1D db 0
    db 13 ; Enter на цифровой клавиатуре
    times (0x35-0x1D-1) db 0
    db 47 ; / на цифровой клавиатуре
    times (0x47-0x35-1) db 0
    db 71, 72, 73, 0, 75, 0, 77, 0, 79, 80, 81, 0, 82, 83 ; Home, Up, PgUp, Left, Right, End, Down, PgDn, Ins, Del
PS2.ScancodeMapExtended.End:

;Таблицы для функциональных клавиш 
PS2.ScancodeMapFkeysNormal: db 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 133, 134
PS2.ScancodeMapFkeysShift:  db 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 135, 136
PS2.ScancodeMapFkeysCtrl:   db 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 137, 138
PS2.ScancodeMapFkeysAlt:    db 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 139, 140

; Таблица для Alt + буквенно-цифровые клавиши 
PS2.ScancodeMapAltChars:
    db 0,0,120,121,122,123,124,125,126,127,128,129,130,131,0,0 ; 0x00-0x0F
    db 16,17,18,19,20,21,22,23,24,25,0,0,0,0,30,31,32,33,34,35,36,37,38,0,0,0 ; 0x10-0x2A
    db 44,45,46,47,48,49,50,0,0,0,0,0,0,0,0,0 ; 0x2B-0x3A
PS2.ScancodeMapAltChars.End:
;Состояние клавиш-модификаторов
PS2.ModifierState   db 0 ; Битовая маска флагов MOD_*
}
block(.text){

proc PS2.IRQ12Handler 

 ;    xchg bx, bx

     pusha
     call      PS2.ReadData
     
     mov       cl, [PS2.Mouse.Cycle]
     
     test      cl, cl
     je        .Byte0
     cmp       cl, 2
     je        .Byte2

.Byte1:
     mov       [PS2.Mouse.Packet + 1], al
     mov       [PS2.Mouse.Cycle], 2
     jmp       .EndProc

.Byte0:
     test      al, 0x08
     jnz       @F
     mov       [PS2.Mouse.Cycle], 0
     jmp       .EndProc
 @@:        
     mov       [PS2.Mouse.Packet], al
     mov       [PS2.Mouse.Cycle], 1
     jmp       .EndProc

.Byte2:
     mov       [PS2.Mouse.Packet + 2], al

.ProccessPacket:
     mov       [PS2.Mouse.Cycle], 0
     
     movzx     eax, byte [PS2.Mouse.Packet+1] ; dx
     movzx     edx, byte [PS2.Mouse.Packet+2] ; dy
     movzx     ecx, byte [PS2.Mouse.Packet]   ; flags

     mov       [PS2.Mouse.Buttons], cl

     test      cl, 0x10
     jz        @F
     or        eax, 0xFFFFFF00
@@:
     add       [PS2.Mouse.X], eax


     test      cl, 0x20
     jz        @F
     or        edx, 0xFFFFFF00
@@:
     sub       [PS2.Mouse.Y], edx

.EndProc:
     mov       al, 0x20
     out       0xA0, al
     out       0x20, al
     popa
     iretd


endp

}
block(.data){
;Состояние мыши
PS2.Mouse.Cycle     db ?    ; Текущий байт в 3-байтовом пакете (0, 1, 2)
PS2.Mouse.Packet    db ?,?,?; Массив для хранения пакета
PS2.Mouse.X         dd ?    ; Координата X
PS2.Mouse.Y         dd ?    ; Координата Y
PS2.Mouse.Buttons   db ?    ; Состояние кнопок

;            ПЕРЕМЕННЫЕ
;Очередь команд
PS2.CmdQueue:       times PS2_CMD_QUEUE_SIZE db ?
PS2.CmdQueue.Head   dd ?
PS2.CmdQueue.Tail   dd ?

;Состояние текущей команды
PS2.CurrentCommand  db ?
PS2.RetryCount      db ?

;Конечный автомат для скан-кодов
PS2.ScancodeState   db ?


;Выходной буфер для ОС
PS2.KeyBuffer:       times KEY_BUFFER_SIZE db ?
PS2.KeyBufferHead    dd ?
PS2.KeyBufferTail    dd ?
}