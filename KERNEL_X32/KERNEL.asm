        format binary as 'SYS'
        include 'proc16.inc'
        include 'Macros.inc'

;define DEBUG
macro STOP_POINT {
     match =DEBUG, DEBUG 
     \{
          rept 0 \\{
     \}
     match , 
     \{
          xchg bx, bx
     \}
}

macro PRINT_STOP  {
     match =DEBUG, DEBUG 
     \{
          rept 0 \\{
     \}
     match , 
     \{
          
          mov ax, $be00 or 'S' 
          mov  edi, $f00B8000
          mov  [edi], ax
          cli
          hlt 
          
     \}
}


block(.consts) {
GDT_NULL_SELECTOR     equ 0x00
KERNEL_CODE_SELECTOR  equ 0x08
KERNEL_DATA_SELECTOR  equ 0x10
TSS_SELECTOR          equ 0x18
USER_CODE_SELECTOR    equ 0x20
USER_DATA_SELECTOR    equ 0x28
CPL0_PROCDATA         equ 0x30
CPL0_THREAD           equ 0x38

TIMER_HZ = 1000

Options.Kernel.GDT        equ     GDT
Options.Kernel.IDT        equ     $1000
Options.Kernel.E820       equ     Options.Kernel.IDT + 256 * 8
Options.Kernel.EntryPoint equ     $2000 
Options.Kernel.Base       equ     Options.Kernel.EntryPoint - 64 - 16
Options.Kernel.StackHead  equ     Options.Kernel.IDT
Options.Kernel.HierHalf   equ     $E0000000

}

include 'Structs.asm'
  

block (.text) {

use16
org Options.Kernel.Base
    dd ?

GDTDescriptor:
    dw GDTend - GDT - 1 ; Лимит (размер - 1)
    dd GDT              ; Линейный адрес таблицы

IDTDescriptor:
    dw 256 * 8 - 1        ; Лимит (размер - 1)
    dd Options.Kernel.IDT ; Линейный адрес таблицы

GDT:
    ; 0x00: GDT_NULL_SELECTOR
    dq 0x0000000000000000

    ; 0x08: KERNEL_CODE_SELECTOR (Base=0, Limit=0xfffff, Type=Code, DPL=0)
    dw 0xFFFF      ; Limit (bits 0-15)
    dw 0x0000      ; Base (bits 0-15)
    db 0x00        ; Base (bits 16-23)
    db 10011010b   ; Access: P=1, DPL=00, S=1, Type=1010 (Code Exec/Read)
    db 11001111b   ; Flags: G=1 (4KB units), D=1 (32-bit), L=0, Limit (16-19)
    db 0x00        ; Base (bits 24-31)

    ; 0x10: KERNEL_DATA_SELECTOR (Base=0, Limit=0xfffff, Type=Data, DPL=0)
    dw 0xFFFF      ; Limit
    dw 0x0000      ; Base
    db 0x00        ; Base
    db 10010010b   ; Access: P=1, DPL=00, S=1, Type=0010 (Data Read/Write)
    db 11001111b   ; Flags: G=1, D=1, Limit (16-19)
    db 0x00        ; Base

    ; 0x18: TSS_SELECTOR (Base и Limit заполняются динамически в ядре)
    ; Пока зарезервируем место (8 байт)
    dq 0x0000000000000000 

    ; 0x20: USER_CODE_SELECTOR (Base=0, Limit=0xfffff, Type=Code, DPL=3)
    dw 0xFFFF
    dw 0x0000
    db 0x00
    db 11111010b   ; Access: P=1, DPL=11 (User), S=1, Type=1010
    db 11001111b
    db 0x00

    ; 0x28: USER_DATA_SELECTOR (Base=0, Limit=0xfffff, Type=Data, DPL=3)
    dw 0xFFFF
    dw 0x0000
    db 0x00
    db 11110010b   ; Access: P=1, DPL=11 (User), S=1, Type=0010
    db 11001111b
    db 0x00

    ; 0x30: CPL0_PROCDATA (Специфические данные процесса ядра)
    dw 0xFFFF
    dw 0x0000
    db 0x00
    db 10010010b   ; Ring 0 Data
    db 11001111b
    db 0x00

    ; 0x38: CPL0_THREAD (Специфические данные потока ядра)
    dw 0xFFFF
    dw 0x0000
    db 0x00
    db 10010010b   ; Ring 0 Data
    db 11001111b
    db 0x00

GDTend:

org Options.Kernel.EntryPoint
RealEntry:
     xchg    bx, bx
     mov     si, dx
     shl     esi, 16
     mov     si, ax 

     xor     ax, ax
     mov     ss, ax
     mov     sp, Options.Kernel.StackHead

     cli
     call      EnableA20WithMessage     
     sti
     
     mov     ah, 00h
     mov     al, 03h
     int     10h

     mov     ah, 05h               
     mov     al, 0                 
     int     10h

     mov bx,0                 
     mov dl,0                
     mov dh,25              
     mov ah,02h               
     int 10h
     cli 
jmp GotoProtected 


;===============================================================
; Процедура включения A20 линии
; Возвращает: CF=0 - успех, CF=1 - ошибка
;===============================================================
proc EnableA20
    call .TestA20
    jnc .Success
    
    mov ax, 0x2401
    int 0x15
    call .TestA20
    jnc .Success
    
    call .KbcMethod
    call .TestA20
    jnc .Success
    
    call .FastMethod
    call .TestA20
    jnc .Success
    
    stc
    ret
    
.Success:
    clc
    ret

;---------------------------------------------------------------
; Метод через клавиатурный контроллер
;---------------------------------------------------------------
.KbcMethod:
    call .WaitKbcEmpty
    
    mov al, 0xD1
    out 0x64, al
    call .WaitKbcEmpty
    

    mov al, 0xDF   
    out 0x60, al
    call .WaitKbcEmpty
    
    mov ecx, 10000
.WaitLoop:
    nop
    loop .WaitLoop
    
    ret

;---------------------------------------------------------------
; Быстрый метод через порт 0x92
;---------------------------------------------------------------
.FastMethod:
    in al, 0x92
    or al, 2        
    and al, 0xFE    
    out 0x92, al
    ret

;---------------------------------------------------------------
; Ожидание освобождения контроллера клавиатуры
;---------------------------------------------------------------
.WaitKbcEmpty:
    push ecx
    mov ecx, 100000  
    
.WaitLoop1:
    in al, 0x64
    test al, 2
    jz .Ready
    loop .WaitLoop1
    
.Ready:
    pop ecx
    ret

;---------------------------------------------------------------
; Проверка работы A20 линии
; Возвращает: CF=0 - работает, CF=1 - не работает
;---------------------------------------------------------------
.TestA20:
    pushad
    push es
    push fs
    
    
    xor ax, ax
    mov es, ax          
    dec ax
    mov fs, ax          

    mov eax, [es:0x600]
    mov ebx, [fs:0x610]
    push eax
    push ebx
    
    mov ecx, 100  

.TestLoop:
    mov eax, ecx
    not eax
    
    mov [es:0x600], eax
    
    mov ebx, [fs:0x610]
    
    wbinvd
    
    cmp eax, ebx
    je .NotWorking 

    not eax
    mov [es:0x600], eax
    wbinvd
    mov ebx, [fs:0x610]
    cmp eax, ebx
    jne .Working

    loop .TestLoop
    
    
.NotWorking:
    pop ebx
    pop eax
    
    mov [fs:0x610], ebx
    mov [es:0x600], eax
    stc
    jmp .Exit
    
.Working:
    pop ebx
    pop eax
    
    mov [fs:0x610], ebx
    mov [es:0x600], eax
    clc

.Exit:
    pop fs
    pop es
    popad
    ret

endp

;===============================================================
; Процедура с сообщением об ошибке
;===============================================================
proc EnableA20WithMessage
    call EnableA20
    jnc .Success
    
    mov si, .A20ErrorMsg
.ErrorLoop:
    lodsb
    test al, al
    jz .Halt
    mov ah, 0x0E
    int 0x10
    jmp .ErrorLoop
    
.Halt:
    cli
    hlt
    jmp .Halt
    
.Success:
    ret

.A20ErrorMsg db "Fatal: Cannot enable A20 line", 13, 10, 0

endp

GotoProtected:   
        
     mov       di,  Options.Kernel.E820 + 4
     xor       ebx, ebx
     xor       ebp, ebp
     mov       edx, 0x0534D4150
     mov       [es:di + 20], dword 1
     mov       ecx, 24
     mov       eax, 0xe820
     int       15h
     jc        .Error
     
     mov       edx, 0x0534D4150
     cmp       eax, edx
     jne       .Error
     
     test      ebx, ebx
     jz        .Error
     jmp       .TestEntry

.E820lp:
     mov       [es:di + 20], dword 1
     mov       ecx, 24       
     mov       eax, 0xe820              
     int       15h
     jc        .E820f           
     mov       edx, 0x0534D4150      
.TestEntry:
     jcxz      .SkipEntry
     cmp       cl, 20
     jbe       .NoText
     test      byte [es:di + 20], 1
     je        .SkipEntry
.NoText:
     mov       ecx, [es:di + 8]
     or        ecx, [es:di + 12]
     jz        .SkipEntry
     inc       ebp
     add       di, 24
     cmp       di, Options.Kernel.Base     
     jae       .E820f
.SkipEntry:
     test      ebx, ebx              
     jne       .E820lp
.E820f:
     mov       [es:Options.Kernel.E820], ebp
     
     cli

     lgdt      [GDTDescriptor]
     
     lidt      [IDTDescriptor]
     
     
     mov       eax, cr0
     and       al,  11111011b
     or        al,  00000011b
     mov       cr0, eax
     jmp       0x0008:ProtectedEntry

.Error:
     STOP_POINT
        pusha
        mov bx,0                 
        mov dl,0                
        mov dh,0              
        mov ah,02h               
        int 10h
        mov     ax, $0e00 or 'E'
        int     10h
        xor     ax, ax
        int     16h
        popa
     cli
     hlt

include 'macro\proc32.inc'
use32

proc Paging.Init
     push      ebp
     mov       ebp, esp
     push      edi ecx eax ebx
     mov       edi, PageDirectory
     mov       ecx, 1024 * 3
     xor       eax, eax
     rep stosd
     
     mov       dword [PageDirectory + 0xFFC00000 shr 22 * 4], PageDirectory or 0x019 ;(Present, Read/Write, Global)
     
     mov       dword [PageTable2], 0x00 or 0x003
     mov       dword [PageTable2 + 4], 0x01000 or 0x003 
     mov       dword [PageDirectory + 0xFF000000 shr 22 * 4], PageTable2 or 0x019 ; (Present, Read/Write, Global)

     mov       edi, PageTable1
     mov       esi, Options.Kernel.EntryPoint shr 12 
.MapKernelPages:
    
     mov       eax, esi
     shl       eax, 12
     or        eax, 0x003

     mov dword [edi], eax 
     inc       esi
     add       edi, 4

     cmp       esi, TSS shr 12
     jb        .MapKernelPages
 xchg bx, bx
     mov dword [PageDirectory], PageTable1 or 0x003

     mov dword [PageDirectory + (Options.Kernel.HierHalf shr 22) * 4], PageTable1 or 0x003

     
 

     mov       eax, PageDirectory
     mov       cr3, eax

     mov       eax, cr0
     or        eax, 0x80000000
     mov       cr0, eax

     add       esp, 0xFF000000 - 0x1000
     add       ebp, 0xFF000000 - 0x1000
     
     add       dword [GDTDescriptor + 2], 0xFF000000
     lgdt      [GDTDescriptor]

     
     add       dword [IDTDescriptor + 2], 0xFF000000
     lidt      [IDTDescriptor]

     add       dword[ebp + 4], Options.Kernel.HierHalf ;- Options.Kernel.EntryPoint
     xor       eax, eax
.UpDate:
     invlpg    [eax]
     add       eax, 4096
     cmp       eax, 1024 * 1024 
     jb        .UpDate
     pop       ebx eax ecx edi ebp
     ret
endp

ProtectedEntry:
     use32

     mov       ax, KERNEL_DATA_SELECTOR
     mov       ds, ax
     mov       es, ax
     mov       ss, ax
     mov       esp, Options.Kernel.StackHead

     
     call      Paging.Init
org Options.Kernel.HierHalf + $
        

     jmp       KERNEL_CODE_SELECTOR:@F
     
@@:
     mov       ax, KERNEL_DATA_SELECTOR      
     mov       ds, ax
     mov       es, ax
     mov       ss, ax
     add       esp, Options.Kernel.HierHalf 
     mov       ax, CPL0_THREAD
     mov       fs, ax
     mov       ax, CPL0_PROCDATA 
     mov       gs, ax
     finit

     mov dword [0xfffff000], 0x00000002
     

     call      IRQ.Init
     
     call      TSS.Init
     
     
 ;    stdcall   FramePool.Init1

     
     ;Инициализация страници под procdata for this processor
 ;    stdcall   FramePool.GetFreePage
 ;    stdcall   Pager.MapPage, 0xFF000, eax,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     ;Инициализация страниц под первые 12 потоков
;     stdcall   FramePool.GetFreePage 
;     stdcall   Pager.MapPage, 0xFE000, eax,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     
     
;     stdcall   FramePool.GetFreePage 
;     stdcall   Pager.MapPage, 0xFE001, eax,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     
;     stdcall   FramePool.GetFreePage 
;     stdcall   Pager.MapPage, 0xFE002, eax,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     

     ;Страница для первых N процессов
;     stdcall   FramePool.GetFreePage 
;     stdcall   Pager.MapPage, 0xFF102, eax,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     
     ;тут вместо регистра надо адрес таблицы и IDT и GDT
     
;     stdcall   Pager.MapPage, 0xFF100, Real.GDT shr 12,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     
     ;Переназначение таблицы
     

     ; E820 memory map
;     stdcall   Pager.MapPage, 0xFF101, 0x2, AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC

     ; page fault handler table
;     stdcall   FramePool.GetFreePage 
;     stdcall   Pager.MapPage, 0xFF120, eax,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC

    
     

;     stdcall   GS.Init 
    
;     stdcall   IntHeand.Init
;     stdcall   XInt.Init
     
;     stdcall   HardwInt.Init       
;     stdcall   KernPageFault.Init  

;     stdcall   KernelMemManager.Init   
         
  
;     stdcall   VGA.Init
;     stdcall   VGA.SetColor, VGA_COLOR_YELLOW, VGA_COLOR_BLUE     
;     mov  edi, 0xf00b8000 + 80 * 20 * 2
;     mov  ebx, dword[VGA.CursorX]
;     call HexPrint
;     mov  edi, 0xf00b8000 + 80 * 21 * 2
;     mov  ebx, dword[VGA.ColorFg]
;     call HexPrint
;     stdcall   VGA.PutString, Str.Goida 

     
;     stdcall   ProcessManager.Init     
;     stdcall   Sched.Init               
     
     
;     stdcall   FramePool.Init2
     
  ;   PRINT_STOP
     
;     stdcall   Timer.TimerInit, TIMER_HZ
     
 
 ;    STOP_POINT
     



 ;    stdcall   DMA.Init
     
    
     
  ;   stdcall   Process.Create
   ;  push      eax
    ; stdcall   Threads.Create, eax, PrintThread
    ; pop       eax
    ; stdcall   Threads.Create, eax, HellThread
    ; sti
    ; int       30h
     jmp       $

proc PrintThread

     ;STOP_POINT
     ;stdcall   Floppy.Init
     ;stdcall   Floppy.DetectDrives

     ; Чтение загрузочного сектора
     ;stdcall   FramePool.GetFreePage 
     ;stdcall   Pager.MapPage, 0x7C00 shr 12, eax,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
      
     ;stdcall  Floppy.Read, 0, 0, 1, 0x7C00

     ;stdcall  VGA.PutString, 0x7C00
     ; Асинхронное чтение нескольких секторов
     ;stdcall Floppy.ReadAsync, 0, 1, 4, buffer_address, callback_function

     ; Проверка статуса
     ;stdcall Floppy.GetStatus
     ;STOP_POINT
     ;stdcall FAT16.Init
     ;xchg bx, bx
    
     ;stdcall FAT16.Test
     ;stdcall   VGA.PrintDec, eax
     ;stdcall   VGA.PutString, Str.Goida
.InfLoop:
     ;stdcall   VGA.SetColor, dword[Color2], dword[Color1]
     ;stdcall   VGA.ClearScreen
     ;stdcall   VGA.PrintDec, [Timer.TimerMs]
     ;stdcall   VGA.PutString, Str.Goida
     ;inc       [Color1]
     and       [Color1], 00000111b
     inc       [Color2]
     and       [Color2], 00000111b
     ;stdcall   Timer.Sleep, 1000 
     int       30h
     jmp       .InfLoop
endp

proc HellThread

.InfLoop:
     ;stdcall   Timer.Sleep, 500
     ;stdcall   VGA.SetColor, VGA_COLOR_MAGENTA, VGA_COLOR_BLACK
     ;stdcall   VGA.ClearScreen
     ;stdcall   VGA.PutString, Str.Hell
     ;
     ;stdcall   Timer.Sleep, 500
     ;int       30h
     jmp       .InfLoop
endp


proc HexPrint
    push    ebx ecx edx eax
    mov     ecx, 8
@@:
    rol     ebx, 4
    mov     ax, bx
    and     al, 0000'0000_0000'1111b

    cmp     al, $0A
    sbb     al, $69
    das
  
    mov     ah, $07
    stosw

    loop    @B
    pop    eax edx ecx ebx
    ret
endp

proc TSS.Init uses eax

     mov       [TSS.SS0], KERNEL_DATA_SELECTOR
     mov       [TSS.ESP0], 0x200
     mov       [TSS.IOPB], TSSend - TSS
     mov       ax, TSS_SELECTOR
     ltr       ax
     ;stdcall   Pager.MapPage, eax, eax, eax

     ret       
endp

proc IRQ.Init 
     PIC1             equ 0x20   ; IO базовый адрес для master PIC */
     PIC2             equ 0xA0   ; IO базовый адрес для slave PIC */
     PIC1_COMMAND     equ PIC1
     PIC1_DATA        equ (PIC1+1)
     PIC2_COMMAND     equ PIC2
     PIC2_DATA        equ (PIC2+1)

     ;PIC_EOI          equ 0x20   ; End-of-interrupt command code */

     ICW1_ICW4        equ 0x01   ; ICW4 (not) needed */
     ICW1_SINGLE      equ 0x02   ; Single (cascade) mode */
     ICW1_INTERVAL4   equ 0x04   ; Call address interval 4 (8) */
     ICW1_LEVEL       equ 0x08   ; Level triggered (edge) mode */
     ICW1_INIT        equ 0x10   ; Initialization - required! */

     ICW4_8086        equ 0x01   ; 8086/88 (MCS-80/85) mode */
     ICW4_AUTO        equ 0x02   ; Auto (normal) EOI */
     ICW4_BUF_SLAVE   equ 0x08   ; Buffered mode/slave */
     ICW4_BUF_MASTER  equ 0x0C   ; Buffered mode/master */
     ICW4_SFNM        equ 0x10   ; Special fully nested (not) */

     ; PIC1
     IRQ_TIMER        equ 0x01
     IRQ_KEYB         equ 0x02
     IRQ_CASCADE      equ 0x04
     IRQ_FDC          equ 0x40

     ; PIC2
     IRQ_PS2          equ 0x10

.EntryPoint:

     mov       ecx, 0x1b
     rdmsr
     and       eax, 0xfffff7ff
     wrmsr

     mov     ecx, 10
     xor     edx, edx
     mov     esi, .Data
@@:  
     lodsw
     mov     dl, al
     mov     al, ah
     out     dx, al
     jcxz    $+2
     jcxz    $+2
     loop    @B
     ret

.Data:  ; Данные для отправки команд
        db      PIC1_COMMAND, ICW1_INIT + ICW1_ICW4
        db      PIC2_COMMAND, ICW1_INIT + ICW1_ICW4
        db      PIC1_DATA,    0x20
        db      PIC2_DATA,    0x28
        db      PIC1_DATA,    0x04
        db      PIC2_DATA,    0x02
        db      PIC1_DATA,    ICW4_8086 + ICW4_AUTO
        db      PIC2_DATA,    ICW4_8086 + ICW4_AUTO
        db      PIC1_DATA,    0;0xFF xor (IRQ_KEYB or IRQ_CASCADE or IRQ_TIMER)
        db      PIC2_DATA,    0;0xFF xor (IRQ_PS2)

endp
  
}

block(.initData){
Kernel.MaxMem dd 0xf0100000;Kernel.EndMem 
Color1    db 1;VGA_COLOR_BLACK
Color2    db 1;VGA_COLOR_BLUE
Str.Goida db "Hello OS x32 <3", 13, 0
Str.Hell db "Hello Word!!!", 13, 0
Str.A     db "A",0
}
block(.data){
}


;include 'Memory/Pager.asm'
;include 'Memory/FramePool.asm'
;include 'Memory/GS.asm'
;include 'Memory/KernelMemManager.asm'

;include 'Threads/Mutex.asm'
;include 'Threads/Process.asm'
;include 'Threads/ProcessManager.asm'
;include 'Threads/Sched.asm'
;include 'Threads/Threads.asm'

;include 'Interrupt/IntHeand.asm'
;include 'Interrupt/XInt.asm'
;include 'Interrupt/HardwInt.asm' 
;include 'Interrupt/KernPageFault.asm'
;include 'Interrupt/Timer.asm'

;include 'Drivers/VGA.asm'
;include 'Drivers/DMA.asm'
;include 'Drivers/Floppy.asm'
;include 'Drivers/FAT16.asm'

putBlocks .consts
putBlocks .text
putBlocks .initData
putBlocks .data
putBlocks .structs
Kernel.EndMem = $
IDE.Write $