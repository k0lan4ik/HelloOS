        format binary as 'SYS'
        include 'proc16.inc'




; чтобы всё поместилось сделаю загрузку на 4000 ядра

Options.Kernel.SDZSegment        equ     $0400

RealEntry:
     
        mov     si, dx
        shl     esi, 16
        mov     si, ax 

        xor     ax, ax
        mov     ss, ax
        mov     sp, (Options.Kernel.SDZSegment) shl 4
        
        cli
        in      al, 92h
        or      al, 2
        out     92h, al
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
        
        call CreateGDT_IDT
        cli
        mov eax, cr0
        or  al,  1
        mov cr0, eax
        jmp 0x0008:0x0000



; si - размер ос
proc CreateGDT_IDT

     mov       ax, 0x0060
     mov       es, ax

     xor       di, di
     mov       cx, 0x100 / 2
     xor       ax, ax
     rep stosw


     mov       cx, 256
@@:
     mov       eax, Interrupt.Void
     stosw

     mov       ax, 0x8
     stosw

     mov       ah, 1_00_0_1111b
     xor       al, al
     stosw

     shr       eax, 16
     stosw
     loop      @B


     mov       di, 8

     ; Код ядра
     mov       eax, esi
     mov       ebx, ProtectedEntry + Options.Kernel.SDZSegment shl 4
     mov       cx,  0_1_0_0_0000_1_00_1_1000b  ;G:D/B:L:AVL:NotNeed:P:DPL:S:Type
     call      CreateDescriptor

     ; Тоже, но для данных ядра
     mov       eax, esi
     mov       ebx, ProtectedEntry + Options.Kernel.SDZSegment shl 4
     mov       cx,  0_1_0_0_0000_1_00_1_0010b  ;G:D/B:L:AVL:NotNeed:P:DPL:S:Type
     call      CreateDescriptor

     ;Cтек
     mov       eax, 0x2500
     mov       ebx, (Options.Kernel.SDZSegment) shl 4 - 0x2500
     mov       cx,  0_1_0_0_0000_1_00_1_0010b  ;G:D/B:L:AVL:NotNeed:P:DPL:S:Type
     call      CreateDescriptor
     
     ;Сама GDT    
     mov       eax, 0x0100
     mov       ebx, 0x0600
     mov       cx,  0_1_0_0_0000_1_00_1_0010b  ;G:D/B:L:AVL:NotNeed:P:DPL:S:Type
     call      CreateDescriptor
     
     ;IDT
     mov       eax, 0x800
     mov       ebx, 0x0700
     mov       cx,  0_1_0_0_0000_1_00_1_0010b  ;G:D/B:L:AVL:NotNeed:P:DPL:S:Type
     call      CreateDescriptor


     xor       ax, ax
     mov       es, ax
     
     mov word  [es:0x0580], 0x00FF
     mov dword [es:0x0582], 0x0600
     lgdt      [es:0x0580]

     ; Загрузка IDT
     mov word  [es:0x0586], 0x7FF
     mov dword [es:0x0588], 0x0700
     lidt      [es:0x0586]
   
     ret   
endp 

;==============================================================================}
proc CreateDescriptor;{Создание дескриптора в реальном режиме
; --------------------------------------------------------
; EAX - Лимит 20
; EBX - Адрес 32
; CX  - Конфигурация
; ES:DI - Указатель на элемент GDT
; --------------------------------------------------------

     stosw ; limit

     xchg  eax, ebx
     stosw ; address 0..15

     shr   eax, 16
     stosb ; addr 16..23

     xchg  eax, ebx
     mov   al, cl
     stosb ; config low

     shr   eax, 16
     or    al, ch
     stosb ; config + limit

     xchg  eax, ebx
     shr   ax, 8
     stosb ; addr 24..31

    ret
endp


include 'macro\proc32.inc'

ProtectedEntry:
     org 0
     use32

     mov        ax, 0x10
     mov        ds, ax
     mov        es, ax
     

     mov        ax, 0x18
     mov        ss, ax
     mov        esp, 0x2500

     mov        ax, 0
     mov        fs, ax
     mov        gs, ax

     sti

     mov        edi, 0x30
     mov        eax, 0x7FFF
     mov        ebx, 0x0B8000     
     mov        cx,  0_1_0_0_0000_1_00_1_0010b  ;G:D/B:L:AVL:NotNeed:P:DPL:S:Type
     call       CreateDescriptor32

     mov        [ScreenMode03.Selector], 0x30
     
     call       ScreenMode03.Clear

     call       IRQ.Init
     call       PS2.Init
     
     mov        esi, Str.Goida
     call       ScreenMode03.PrintString

.WriteLoop:
     mov       eax, [PS2.KeyBufferTail]
     mov       edx, [PS2.KeyBufferHead]
     cmp       eax, edx
     je        .WriteLoop
     
     mov       dl,  [PS2.KeyBuffer + eax]
     inc       eax
     and       eax, 63
     mov       [PS2.KeyBufferTail], eax
     
     test      dl, dl
     jne       @F
     inc       eax
     and       eax, 63
     mov       [PS2.KeyBufferTail], eax
     jmp       .WriteLoop
@@:
     xchg      al, dl
     call      ScreenMode03.PrintSymbol
     jmp       .WriteLoop

;==============================================================================}
proc CreateDescriptor32 uses es;{Создание дескриптора в защищённом режиме
; --------------------------------------------------------
; EAX - Лимит 20
; EBX - Адрес 32
; CX  - Конфигурация
; EDI - Указатель на элемент GDT
; --------------------------------------------------------
     push  0x20
     pop   es    
     
     stosw ; limit

     xchg  eax, ebx
     stosw ; address 0..15

     shr   eax, 16
     stosb ; addr 16..23

     xchg  eax, ebx
     mov   al, cl
     stosb ; config low

     shr   eax, 16
     or    al, ch
     stosb ; config + limit

     xchg  eax, ebx
     shr   ax, 8
     stosb ; addr 24..31

    ret
endp

proc HexPrint
    mov     ecx, 4
@@:
    rol     bx, 4
    mov     ax, bx
    and     al, 0000'0000_0000'1111b

    cmp     al, $0A
    sbb     al, $69
    das
  
    mov     ah, $0E
    call    ScreenMode03.PrintSymbol

    loop    @B
    ret
endp

proc IRQ.Init 
     PIC1             equ 0x20   ; IO базовый адрес для master PIC */
     PIC2             equ 0xA0   ; IO базовый адрес для slave PIC */
     PIC1_COMMAND     equ PIC1
     PIC1_DATA        equ (PIC1+1)
     PIC2_COMMAND     equ PIC2
     PIC2_DATA        equ (PIC2+1)

     PIC_EOI          equ 0x20   ; End-of-interrupt command code */

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
     mov     esi, .data
@@:  
     lodsw
     mov     dl, al
     mov     al, ah
     out     dx, al
     jcxz    $+2
     jcxz    $+2
     loop    @B
     ret

.data:  ; Данные для отправки команд
        db      PIC1_COMMAND, ICW1_INIT + ICW1_ICW4
        db      PIC2_COMMAND, ICW1_INIT + ICW1_ICW4
        db      PIC1_DATA,    0x20
        db      PIC2_DATA,    0x28
        db      PIC1_DATA,    0x04
        db      PIC2_DATA,    0x02
        db      PIC1_DATA,    ICW4_8086
        db      PIC2_DATA,    ICW4_8086
        db      PIC1_DATA,    0xFF xor (IRQ_KEYB)
        db      PIC2_DATA,    0xFF xor (IRQ_PS2)

endp

Str.Goida db "Hello OS x32", 13, 10, ">", 0

include 'Interrupt.asm'
include 'ScreenMode03.asm'
include 'PS2.asm'
