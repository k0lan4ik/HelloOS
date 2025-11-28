        format binary as 'SYS'
        include 'proc16.inc'
        include 'Blocks.inc'

macro IDE.Write Value*
{
  local value, ofs, digit
  value = Value
  ofs = 15
  repeat 16
    digit = (value shr (ofs * 4)) and $F
    if digit > 9
      display digit + 'A' - 10
    else
      display digit + '0'
    end if
    if % = 8
      display ''''
    end if
    ofs = ofs - 1
  end repeat
  display 13,10
}

define DEBUG
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

; чтобы всё поместилось сделаю загрузку на 4000 ядра
block(.consts) {
GDT_NULL_SELECTOR     equ 0x00
KERNEL_CODE_SELECTOR  equ 0x08
KERNEL_DATA_SELECTOR  equ 0x10
USER_CODE_SELECTOR    equ 0x18
USER_DATA_SELECTOR    equ 0x20
TSS_SELECTOR          equ 0x18
Options.Kernel.Base     equ     $0600
Options.Kernel.HierHalf equ     $F0000000
}
  include 'Structs.asm'
  

block (.text) {
use16
org Options.Kernel.Base
RealEntry:
     
        mov     si, dx
        shl     esi, 16
        mov     si, ax 

        xor     ax, ax
        mov     ss, ax
        mov     sp, Options.Kernel.Base

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
        jmp GotoProtected 


proc CreateGDT_IDT

     xor       ax, ax
     mov       es, ax
     mov       di, Real.GDT
     mov       cx, 6 * 4
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


     mov       di, 8 + Real.GDT

     ; Код ядра
     mov       eax, $000FFFFF
     xor       ebx, ebx 
     mov       cx,  1_1_0_0_0000_1_00_1_1010b  ;G:D/B:L:AVL:NotNeed:P:DPL:S:Type
     call      CreateDescriptor

     ; Тоже, но для данных ядра
     mov       eax, $000FFFFF
     xor       ebx, ebx
     mov       cx,  1_1_0_0_0000_1_00_1_0010b  ;G:D/B:L:AVL:NotNeed:P:DPL:S:Type
     call      CreateDescriptor
     
     mov       eax, TSSend - TSS - 1
     mov       ebx, TSS     
     mov       cx,  0_0_0_0_0000_1_00_0_1001b  ;G:D/B:L:AVL:NotNeed:P:DPL:S:Type
     call      CreateDescriptor

     ; Пользовательский код
     mov       eax, $000FFFFF
     xor       ebx, ebx
     mov       cx,  1_1_0_0_0000_1_11_1_1010b  ;G:D/B:L:AVL:NotNeed:P:DPL:S:Type
     call      CreateDescriptor

     ; Тоже, но для данных Пользователя
     xor       eax, eax 
     mov       ebx, $000FFFFF
     mov       cx,  1_1_0_0_0000_1_11_1_0010b  ;G:D/B:L:AVL:NotNeed:P:DPL:S:Type
     call      CreateDescriptor

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

GotoProtected:   
       
     mov       di,  Options.Kernel.Base + 4
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
     cmp       ebp, 5
     jge       .E820f
     add       di, 24
.SkipEntry:
        test      ebx, ebx              
        jne       .E820lp
.E820f:
        mov       [es:Options.Kernel.Base], ebp
     

     
     cli

     mov word  [es:Real.GDTptr], GDTend - GDT
     mov dword [es:Real.GDTptr + 2], Real.GDT
     lgdt      [es:Real.GDTptr]

     ; Загрузка IDT
     mov word  [es:Real.IDTptr], IDTend - IDT 
     mov dword [es:Real.IDTptr + 2], Real.IDT
     lidt      [es:Real.IDTptr]         
     
     
     mov       eax, cr0
     or        al,  1
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
     mov       ecx, 1024
     mov       eax, 0x00000002 ; Supervisor, R/W, Not Present
     rep stosd

     mov       dword [PageDirectory + 0x3FF * 4], PageDirectory or 0x019
     mov       dword [PageDirectory + 0x3F8 * 4], (P3 - Options.Kernel.HierHalf) or 0x019
     mov       dword [PageDirectory + 0x200 * 4], (P2 - Options.Kernel.HierHalf) or 0x001
     
     IDE.Write  PageDirectory

     mov       edi, PageTable1
     mov       ecx, 1024
     xor       ebx, ebx

.MapFirst4MB:
     lea       eax, [edi + ecx*4 - 4]
     mov       ebx, ecx
     dec       ebx
     shl       ebx, 12
     or        ebx, 0x003

     mov dword [eax], ebx 
     loop      .MapFirst4MB

     mov dword [PageDirectory], PageTable1 or 0x003

     mov dword [PageDirectory + (Options.Kernel.HierHalf shr 22) * 4], PageTable1 or 0x003

     mov       eax, PageDirectory
     mov       cr3, eax

     mov       eax, cr0
     or        eax, 0x80000000
     mov       cr0, eax

     add       dword[ebp + 4], Options.Kernel.HierHalf 

     pop       ebx eax ecx edi ebp
     ret
endp

ProtectedEntry:
     use32

     mov       ax, KERNEL_DATA_SELECTOR
     mov       ds, ax
     mov       es, ax
     mov       ss, ax
     mov       esp, Options.Kernel.Base

     mov       ax, 0
     mov       fs, ax
     mov       gs, ax
     call      Paging.Init

org Options.Kernel.HierHalf + $
     
     
     mov dword [GDTptr + 2], GDT
     lgdt      [GDTptr]

     
     mov dword [IDTptr + 2], IDT
     lidt      [IDTptr]         

     jmp       KERNEL_CODE_SELECTOR:@F
     
@@:
     mov       ax, KERNEL_DATA_SELECTOR      
     mov       ds, ax
     mov       es, ax
     mov       fs, ax
     mov       gs, ax
     mov       ss, ax
     add       esp, Options.Kernel.HierHalf 
     
     mov dword [0xfffff000], 0x00000002
     
     call      Interrupt.FaultsInit
     call      IRQ.Init
     sti


     call      TSS.Init
     stdcall   FramePool.Init1

     ;Инициализация страници под procdata for this processor
     stdcall   FramePool.GetFreePage 
     stdcall   Pager.MapPage, 0xFF000, eax,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     
     ;Инициализация страниц под первые 12 потоков
     stdcall   FramePool.GetFreePage 
     stdcall   Pager.MapPage, 0xFE000, eax,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     
     stdcall   FramePool.GetFreePage 
     stdcall   Pager.MapPage, 0xFE001, eax,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     
     stdcall   FramePool.GetFreePage 
     stdcall   Pager.MapPage, 0xFF002, eax,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     
     ;Страница для первых N процессов
     stdcall   FramePool.GetFreePage 
     stdcall   Pager.MapPage, 0xFF102, eax,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     
     ;тут вместо регистра надо адрес таблицы и IDT и GDT
     stdcall   Pager.MapPage, 0xFF100, eax,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     
     ;Переназначение таблицы
     
     
     ; E820 memory map
     stdcall   Pager.MapPage, 0xFF101, 0x2, AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC

     ; page fault handler table
     stdcall   FramePool.GetFreePage 
     stdcall   Pager.MapPage, 0xFF102, eax,  AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     
     stdcall   GS.Init

     stdcall   IntHeand.Init
     stdcall   XInt.Init
     stdcall   HardwInt.Init
     stdcall   KernPageFault.Init

     stdcall   KernelMemManager.Init
     STOP_POINT     

     stdcall   ProcessManager.Init
     stdcall   Sched.Init
     
     stdcall   FramePool.Init2

     ;timerzzz

     mov       ebx, 100000 ; 100 KHz
     call      Timer.Init
     call      PS2.Init

     call      ScreenMode03.Clear
     
     xor       edx, edx
     mov       ecx, 4
     call      ScreenMode03.SetCursor
     
     mov       esi, Str.Goida
     call      ScreenMode03.PrintString
.WriteLoop:


     push      [ScreenMode03.CursorX]
     push      [ScreenMode03.CursorY]

     xor       edx, edx
     xor       ecx, ecx
     call      ScreenMode03.SetCursor

     mov       ebx, [PS2.Mouse.X]
     call      HexPrint

     xor       edx, edx
     mov       ecx, 1
     call      ScreenMode03.SetCursor

     mov       ebx, [PS2.Mouse.Y]
     call      HexPrint

     xor       edx, edx
     mov       ecx, 2
     call      ScreenMode03.SetCursor
     mov       ecx, [es:Options.Kernel.HierHalf + Options.Kernel.Base]
     test      ecx, ecx
     jz        .Zoc
.PrintMem:
     mov       eax, ecx
     push      ecx
     dec       eax
     mov       edx, 24
     mul       edx
     xchg      edx, eax
     mov       ebx, [es:Options.Kernel.HierHalf + Options.Kernel.Base + 4 + edx]
     call      HexPrint

     mov       al, ' '
     call      ScreenMode03.PrintSymbol 

     add       edx, 8
     mov       ebx, [es:Options.Kernel.HierHalf + Options.Kernel.Base + 4 + edx]
     call      HexPrint
     
     mov       al, ' '
     call      ScreenMode03.PrintSymbol 

     pop       ecx
     loop      .PrintMem
      
.Zoc:


     xor       edx, edx
     mov       ecx, 3
     call      ScreenMode03.SetCursor

     mov       ebx, [Timer.TimerMs]
     call      HexPrint
     

     pop       [ScreenMode03.CursorY]
     pop       [ScreenMode03.CursorX]

     mov       esi, [PS2.Mouse.X]
     mov       edi, [PS2.Mouse.Y]

     sub       esi, [Mouse.X]
     add       [Mouse.X], esi
     sar       esi, 1
     sbb       [Mouse.X], 0


     
     sub       edi, [Mouse.Y]
     add       [Mouse.Y], edi
     sar       edi, 1
     sbb       [Mouse.Y], 0

     call      ScreenMode03.DrawMouseCursor
     
     mov       eax, [PS2.KeyBufferTail]
     
     mov       edx, [PS2.KeyBufferHead]
     cmp       eax, edx
     je        .WriteLoop
     ;IDE.Write $%
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


proc HexPrint
    pusha
    mov     ecx, 8
@@:
    rol     ebx, 4
    mov     ax, bx
    and     al, 0000'0000_0000'1111b

    cmp     al, $0A
    sbb     al, $69
    das
  
    mov     ah, $0E
    call    ScreenMode03.PrintSymbol

    loop    @B
    popa
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
        db      PIC1_DATA,    ICW4_8086
        db      PIC2_DATA,    ICW4_8086
        db      PIC1_DATA,    0xFF xor (IRQ_KEYB or IRQ_CASCADE or IRQ_TIMER)
        db      PIC2_DATA,    0xFF xor (IRQ_PS2)

endp
  
}

block(.initData){
Kernel.MaxMem dd Kernel.EndMem 
Str.Goida db "Hello OS x32", 13, 10, ">", 0
}
block(.data){
Mouse.X dd ?
Mouse.Y dd ?
Discriptor.Index dd ?
}
include 'Interrupt.asm'
include 'Timer.asm'
include 'ScreenMode03.asm'
include 'PS2.asm'

include 'Memory/Pager.asm'
include 'Memory/FramePool.asm'
include 'Memory/GS.asm'
include 'Memory/KernelMemManager.asm'

include 'Threads/Mutex.asm'
include 'Threads/Process.asm'
include 'Threads/ProcessManager.asm'
include 'Threads/Sched.asm'
include 'Threads/Threads.asm'


putBlocks .consts
putBlocks .text
putBlocks .initData
putBlocks .data
putBlocks .structs
Kernel.EndMem = $
IDE.Write Kernel.EndMem