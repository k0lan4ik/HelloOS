include 'macro\proc32.inc'

use32


block(.consts) {
SCREEN_MODE_03_MEMORY equ 0xC00B8000 

ScreenMode03.Rows = 25
ScreenMode03.Colms = 80
}

block(.text) {
proc ScreenMode03.Clear uses edi ecx es;{ Очистка экрана
     mov  cx, KERNEL_DATA_SELECTOR
     mov  es, cx
          
     mov  ecx, ScreenMode03.Rows * ScreenMode03.Colms
     mov  al,' '            
     mov  ah,[ScreenMode03.Attribute]

     mov  edi, SCREEN_MODE_03_MEMORY;0xB0000     
     
     rep stosw

     ret                      
endp


proc ScreenMode03.SetCursor uses eax ecx edx ebx;{ Устанавливает курсор в заданую позицию
; EDX - координата по X
; ECX - координата по Y
     xchg      eax, edx
     xor       edx, edx
     mov       ebx, ScreenMode03.Colms
     div       ebx
     mov       [ScreenMode03.CursorX], edx

     add       eax, ecx
     xor       edx, edx
     mov       ebx, ScreenMode03.Rows
     div       ebx
     mov       [ScreenMode03.CursorY], edx
     
     ret
endp

proc ScreenMode03.PrintSymbol ;{ Выводит символ в консоль
; AL - Номер символа по ASCII
     movzx eax, al
     call dWord[.Table + eax * 4]
     ret

.Ignore:
     ret

.Print:
     push      es ecx edi edx
     mov       cx, KERNEL_DATA_SELECTOR
     mov       es, cx 
     mov       ecx, [ScreenMode03.CursorY]

     mov       ah, [ScreenMode03.Attribute]
     xchg      eax, ecx

     mov       edx, ScreenMode03.Colms
     mul       edx
     
     mov       edi, [ScreenMode03.CursorX]      
     add       edi, eax
     shl       edi, 1
     add       edi, SCREEN_MODE_03_MEMORY
     xchg      eax, ecx

     stosw

     inc       [ScreenMode03.CursorX]
     cmp       [ScreenMode03.CursorX], ScreenMode03.Colms
     jb        @F
     mov       [ScreenMode03.CursorX], 0
     call      .GotoNextStr
@@:
     pop       edx edi ecx es
     ret

.GotoStartStr:
     mov       [ScreenMode03.CursorX], 0
     ret

.GotoNextStr: 
     mov       [ScreenMode03.CursorX], 0
     inc       [ScreenMode03.CursorY]
     cmp       [ScreenMode03.CursorY], ScreenMode03.Rows
     jb @F
     push      esi edi ecx eax
     mov       [ScreenMode03.CursorY ], ScreenMode03.Rows - 1
     
     mov       al, ' '
     mov       ah, [ScreenMode03.Attribute] 
     mov       edi, SCREEN_MODE_03_MEMORY
     mov       esi, ScreenMode03.Colms * 2
     add       esi, SCREEN_MODE_03_MEMORY
     mov       ecx, ScreenMode03.Colms * (ScreenMode03.Rows - 1)
     rep movsw
     mov       ecx, ScreenMode03.Colms
     rep stosw 
     pop       eax ecx edi esi
@@: 
     ret

.Table:
     dd .Ignore
     times (9 - 0) dd .Print
     dd .GotoStartStr
     times (12 - 10) dd .Print
     dd .GotoNextStr
     times (255 - 13) dd .Print 

endp

proc ScreenMode03.PrintString uses esi eax;{ Выводит cтроку, кончающуюся на 0 в консоль
; DS:ESI
@@:    
     lodsb
     test      al, al
     jz        @F
     call      ScreenMode03.PrintSymbol
     jmp       @B
@@:     
     ret
endp

proc ScreenMode03.DrawMouseCursor uses es eax edx ecx
     ;esi - dx
     ;edi - dy
     ;xchg      bx, bx
     mov       ax, KERNEL_DATA_SELECTOR
     mov       es, ax
     test      esi, edi
     je        .EndProc    


     mov       eax, [ScreenMode03.MouseCursorY]
     mov       edx, ScreenMode03.Colms
     mul       edx
     add       eax, [ScreenMode03.MouseCursorX]
     shl       eax, 1
     inc       eax

     mov       cl, [ScreenMode03.Attribute]
     mov       [es:eax + SCREEN_MODE_03_MEMORY], cl 

     add       [ScreenMode03.MouseCursorX], esi
     cmp       [ScreenMode03.MouseCursorX], 0
     jnl       @F
     mov       [ScreenMode03.MouseCursorX], 0
 @@:    
     cmp       [ScreenMode03.MouseCursorX], ScreenMode03.Colms - 1
     jng        @F
     mov       [ScreenMode03.MouseCursorX], ScreenMode03.Colms - 1    
@@: 
     
     add       [ScreenMode03.MouseCursorY], edi
     cmp       [ScreenMode03.MouseCursorY], 0
     jnl       @F
     mov       [ScreenMode03.MouseCursorY], 0
@@:    
     cmp       [ScreenMode03.MouseCursorY], ScreenMode03.Rows - 1
     jng        @F
     mov       [ScreenMode03.MouseCursorY], ScreenMode03.Rows - 1     
@@: 

.EndProc:

     mov       eax, [ScreenMode03.MouseCursorY]
     mov       edx, ScreenMode03.Colms
     mul       edx
     add       eax, [ScreenMode03.MouseCursorX]
     shl       eax, 1
     inc       eax

     mov       cl, [ScreenMode03.MouseAttribute]
     mov       [es:eax + SCREEN_MODE_03_MEMORY], cl 

     ret       
endp
}
block(.initData){

ScreenMode03.Attribute    db $1e
ScreenMode03.MouseAttribute db $4f
}
block(.data){
ScreenMode03.CursorX      dd ?
ScreenMode03.CursorY      dd ?
ScreenMode03.Selector     dw ?

ScreenMode03.MouseCursorX      dd ?
ScreenMode03.MouseCursorY      dd ?
}

