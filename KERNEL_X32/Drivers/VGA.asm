block(.consts){
    ; VGA ports
    VGA_CRTC_INDEX  equ 0x3D4
    VGA_CRTC_DATA   equ 0x3D5
    VGA_ATTRIB_INDEX equ 0x3C0
    VGA_ATTRIB_DATA_WRITE equ 0x3C0
    VGA_ATTRIB_DATA_READ equ 0x3C1
    VGA_MISC_OUTPUT equ 0x3C2
    VGA_SEQ_INDEX   equ 0x3C4
    VGA_SEQ_DATA    equ 0x3C5
    VGA_GC_INDEX    equ 0x3CE
    VGA_GC_DATA     equ 0x3CF
    VGA_DAC_READ_INDEX equ 0x3C7
    VGA_DAC_WRITE_INDEX equ 0x3C8
    VGA_DAC_DATA    equ 0x3C9
    
    ; VGA memory
    VGA_TEXT_MEM    equ 0xB8000 or Options.Kernel.HierHalf
    VGA_WIDTH       equ 80
    VGA_HEIGHT      equ 25
    VGA_TOTAL_CELLS equ VGA_WIDTH * VGA_HEIGHT
    
    ; Colors
    VGA_COLOR_BLACK         equ 0x00
    VGA_COLOR_BLUE          equ 0x01
    VGA_COLOR_GREEN         equ 0x02
    VGA_COLOR_CYAN          equ 0x03
    VGA_COLOR_RED           equ 0x04
    VGA_COLOR_MAGENTA       equ 0x05
    VGA_COLOR_BROWN         equ 0x06
    VGA_COLOR_LIGHT_GRAY    equ 0x07
    VGA_COLOR_DARK_GRAY     equ 0x08
    VGA_COLOR_LIGHT_BLUE    equ 0x09
    VGA_COLOR_LIGHT_GREEN   equ 0x0A
    VGA_COLOR_LIGHT_CYAN    equ 0x0B
    VGA_COLOR_LIGHT_RED     equ 0x0C
    VGA_COLOR_LIGHT_MAGENTA equ 0x0D
    VGA_COLOR_YELLOW        equ 0x0E
    VGA_COLOR_WHITE         equ 0x0F
    
}


block(.text){
proc VGA.Init
    
    ; Initialize hardware
    stdcall VGA.SetMode3
    
    ; Initialize cursor position
    mov word [VGA.CursorX], 0
    mov word [VGA.CursorY], 0
    
    ; Default colors: light gray on black
    mov byte [VGA.ColorFg], VGA_COLOR_LIGHT_GRAY
    mov byte [VGA.ColorBg], VGA_COLOR_BLACK
    mov byte [VGA.Blink], 0
    mov byte [VGA.Bold], 0
    mov byte [VGA.IsEnabled], 1
    
    ; Enable cursor
    stdcall VGA.EnableCursor
    
    ; Clear screen
    stdcall VGA.ClearScreen
    
    ret
endp

proc VGA.SetMode3 uses esi
    pushf
    ; Disable interrupts
    cli
    
    ; Write Misc Output Register
    mov dx, VGA_MISC_OUTPUT
    mov al, 0x67
    out dx, al
    
    ; Sequencer: reset and synchronous reset
    mov dx, VGA_SEQ_INDEX
    mov al, 0x00
    out dx, al
    mov dx, VGA_SEQ_DATA
    mov al, 0x03
    out dx, al
    
    ; Wait for vertical retrace
    stdcall VGA.WaitRetrace
    
    ; Write sequencer registers
    mov esi, VGA_MODE3_SEQ_REGISTERS
    mov cx, 5
    mov al, 0
.WriteSeq:
    mov dx, VGA_SEQ_INDEX
    out dx, al
    inc dx
    movsb
    inc al
    loop .WriteSeq
    
    ; End reset
    mov dx, VGA_SEQ_INDEX
    mov al, 0x00
    out dx, al
    mov dx, VGA_SEQ_DATA
    mov al, 0x03
    out dx, al
    
    ; Unlock CRTC registers
    mov dx, VGA_CRTC_INDEX
    mov al, 0x03
    out dx, al
    inc dx
    in al, dx
    and al, 0x7F
    out dx, al
    
    dec dx
    mov al, 0x11
    out dx, al
    inc dx
    in al, dx
    and al, 0x7F
    out dx, al
    
    ; Write CRTC registers
    mov esi, VGA_MODE3_CRTC_REGISTERS
    mov cx, 25
    mov al, 0
.WriteCrtc:
    mov dx, VGA_CRTC_INDEX
    out dx, al
    inc dx
    movsb
    inc al
    loop .WriteCrtc
    
    ; Write Graphics Controller registers
    mov esi, VGA_MODE3_GC_REGISTERS
    mov cx, 9
    mov al, 0
.WriteGC:
    mov dx, VGA_GC_INDEX
    out dx, al
    inc dx
    movsb
    inc al
    loop .WriteGC
    
    ; Write Attribute Controller registers
    mov esi, VGA_MODE3_ATTR_REGISTERS
    mov cx, 21
    
    ; Reset attribute controller
    mov dx, VGA_ATTRIB_DATA_READ
    in al, dx
    
    mov dx, VGA_ATTRIB_INDEX
    mov al, 0x00
.WriteAttr:
    out dx, al
    movsb
    inc al
    loop .WriteAttr
    
    ; Re-enable video
    mov dx, VGA_ATTRIB_INDEX
    mov al, 0x20
    out dx, al
    
    ; Re-enable interrupts
    popf
    

    ret
endp

proc VGA.WaitRetrace
    push edx
    push eax
    
    mov dx, 0x3DA
    
.WaitRetraceEnd:
    in al, dx
    test al, 0x08
    jnz .WaitRetraceEnd
    
.WaitRetraceStart:
    in al, dx
    test al, 0x08
    jz .WaitRetraceStart
    
    pop eax
    pop edx
    ret
endp

proc VGA.ClearScreen
    push eax
    push ecx
    push edi
    
    ; Calculate attribute
    mov al, [VGA.ColorBg]
    shl al, 4
    or al, [VGA.ColorFg]
    test byte [VGA.Blink], 1
    jz .no_blink
    or al, 0x80
.no_blink:
    test byte [VGA.Bold], 1
    jz .no_bold
    or al, 0x08
.no_bold:
    
    ; Fill screen with spaces
    mov ah, al          ; Attribute in AH
    mov al, ' '         ; Space character
    mov ecx, VGA_TOTAL_CELLS
    mov edi, VGA_TEXT_MEM
    cld
    rep stosw
    
    ; Reset cursor
    mov word [VGA.CursorX], 0
    mov word [VGA.CursorY], 0
    stdcall VGA.UpdateCursor
    
    pop edi
    pop ecx
    pop eax
    ret
endp

proc VGA.SetColor fg:BYTE, bg:BYTE
    mov al, [fg]
    mov [VGA.ColorFg], al
    mov al, [bg]
    mov [VGA.ColorBg], al
    ret
endp

proc VGA.GetCursorPos
    mov ax, [VGA.CursorX]
    shl eax, 16
    mov ax, [VGA.CursorY]
    ret
endp

proc VGA.SetCursorPos x, y
    push eax
    
    mov ax, [x]
    cmp ax, VGA_WIDTH
    jb .x_ok
    mov ax, VGA_WIDTH - 1
.x_ok:
    mov [VGA.CursorX], ax
    
    mov ax, [y]
    cmp ax, VGA_HEIGHT
    jb .y_ok
    mov ax, VGA_HEIGHT - 1
.y_ok:
    mov [VGA.CursorY], ax
    
    stdcall VGA.UpdateCursor
    
    pop eax
    ret
endp

proc VGA.UpdateCursor
    push eax
    push edx
    
    ; Calculate linear position
    mov ax, [VGA.CursorY]
    mov dx, VGA_WIDTH
    mul dx
    add ax, [VGA.CursorX]
    
   
    
    pop edx
    pop eax
    ret
endp

proc VGA.EnableCursor
    push   edx eax
    ; Start scanline = 13, end scanline = 15 (standard cursor)
    mov dx, VGA_CRTC_INDEX
    mov al, 0x0A
    out dx, al
    inc dx
    in al, dx
    and al, 0xC0        ; Clear cursor start bits
    or al, 13           ; Start at scanline 13
    out dx, al
    
    dec dx
    mov al, 0x0B
    out dx, al
    inc dx
    in al, dx
    and al, 0xE0        ; Clear cursor end bits
    or al, 15           ; End at scanline 15
    out dx, al
    pop eax edx
    ret
endp

proc VGA.DisableCursor
    push eax edx
    ; Set cursor start > cursor end to hide
    mov dx, VGA_CRTC_INDEX
    mov al, 0x0A
    out dx, al
    inc dx
    mov al, 0x20        ; Bit 5 = disable cursor
    out dx, al
    
    pop edx eax
    ret
endp

proc VGA.PutChar char:BYTE
    push ebx ecx edx edi

    cmp byte [char], 0x0A    ; Line feed
    je .line_feed
    cmp byte [char], 0x0D    ; Carriage return
    je .carriage_return
    cmp byte [char], 0x08    ; Backspace
    je .backspace
    cmp byte [char], 0x09    ; Tab
    je .tab
    cmp byte [char], 0x07    ; Bell
    je .bell
    
    movzx eax, word [VGA.CursorY]
    mov ecx, VGA_WIDTH
    mul ecx
    movzx ecx, word [VGA.CursorX]
    add eax, ecx
    shl eax, 1            ; *2 for char+attr
    
    mov dl, [VGA.ColorBg]
    shl dl, 4
    or dl, [VGA.ColorFg]
    test byte [VGA.Blink], 1
    jz .no_blink2
    or dl, 0x80
.no_blink2:
    test byte [VGA.Bold], 1
    jz .no_bold2
    or dl, 0x08
.no_bold2:
    
    mov edi, VGA_TEXT_MEM
    add edi, eax
    mov al, [char]
    mov [edi], al
    inc edi
    mov [edi], dl
    
    inc word [VGA.CursorX]
    cmp word [VGA.CursorX], VGA_WIDTH
    jb .update_cursor
    jmp .line_feed
    
.update_cursor:
    stdcall VGA.UpdateCursor
    jmp .done
    
.line_feed:
    inc word [VGA.CursorY]
    jmp .check_scroll
    
.carriage_return:
    mov word [VGA.CursorX], 0
    jmp .update_cursor
    
.backspace:
    cmp word [VGA.CursorX], 0
    je .backspace_at_start
    dec word [VGA.CursorX]
    jmp .update_cursor
.backspace_at_start:
    cmp word [VGA.CursorY], 0
    je .update_cursor
    dec word [VGA.CursorY]
    mov word [VGA.CursorX], VGA_WIDTH - 1
    jmp .update_cursor
    
.tab:
    mov ax, [VGA.CursorX]
    add ax, 8
    and ax, 0xFFF8    ; Round down to nearest multiple of 8
    cmp ax, VGA_WIDTH
    jb .tab_ok
    mov ax, VGA_WIDTH - 1
.tab_ok:
    mov [VGA.CursorX], ax
    jmp .update_cursor
    
.bell:
    ; TODO: Implement beep sound via PC speaker
    jmp .done
    
.check_scroll:
    cmp word [VGA.CursorY], VGA_HEIGHT
    jb .update_cursor
    stdcall VGA.ScrollUp
    mov word [VGA.CursorY], VGA_HEIGHT - 1
    jmp .update_cursor
    
.done:
    pop edi
    pop edx
    pop ecx
    pop ebx
    ret
endp

proc VGA.PutString string
    push esi
    push eax
    
    mov esi, [string]
    
.string_loop:
    mov al, [esi]
    test al, al
    jz .done
    
    
    stdcall VGA.PutChar, eax
    
    inc esi
    jmp .string_loop
    
.done:
    pop eax
    pop esi
    ret
endp

proc VGA.ScrollUp
    push esi
    push edi
    push ecx
    
    ; Move all lines up by one
    mov esi, VGA_TEXT_MEM + (VGA_WIDTH * 2)    ; Start of line 1
    mov edi, VGA_TEXT_MEM                      ; Start of line 0
    mov ecx, (VGA_WIDTH * (VGA_HEIGHT - 1)) / 2 ; Words to move
    rep movsd
    
    ; Clear last line
    mov edi, VGA_TEXT_MEM + (VGA_WIDTH * (VGA_HEIGHT - 1) * 2)
    mov ecx, VGA_WIDTH
    
    ; Calculate attribute for blank line
    mov al, [VGA.ColorBg]
    shl al, 4
    or al, [VGA.ColorFg]
    test byte [VGA.Blink], 1
    jz .no_blink3
    or al, 0x80
.no_blink3:
    test byte [VGA.Bold], 1
    jz .no_bold3
    or al, 0x08
.no_bold3:
    
    mov ah, al
    mov al, ' '
.clear_last:
    stosw
    loop .clear_last
    
    pop ecx
    pop edi
    pop esi
    ret
endp

proc VGA.GetCharAt pos_x, pos_y
    push ebx
    
    ; Calculate position
    movzx eax, word [pos_y]
    mov ecx, VGA_WIDTH
    mul ecx
    movzx ecx, word [pos_x]
    add eax, ecx
    shl eax, 1
    
    ; Read character
    mov ebx, VGA_TEXT_MEM
    add ebx, eax
    movzx eax, byte [ebx]
    
    pop ebx
    ret
endp

proc VGA.GetAttrAt pos_x, pos_y
    push ebx
    
    ; Calculate position
    movzx eax, word [pos_y]
    mov ecx, VGA_WIDTH
    mul ecx
    movzx ecx, word [pos_x]
    add eax, ecx
    shl eax, 1
    
    ; Read attribute
    mov ebx, VGA_TEXT_MEM
    add ebx, eax
    movzx eax, byte [ebx + 1]
    
    pop ebx
    ret
endp

proc VGA.SetCharAt pos_x, pos_y, char, attr
    push ebx
    push ecx
    
    ; Calculate position
    movzx eax, word [pos_y]
    mov ecx, VGA_WIDTH
    mul ecx
    movzx ecx, word [pos_x]
    add eax, ecx
    shl eax, 1
    
    ; Write character and attribute
    mov ebx, VGA_TEXT_MEM
    add ebx, eax
    mov cl, [char]
    mov [ebx], cl
    mov cl, [attr]
    mov [ebx + 1], cl
    
    pop ecx
    pop ebx
    ret
endp

; UTF-8 support functions
proc UTF8.Decode utf8_ptr
    push esi
    push ecx
    push ebx
    
    mov esi, [utf8_ptr]
    xor eax, eax
    xor ebx, ebx
    
    ; Get first byte
    mov bl, [esi]
    
    ; ASCII character
    test bl, 0x80
    jz .ascii
    
    ; 2-byte UTF-8 (110xxxxx)
    test bl, 0xE0
    jz .invalid
    cmp bl, 0xC0
    jb .invalid
    cmp bl, 0xDF
    ja .check3byte
    
    mov eax, ebx
    and eax, 0x1F
    shl eax, 6
    mov bl, [esi + 1]
    and bl, 0x3F
    or eax, ebx
    jmp .done
    
.check3byte:
    ; 3-byte UTF-8 (1110xxxx)
    test bl, 0xF0
    jz .invalid
    cmp bl, 0xE0
    jb .invalid
    cmp bl, 0xEF
    ja .check4byte
    
    mov eax, ebx
    and eax, 0x0F
    shl eax, 12
    mov bl, [esi + 1]
    and bl, 0x3F
    shl ebx, 6
    or eax, ebx
    mov bl, [esi + 2]
    and bl, 0x3F
    or eax, ebx
    jmp .done
    
.check4byte:
    ; 4-byte UTF-8 (11110xxx) - not fully supported by VGA
    test bl, 0xF8
    jz .invalid
    cmp bl, 0xF0
    jb .invalid
    cmp bl, 0xF7
    ja .invalid
    
    ; For VGA, we only handle up to 0xFFFF
    ; Return replacement character for higher codepoints
    mov eax, 0xFFFD  ; Replacement character
    jmp .done
    
.ascii:
    mov eax, ebx
    jmp .done
    
.invalid:
    xor eax, eax
    
.done:
    pop ebx
    pop ecx
    pop esi
    ret
endp

proc UTF8.GetByteCount first_byte
    mov al, [first_byte]
    
    ; ASCII
    test al, 0x80
    jz .one_byte
    
    ; 2-byte UTF-8
    mov cl, al
    and cl, 0xE0
    cmp cl, 0xC0
    jne .check3byte
    mov al, 2
    ret
    
.check3byte:
    mov cl, al
    and cl, 0xF0
    cmp cl, 0xE0
    jne .check4byte
    mov al, 3
    ret
    
.check4byte:
    mov cl, al
    and cl, 0xF8
    cmp cl, 0xF0
    jne .invalid
    mov al, 4
    ret
    
.invalid:
    mov al, 1    ; Treat as ASCII
    
.one_byte:
    ret
endp

proc UTF8.ToCP437 unicode
    mov eax, [unicode]
    
    ; ASCII range (0-127) - directly usable
    cmp eax, 128
    jb .done
    
    ; Extended ASCII/Codepage 437 range (128-255)
    cmp eax, 256
    jb .check_extended
    
    ; Common Unicode symbols mapped to CP437
    cmp eax, 0x2500  ; Box drawing start
    jb .check_symbols
    cmp eax, 0x257F  ; Box drawing end
    ja .check_symbols
    
    ; Box drawing characters
    cmp eax, 0x2500  ; ─
    jne .check_2502
    mov eax, 0xC4
    jmp .done
    
.check_2502:
    cmp eax, 0x2502  ; │
    jne .check_250C
    mov eax, 0xB3
    jmp .done
    
.check_250C:
    cmp eax, 0x250C  ; ┌
    jne .check_2510
    mov eax, 0xDA
    jmp .done
    
.check_2510:
    cmp eax, 0x2510  ; ┐
    jne .check_2514
    mov eax, 0xBF
    jmp .done
    
.check_2514:
    cmp eax, 0x2514  ; └
    jne .check_2518
    mov eax, 0xC0
    jmp .done
    
.check_2518:
    cmp eax, 0x2518  ; ┘
    jne .check_251C
    mov eax, 0xD9
    jmp .done
    
.check_251C:
    cmp eax, 0x251C  ; ├
    jne .check_2524
    mov eax, 0xC3
    jmp .done
    
.check_2524:
    cmp eax, 0x2524  ; ┤
    jne .check_252C
    mov eax, 0xB4
    jmp .done
    
.check_252C:
    cmp eax, 0x252C  ; ┬
    jne .check_2534
    mov eax, 0xC2
    jmp .done
    
.check_2534:
    cmp eax, 0x2534  ; ┴
    jne .check_253C
    mov eax, 0xC1
    jmp .done
    
.check_253C:
    cmp eax, 0x253C  ; ┼
    jne .box_default
    mov eax, 0xC5
    jmp .done
    
.box_default:
    ; Default box character
    mov eax, 0xFE  ; ■
    jmp .done
    
.check_symbols:
    ; Common symbols
    cmp eax, 0x00A9  ; ©
    jne .check_00AE
    mov eax, 0xA9
    jmp .done
    
.check_00AE:
    cmp eax, 0x00AE  ; ®
    jne .check_00B0
    mov eax, 0xAE
    jmp .done
    
.check_00B0:
    cmp eax, 0x00B0  ; °
    jne .check_00B1
    mov eax, 0xF8
    jmp .done
    
.check_00B1:
    cmp eax, 0x00B1  ; ±
    jne .check_00B5
    mov eax, 0xF1
    jmp .done
    
.check_00B5:
    cmp eax, 0x00B5  ; µ
    jne .check_00F7
    mov eax, 0xE6
    jmp .done
    
.check_00F7:
    cmp eax, 0x00F7  ; ÷
    jne .check_00D7
    mov eax, 0xF6
    jmp .done
    
.check_00D7:
    cmp eax, 0x00D7  ; ×
    jne .unknown
    mov eax, 0xF7
    jmp .done
    
.check_extended:
    ; Already in CP437 range
    jmp .done
    
.unknown:
    ; Unknown character - use replacement
    mov eax, 0x3F  ; '?'
    
.done:
    ret
endp

proc VGA.PutUTF8Char utf8_ptr
    push esi
    push eax
    
    mov esi, [utf8_ptr]
    
    ; Decode UTF-8
    stdcall UTF8.Decode
    test eax, eax
    jz .invalid
    
    ; Map Unicode to CP437
    stdcall UTF8.ToCP437, eax
    
    ; Output the character
    stdcall VGA.PutChar, eax
    
.invalid:
    pop eax
    pop esi
    ret
endp

proc VGA.PutUTF8String str
    push esi
    push eax
    push ecx
    
    mov esi, [str]
    
.utf8_loop:
    mov al, [esi]
    test al, al
    jz .done
    
    ; Check if ASCII
    test al, 0x80
    jz .ascii
    
    ; UTF-8 sequence
    stdcall VGA.PutUTF8Char, esi
    
    ; Skip the UTF-8 bytes
    push eax
    mov al, [esi]
    stdcall UTF8.GetByteCount
    movzx ecx, al
    pop eax
    add esi, ecx
    jmp .utf8_loop
    
.ascii:
    stdcall VGA.PutChar, eax
    inc esi
    jmp .utf8_loop
    
.done:
    pop ecx
    pop eax
    pop esi
    ret
endp

; Utility functions
proc VGA.PrintHex value
    push eax
    push ecx
    push edx
    
    mov eax, [value]
    mov ecx, 8
    
.hex_loop:
    rol eax, 4
    mov edx, eax
    and edx, 0x0F
    cmp edx, 9
    jbe .digit
    add edx, 'A' - 10
    jmp .print
.digit:
    add edx, '0'
.print:
    push eax
    push ecx
    stdcall VGA.PutChar, edx
    pop ecx
    pop eax
    loop .hex_loop
    
    pop edx
    pop ecx
    pop eax
    ret
endp

proc VGA.PrintDec value
    push eax
    push ebx
    push ecx
    push edx
    
    mov eax, [value]
    xor ecx, ecx
    mov ebx, 10
    
.div_loop:
    xor edx, edx
    div ebx
    add dl, '0'
    push edx
    inc ecx
    test eax, eax
    jnz .div_loop
    
.print_loop:
    pop edx
    push ecx
    stdcall VGA.PutChar, edx
    pop ecx
    loop .print_loop
    
    pop edx
    pop ecx
    pop ebx
    pop eax
    ret
endp

proc VGA.DrawBox x, y, width, height, border_char, fill_char
    push esi
    push edi
    push ecx
    push ebx
    push eax
    
    ; Draw top border
    mov ax, [x]
    mov bx, [y]

    stdcall VGA.SetCursorPos, eax, ebx

    
    mov ecx, [width]
    mov al, [border_char]
.top_border:
    stdcall VGA.PutChar, eax

    loop .top_border
    
    ; Draw bottom border
    mov ax, [x]
    mov bx, [y]
    add bx, [height]
    dec bx

    stdcall VGA.SetCursorPos, eax, ebx
 
    
    mov ecx, [width]
    mov al, [border_char]
.bottom_border:
    stdcall VGA.PutChar, eax
    loop .bottom_border
    
    ; Draw side borders and fill
    mov esi, 1  ; row counter
.draw_rows:
    cmp esi, [height]
    jge .done
    
    ; Left border
    mov ax, [x]
    mov bx, [y]
    add bx, si
    stdcall VGA.SetCursorPos, eax, ebx
    
    mov al, [border_char]
    stdcall VGA.PutChar, eax
    
    ; Fill
    mov ecx, [width]
    sub ecx, 2
    jle .right_border
    mov al, [fill_char]
.fill:
    stdcall VGA.PutChar, eax
    loop .fill
    
    ; Right border
.right_border:
    mov ax, [x]
    add ax, [width]
    dec ax
    mov bx, [y]
    add bx, si

    stdcall VGA.SetCursorPos, eax, ebx

    
    mov al, [border_char]

    stdcall VGA.PutChar, eax

    
    inc esi
    jmp .draw_rows
    
.done:
    pop eax
    pop ebx
    pop ecx
    pop edi
    pop esi
    ret
endp
}

block(.initData){
    ; VGA mode 3 (80x25 text) register values
    VGA_MODE3_SEQ_REGISTERS:
        db 0x03, 0x00, 0x03, 0x00, 0x02  ; Reset, Clocking, Map Mask, Char Map, Mem Mode
    
    VGA_MODE3_CRTC_REGISTERS:
        db 0x5F, 0x4F, 0x50, 0x82, 0x55, 0x81, 0xBF, 0x1F
        db 0x00, 0x4F, 0x0D, 0x0E, 0x00, 0x00, 0x00, 0x00
        db 0x9C, 0x0E, 0x8F, 0x28, 0x1F, 0x96, 0xB9, 0xA3
        db 0xFF
    
    VGA_MODE3_GC_REGISTERS:
        db 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x0E, 0x00, 0xFF
    
    VGA_MODE3_ATTR_REGISTERS:
        db 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07
        db 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F
        db 0x41, 0x00, 0x0F, 0x00, 0x00

}

block(.data){
    ; VGA driver state
    VGA.CursorX    dw ?    ; Cursor X position (0-based)
    VGA.CursorY    dw ?    ; Cursor Y position (0-based)
    VGA.ColorFg    db ?    ; Current foreground color
    VGA.ColorBg    db ?    ; Current background color
    VGA.Blink      db ?    ; Blink state (0=off, 1=on)
    VGA.Bold       db ?    ; Bold/intensity state
    VGA.IsEnabled  db ?    ; Is VGA initialized

    ; UTF-8 decoding buffer
    UTF8.Buffer db 8 dup ?
}