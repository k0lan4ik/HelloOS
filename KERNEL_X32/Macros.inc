macro block Params&
{
    local params, parsed, cb, header    
    define params Params
    define parsed 0
    match =0 (cbt) { rest , parsed params
    \{  
        restore parsed  
        Blocks@ \# cbt equ cb
        define header macro cb { rest
    \}
    match =0 (cbt) { , parsed params
    \{
        restore parsed
        Blocks@ \# cbt equ cb
        define header macro cb {
    \}
    match =0 (cbt) , parsed params
    \{
        restore parsed
        Blocks@ \# cbt equ cb
        define header macro cb
    \}
    match =0 , parsed \{ block Params \}

    match h , header \{ h \}
}

macro putBlocks BlockType*
{
    irpv i , Blocks@ # BlockType \{ i \}
}

; короче по памяти чем просто mov но хуже по производительности
; Можно юзать в местах, где не важна производительность для экономии        
macro movi dst, imm
{
if imm >= -0x80 & imm <= 0x7F
        push    imm
        pop     dst
else
        mov     dst, imm
end if
}

macro displayPnt
{
    a = 8
    c = $
    repeat a
    display (c mod 16) + '0'
    c = c / 16
    end repeat
    display 13, 10
}

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