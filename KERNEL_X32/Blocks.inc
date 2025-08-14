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