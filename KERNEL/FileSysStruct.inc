 include 'win32a.inc'
struct DirEnt
       Name db 11 dup (?)
       Attr db ?
       NTRes db ?
       CrtTimeTenth dw ?
       CrtTime dw ?
       CrtDate dw ?
       LstAccDate dw ?
       FstClusHI dw ?
       WrtTime dw ?
       WrtDate dw ?
       FstClusLO dw ?
       FileSize dd ?
ends
struct FileHandle
       CurrCls    dd 0
       CurrPoint  dd 0
       Mode       dw 0
       Reserved   db 6 dup ?
       Dir        DirEnt
ends

struct FileArea
       CountHandle db CountFile
       ActualClus  dd 0
       isGird      db 0
       ClusSec     dw 0
       Reserved    db 8 dup ?
ends


