struct DirEnt
       Name db 11 dup (?)
       Attr db ?
       NTRes db ?
       CrtTimeTenth ?
       CrtTime dw ?
       CrtDate
       LstAccDate dw ?
       FstClusHI dw ?
       WrtTime dw ?
       WrtDate dw ?
       FstClusLO dw ?
       FileSize dd ?
ends

struct FileHandle
       CurrCls    dd ?
       CurrPoint  dd ?
       Mode       dw ?
       Reserved   db 6 dup ?
       Dir        DirEnt
ends

struct FileOpenArea
       CountHandle db ?
       ActualClus  dd ?
       isGird      db ?
       Reserved    db 10 dub ?
ends


