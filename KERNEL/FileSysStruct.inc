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
       CurrCls dd ?
       CurrPoint  dd ?
       Mode       dw ?
ends