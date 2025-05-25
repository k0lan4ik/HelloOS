 include 'win32a.inc'
struct DIRENT
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
       CurrCls dd ?
       CurrPoint  dd ?
       Mode       dw ?
ends