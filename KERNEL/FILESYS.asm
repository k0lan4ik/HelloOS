label   BS_jmpBoot      word at $0000
label   BS_OEMName      byte at $0003
label   BPB_BytsPerSec  word at $000B
label   BPB_SecPerClus  byte at $000D
label   BPB_RsvdSecCnt  word at $000E
label   BPB_NumFATs     byte at $0010
label   BPB_RootEntCnt  word at $0011
label   BPB_TotSec16    word at $0013
label   BPB_Media       byte at $0015
label   BPB_FATSz16     word at $0016
label   BPB_SecPerTrk   word at $0018
label   BPB_NumHeads    word at $001A
label   BPB_HiddSec     dword at $001C
label   BPB_TotSec32    dword at $0020

label   BS_DrvNum       byte at $0024
label   BS_Reserved1    byte at $0025
label   BS_BootSig      byte at $0026
label   BS_VolID        dword at $0027
label   BS_VolLab       byte at $002B
label   BS_FilSysType   byte at $0036



;Parameters
;       dx:ax - LBA of start load
;       cx - count of segment
;       es:bx - start load in memory
;Return in cf is error
proc FileSys.Load uses ds si
locals
     culinder dw ?
     sector db ?
     heads db ?
endl
     mov     ds, [cs:FileSys.TableSeg]
.ReadAll:
     push    cx
     push    dx
     push    ax

     cmp     dx, [ds:BPB_SecPerTrk]
     jae     .ErrExit
     div     [ds:BPB_SecPerTrk]
     inc     dl
     mov     [sector], dl
     xor     dx, dx
     div     [ds:BPB_NumHeads]
     mov     [heads], dl
     mov     [culinder], ax

     mov     si, 6

.ReadTry:

     dec     si
     jz      .ErrExit

     mov     ah, $02
     mov     al, 1
     mov     dx, [culinder]
     mov     cl, 6
     shl     dh, cl
     or      dh, [sector]
     mov     cx, dx
     xchg    cl, ch
     mov     dl, [ds:BS_DrvNum]
     mov     dh, [heads]

     int     13h
     jnc     .Step
     mov     ah, 00h
     int     13h
     jmp     .ReadTry
.Step:
     add     bx, 512
     jnc     @F
     mov     ax, es
     add     ax, $0100
     mov     es, ax
@@:
     pop     ax
     pop     dx
     add     ax, 1
     adc     dx, 0
     pop     cx
     loop    .ReadAll

     clc
     jmp     .Exit
.ErrExit:
     stc
.Exit:
     ret
endp

;Parameters
;Return in cf is error
proc FileSys.Initialize uses es ds di si bx
     mov     bx, cs
     mov     ax, 512
     stdcall Memory.AllocFirst
     jc      .EndProc

     mov     [cs:FileSys.TableSeg], ax
     mov     es, ax
     xor     di, di
     mov     ds, di
     mov     si, $7C00
     mov     cx, 512 / 2
     cld
     rep     movsw

     mov     ds, [cs:FileSys.TableSeg]
     mov     bx, cs
     xor     dx, dx
     mov     ax, [ds:BPB_FATSz16]
     shld    dx, ax, 9
     shl     ax, 9

     stdcall Memory.AllocFAT
     jc      .EndProc

     mov     [cs:FileSys.FATSeg], ax
     mov     es, ax
     mov     ax, word[ds:BPB_HiddSec]
     mov     dx, word[ds:BPB_HiddSec + 2]
     add     ax, [ds:BPB_RsvdSecCnt]
     adc     dx, 0
     mov     cx, [ds:BPB_FATSz16]
     xor     bx, bx

     stdcall FileSys.Load
     jc      .EndProc

     mov     bx, cs
     mov     ax, [ds:BPB_RootEntCnt]
     shl     ax, 5

     stdcall Memory.AllocFirst
     jc      .EndProc

     mov     [cs:FileSys.DirSeg], ax

     xor     ah, ah
     mov     al, [ds:BPB_NumFATs]
     mul     [ds:BPB_FATSz16]
     add     ax, [ds:BPB_RsvdSecCnt]
     adc     dx, 0
     add     ax, word [ds:BPB_HiddSec]
     adc     dx, word [ds:BPB_HiddSec + 2]
     mov     word[cs:FileSys.PathStack], ax
     mov     word[cs:FileSys.PathStack + 2], dx
     push    ax
     push    dx
     mov     word[cs:FileSys.DataClasterOfs], ax
     mov     word[cs:FileSys.DataClasterOfs + 2], dx

     mov     ax, [ds:BPB_BytsPerSec]
     mov     cx, 32
     div     cx
     xchg    ax, cx
     mov     ax, [ds:BPB_RootEntCnt]
     div     cx
     add     word[cs:FileSys.DataClasterOfs], ax
     adc     word[cs:FileSys.DataClasterOfs + 2], 0

     mov     cx, [ds:BPB_RootEntCnt]
     shr     cx, 4
     mov     es, [cs:FileSys.DirSeg]

     xor     bx, bx
     pop     dx
     pop     ax

     stdcall FileSys.Load
     jc      .EndProc

     add     [cs:FileSys.DirPointer], 2
     clc
.EndProc:
     ret
endp

;Parameters
;       ds:si - name file on format 8:3
;Return in dx:ax size, bx first cluster
proc FileSys.SearchFile uses es di
     push    [cs:FileSys.TableSeg]
     pop     es
     mov     cx, [es:BPB_RootEntCnt]

     mov     es, [cs:FileSys.DirSeg]
     xor     di, di
     .Loop:
     push    si
     push    di
     push    cx
     mov     cx, 11
     cld
     repe    cmpsb
     je      .Found
     pop     cx
     pop     di
     pop     si
     add di, 32
     loop   .Loop
     stc
     jmp    .EndProc
.Found:
     pop     cx di si
     mov     ax, [es:di + 28]
     mov     dx, [es:di + 30]
     mov     bx, [es:di + 26]
     clc
.EndProc:
     ret
endp

;Parameters
;       ax - first cluster
;       bx - segment for loadin
;Return cf is error
proc FileSys.LoadClusFile uses es ds si di
     locals
       oldEnd dw ?
     endl

     mov     es, [cs:FileSys.FATSeg]
     mov     ds, [cs:FileSys.TableSeg]

     movzx   si, [ds:BPB_SecPerClus]
     imul    si, [ds:BPB_BytsPerSec]
     shr     si, 4

     mov     [oldEnd], ax
.LoadLoop:

     mov     di, ax
     shl     di, 1
     push    di
     movzx   cx, [ds:BPB_SecPerClus]

     sub     ax, 2
     mul     cx
     add     ax, word [cs:FileSys.DataClasterOfs]
     adc     dx, word [cs:FileSys.DataClasterOfs + 2]

     movzx   cx, [ds:BPB_SecPerClus]
     push    bx es
     mov     es, bx
     xor     bx, bx
     stdcall FileSys.Load
     pop     es bx di
     jc      .EndProc
     add     bx, si

     mov   ax,[es:di]
     cmp   [oldEnd], $fff8
     mov   [oldEnd], ax
     jb    .LoadLoop

.EndProc:
     ret
endp

;Parameters
;       ds:si - name file on format 8:3
;       bx - loader prog segment (parent handle)
;|Find and load file in curr dir|
;Return in cf is error (is not found ah = $04), di - handle of file (segment of load file)
proc FileSys.LoadFile uses ds si es bx
locals
  Res dw ?
endl
     mov     di, bx
     stdcall FileSys.SearchFile
     jnc     .Found
     mov     ah, $04
     jmp     .EndProc

.Found:
     push    dx ax
     mov     es, [cs:FileSys.TableSeg]
     movzx   cx, [es:BPB_SecPerClus]
     imul    cx, [es:BPB_BytsPerSec]
     add     ax, cx
     adc     dx, 0
     div     cx
     mul     cx
     xchg    di, bx
     stdcall Memory.AllocFAT
     jnc     @F
     mov     ah, $03
     jmp     .EndProc
@@:
     mov     [Res], ax
     mov     bx, di
     xchg    ax, bx

     stdcall FileSys.LoadClusFile
     jnc     @F
     mov     ax, [Res]
     stdcall Memory.Free
     mov     ah, $05
     stc
     jmp     .EndProc
@@:
     mov     di, [Res]
     pop     ax dx

     clc
.EndProc:
     ret
endp

;Parameters
;       es:di - pointer for name file on format 8:3
;       ax - num of dir rec
;Return dx:ax - size of file, cl - type (10 is directory, F0 if skiped)
proc FileSys.ScanDir uses ds si
     mov     ds, [FileSys.DirSeg]
     shl     ax, 5
     mov     si, ax
     mov     ch, byte[si]
     cmp     ch, $E5
     je     .Skip
     cmp     ch, $00
     je     .Skip

     mov     cl, [si + 11]
     cmp     cl, $02
     je      .Skip
     cmp     cl, $08
     je      .Skip
     cmp     cl, $0F
     je      .Skip

     mov     cx, 11
     cld
     rep     movsb

     mov     cl, [si]
     cmp     cl, $10
     je      @F
     xor     cl, cl
@@:
     xor     ch, ch
     mov     ax, [si + 17]
     mov     dx, [si + 19]
     jmp     .EndProc
.Skip:
     mov     cl, $F0
.EndProc:
     ret
endp




proc FileSys.ListDir uses es di ds si cx ax dx
    mov     cx, [cs:FileSys.TableSeg]
    mov     ds, cx
    mov     cx, [ds:BPB_RootEntCnt]

    mov     ax, 1
    mov     di, .filelist_buffer

.Loop:
    push    ax cx di

    push    cs
    pop     es
    stdcall FileSys.ScanDir

    cmp     cl, $F0
    je      .SkipEntry

    push    cx ax
    mov     cx, 7
    mov     al, ' '
    rep stosb
    dec     di
    pop     ax cx
    cmp     cl, $10
    jne     @F
    mov     cx, [.fileDir.Len]
    sub     di, cx
    inc     di
    mov     si, .fileDir
    rep  movsb
    jmp     .Write
@@:
    test    dx, dx
    jnz     .Write
    mov     cx, 10
.AddNum:
    xor     dx, dx
    div     cx
    add     dl, '0'
    mov     [di], dl
    dec     di
    test    ax, ax
    jnz     .AddNum


.Write:

    push    cs
    pop     ds
    mov     dx, .filelist_buffer
    mov     ah, 0x09
    int     21h

.SkipEntry:
    pop     di cx ax

    inc     ax
    loop    .Loop

    ret
.fileDir db '[DIR]'
.fileDir.Len dw $-.fileDir
.filelist_buffer   db 20 dup 0, 13,10, '$'
endp

proc FileSys.Create
     mov     ax, 1
     mov     cx, [cs:FileSys.TableSeg]
     mov     ds, cx
     mov     cx, [ds:BPB_RootEntCnt]

     mov     di, [cs:FileSys.ListDir.filelist_buffer]

.FingFreeDir:
     push    ax cx di
     push    cs
     pop     es
     stdcall FileSys.ScanDir
     cmp     cx, $00F0
     je      .Found
     cmp     cx, $E5F0
     je      .Found
     jmp     .SkipEntry
.Found:

.SkipEntry:
     pop     di cx ax
     inc     ax
     loop    .FingFreeDir
     ret
endp

FileSys.TableSeg       dw 0
FileSys.FATSeg         dw 0
FileSys.DirSeg         dw 0
FileSys.PathStack      dd 32 dup 0
FileSys.DirPointer     dw 0
FileSys.DataClasterOfs dd 0