format binary as 'bin'
                org 7C00h
                jmp     boot_loader
                nop
BS_OEMName      db  'HelloOS1' ;OEM name string (of OS which formatted the disk).
BPB_BytsPerSec  dw  512        ;Bytes per sector, nearly always 512 but can be 1024,2048 or 4096.
BPB_SecPerClus  db  4          ;Sectors per cluster, valid number are: 1,2,4,8,16,32,64 and 128, but a cluster size larger than 32K should not occur.
BPB_RsvdSecCnt  dw  8          ;Reserved sectors (number of sectors before the first FAT including the boot sector), usually 1.
BPB_NumFATs     db  2          ;Number of FAT's (nearly always 2).
BPB_RootEntCnt  dw  512        ;Maximum number of root directory entries.
BPB_TotSec16    dw  0          ;Total number of sectors (for small disks only, if the disk is too big this is set to 0 and offset 20h is used instead).
BPB_Media       db  $F8        ;Media descriptor byte, pretty meaningless now (see below).
BPB_FATSz16     dw  256        ;Sectors per FAT.
BPB_SecPerTrk   dw  63         ;Sectors per track.
BPB_NumHeads    dw  255        ;Total number of heads/sides.
BPB_HiddSec     dd  2048       ;Number of hidden sectors (those preceding the boot sector).
BPB_TotSec32    dd  262144     ;Total number of sectors for large disks.
;Either extended BPB (see below) or machine code.
BS_DrvNum       db  $80        ;Physical drive number (BIOS system ie 80h is first HDD, 00h is first FDD)
BS_Reserved1    db  0          ;Current head (not used for this but WinNT stores two flags here).
BS_BootSig      db  $29        ;Signature (must be 28h or 29h to be recognised by NT).
BS_VolID        dd  $0A171AE7  ;The serial number, the serial number is stored in reverse order and is the hex representation of the bytes stored here
BS_VolLab       db  'NO NAME    ';Volume label.
BS_FilSysType   db  'FAT16   '   ;File system ID. "FAT12", "FAT16" or "FAT  ".

boot_loader:
root_dir_ofs:
        xor ax, ax
        mov ss, ax
        mov sp, 0x7C00
data_claster_ofs:
        mov ds, ax
        mov es, ax

        mov     [BS_DrvNum], dl

data_kernel_size:
        cmp     [BPB_TotSec16], ax
sector:
        je      @F
heads:
        mov     cx, [BPB_TotSec16]
culinder:
        mov     word[BPB_TotSec32], cx
@@:

        mov     al, [BPB_NumFATs]
        mul     [BPB_FATSz16]
        add     ax, [BPB_RsvdSecCnt]
        adc     dx, 0
        add     ax, word [BPB_HiddSec]
        adc     dx, word [BPB_HiddSec + 2]

        mov     word[root_dir_ofs], ax
        mov     word[root_dir_ofs + 2], dx
        mov     word[data_claster_ofs], ax
        mov     word[data_claster_ofs + 2], dx



        ; Calculate the data cluster offset
        ; offset = root_dir_offset + root_dir_size
        ; root_dir_size = (RootDirEntries * 32) / BytesPerSector
        ;       NOTE, to avoid overflow, do this instead
        ; root_dir_size = RootDirEntries / (BytesPerSector / 0x20)
        mov     ax, [BPB_BytsPerSec]
        mov     cx, 32
        div     cx
        xchg    ax, cx
        mov     ax, [BPB_RootEntCnt]
        div     cx
        add     word[data_claster_ofs], ax
        adc     word[data_claster_ofs + 2], 0

        xor     cx, cx
.load_size:
        xor     dx, dx
        mov     ax, [BPB_RsvdSecCnt]
        add     ax, cx
        adc     dx, 0
        add     ax, word[BPB_HiddSec]
        adc     dx, word[BPB_HiddSec+2]
        mov     bx, cx
        imul    bx, [BPB_BytsPerSec]
        add     bx,  $8000

        call    lba_to_chs

        mov     al, 1
        call    read_sectors
        jc      ErrorDisk
        inc     cx
        cmp     cx, [BPB_FATSz16]
        jae     .load_size

        mov     ax, word[root_dir_ofs]
        mov     dx, word[root_dir_ofs + 2]
        mov     bx, $500
        mov     cx, [BPB_RootEntCnt]
        shr     cx, 4
@@:
        pusha
        call    lba_to_chs
        mov     al, 1
        call    read_sectors
        popa
        jc      ErrorDisk
        add     bx, $100
        add     ax, 1
        adc     dx, 0
        loop    @B
        mov     ax, [BPB_RootEntCnt]
        mov     bx, $500
@@:
        mov     di, bx
        mov     cx, 11
        mov     si, kern_filename
        repe    cmpsb
        je      Continue
        add     bx, 32
        dec     ax
        jnz     @B

Error:
        mov     si, kern_filename
DiskErr:
        call    print_string
        xor     ax, ax
        int     16h
        int     19h
ErrorDisk:
        mov     si, errorsrt2
        jmp     DiskErr
ErrorLBA:
        mov     si, errorsrt3
        jmp     DiskErr


Continue:

        mov     ax, [bx + 28]
        mov     [data_kernel_size], ax
        mov     ax, [bx + 30]
        mov     [data_kernel_size + 2], ax
        mov     ax, [bx + 26]
        mov     di, $700
.LoadLoop:
        mov     bx, $8000
        add     bx, ax
        add     bx, ax
        push    bx
        mov     cl, [BPB_SecPerClus]
        xor     ch, ch

        dec     ax
        dec     ax
        mul     cx
        add     ax, word[data_claster_ofs]
        adc     dx, word[data_claster_ofs + 2]
        ;читаем
        movzx   cx, [BPB_SecPerClus]
.ReadLoop:
        push    ax
        push    dx
        push    cx
        call    lba_to_chs

        mov     bx, di
        mov     al, 1

        call    read_sectors
        jc      ErrorDisk
        add     di, [BPB_BytsPerSec]
        pop     cx
        pop     dx
        pop     ax
        add     ax, 1
        adc     dx, 0
        loop    .ReadLoop
        pop     bx

        mov     ax, [bx]
        cmp     ax, $FFF8
        jb      .LoadLoop
        mov     ax, [data_kernel_size]
        mov     dx, [data_kernel_size + 2]
        jmp      $0070:$0000

lba_to_chs:
        cmp     dx, [BPB_SecPerTrk]
        jae     bedNum
        div     [BPB_SecPerTrk]
        inc     dl
        mov     [sector], dl
        xor     dx, dx
        div     [BPB_NumHeads]
        mov     [heads], dl
        mov     [culinder], ax
return:
        ret
bedNum:
        jmp     ErrorLBA
        ret

read_sectors:
        mov     si, 6
.ReadTry:
        dec     si
        jz      .Exit
        mov     ah, $02
        mov     dx, [culinder]
        mov     cl, 6
        shl     dh, cl
        or      dh, [sector]
        mov     cx, dx
        xchg    cl, ch
        mov     dl, [BS_DrvNum]
        mov     dh, [heads]
        int     13h
        jc      .ReadTry
.Exit:
        ret

print_string:

.repeat:
        lodsb
        test    al, al
        jz      return
        mov     ah, $0E
        mov     bx, $0007
        int     10h
        jmp     .repeat

DataStr:
kern_filename       db 'KERNEL  SYS'
errorsrt            db ' NotFound',0
errorsrt3           db 'LBAtoCHS '
errorsrt2           db 'Disk Error',0


times 510-($-$$) db 0
dw 0xAA55