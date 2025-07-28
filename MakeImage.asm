format binary

BYTES_PER_SECTOR      equ 512
SECTORS_PER_CLUSTER   equ 1
RESERVED_SECTORS      equ 1
NUMBER_OF_FATS        equ 2
ROOT_DIR_ENTRIES      equ 512
TOTAL_SECTORS         equ 2880
MEDIA_DESCRIPTOR      equ 0F0h
SECTORS_PER_FAT       equ 8
SECTORS_PER_TRACK     equ 18
NUMBER_OF_HEADS       equ 2

BYTES_PER_CLUSTER     equ BYTES_PER_SECTOR * SECTORS_PER_CLUSTER

FAT1_OFS      equ RESERVED_SECTORS * BYTES_PER_SECTOR
ROOT_DIR_OFS  equ FAT1_OFS + (SECTORS_PER_FAT * BYTES_PER_SECTOR) * NUMBER_OF_FATS
DATA_AREA_OFS equ ROOT_DIR_OFS + ROOT_DIR_ENTRIES * 32



file_count = 0
current_cluster = 2 

macro add_file [dos_name, real_path]
{
    file_count = file_count + 1

   
    virtual at 0
        file real_path
        size_of_file_#file_count = $
    end virtual

    clusters_for_file = (size_of_file_#file_count + BYTES_PER_CLUSTER - 1) / BYTES_PER_CLUSTER
    if clusters_for_file = 0
       clusters_for_file = 1
    end if

    start_cluster_of_file_#file_count = current_cluster
    clusters_of_file_#file_count = clusters_for_file
    size_of_file_#file_count = size_of_file_#file_count
    path_of_file_#file_count equ real_path
    dos_name_#file_count equ dos_name



    current_cluster = current_cluster + clusters_for_file
}

macro build_fat
{
  repeat NUMBER_OF_FATS
    cur_fat_pos = $
    dw MEDIA_DESCRIPTOR + 0xFF00, 0xFFFF
    i = 1
    repeat file_count

      if clusters_of_file_#file_count > 1
        c = start_cluster_of_file_#file_count + 1 
        repeat clusters_of_file_#file_count - 1
          dw c
          c = c + 1
        end repeat
      end if
      dw 0xFFFF 
      i = i + 1
    end repeat
  times (cur_fat_pos + (SECTORS_PER_FAT * BYTES_PER_SECTOR) - $) db 0
  end repeat
}

macro build_root_dir
{
  org ROOT_DIR_OFS
  i = 1
  repeat file_count
    db dos_name_#file_count
    db 0h
    db 0
    db 8
    dw 0C98h,5AE8h,0,0,0C98h,5AE8h
    dw start_cluster_of_file_#file_count
    dd size_of_file_#file_count

    i = i + 1
  end repeat
}

macro build_data_area
{
  i = 1
  repeat file_count

    pos_file = $
    file path_of_file_#file_count
    times pos_file + size_of_file_#file_count - $ db 0

  end repeat
}

add_file 'KERNEL  SYS', 'KERNEL_X32/KERNEL.sys'

org 7C00h
BootLoader:
jmp short start
nop

OEM_ID                db 'Hello OS'
BytesPerSector        dw BYTES_PER_SECTOR
SectorsPerCluster     db SECTORS_PER_CLUSTER
ReservedSectors       dw RESERVED_SECTORS
NumberOfFATs          db NUMBER_OF_FATS
RootDirEntries        dw ROOT_DIR_ENTRIES
TotalSectors          dw TOTAL_SECTORS
MediaDescriptor       db MEDIA_DESCRIPTOR
SectorsPerFAT         dw SECTORS_PER_FAT
SectorsPerTrack       dw SECTORS_PER_TRACK
NumberOfHeads         dw NUMBER_OF_HEADS
HiddenSectors         dd 0
TotalSectorsBig       dd 0

DriveNumber           db 80h
Reserved              db 0
BootSignature         db 29h
VolumeID              dd 12345678h
VolumeLabel           db 'NO NAME    '
FileSystemType        db 'FAT16   '

start:

root_dir_ofs:
        xor ax, ax
        mov ss, ax
        mov sp, 0x7C00
data_claster_ofs:
        mov ds, ax
        mov es, ax

        mov     [DriveNumber], dl

data_kernel_size:
        cmp     [BytesPerSector], ax
sector:
        je      @F
heads:
        mov     cx, [BytesPerSector]
culinder:
        mov     word[TotalSectorsBig], cx
@@:

        mov     al, [NumberOfFATs]
        mul     [SectorsPerFAT]
        add     ax, [ReservedSectors]
        adc     dx, 0
        add     ax, word [HiddenSectors]
        adc     dx, word [HiddenSectors + 2]

        mov     word[root_dir_ofs], ax
        mov     word[root_dir_ofs + 2], dx
        mov     word[data_claster_ofs], ax
        mov     word[data_claster_ofs + 2], dx



        ; Calculate the data cluster offset
        ; offset = root_dir_offset + root_dir_size
        ; root_dir_size = (RootDirEntries * 32) / BytesPerSector
        ;       NOTE, to avoid overflow, do this instead
        ; root_dir_size = RootDirEntries / (BytesPerSector / 0x20)
        mov     ax, [BytesPerSector]
        mov     cx, 32
        div     cx
        xchg    ax, cx
        mov     ax, [RootDirEntries]
        div     cx
        add     word[data_claster_ofs], ax
        adc     word[data_claster_ofs + 2], 0

        xor     cx, cx
.load_size:
        xor     dx, dx
        mov     ax, [ReservedSectors]
        add     ax, cx
        adc     dx, 0
        add     ax, word[HiddenSectors]
        adc     dx, word[HiddenSectors+2]
        mov     bx, cx
        imul    bx, [BytesPerSector]
        add     bx,  $8000

        call    lba_to_chs

        mov     al, 1
        call    read_sectors
        jc      ErrorDisk
        inc     cx
        cmp     cx, [SectorsPerFAT]
        jae     .load_size

        mov     ax, word[root_dir_ofs]
        mov     dx, word[root_dir_ofs + 2]
        mov     bx, $500
        mov     cx, [RootDirEntries]
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
        mov     ax, [RootDirEntries]
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
        mov     di, $4000 ; ??? ????????
.LoadLoop:
        mov     bx, $8000
        add     bx, ax
        add     bx, ax
        push    bx
        mov     cl, [SectorsPerCluster]
        xor     ch, ch

        dec     ax
        dec     ax
        mul     cx
        add     ax, word[data_claster_ofs]
        adc     dx, word[data_claster_ofs + 2]
        movzx   cx, [SectorsPerCluster]
.ReadLoop:
        push    ax
        push    dx
        push    cx
        call    lba_to_chs

        mov     bx, di
        mov     al, 1

        call    read_sectors
        jc      ErrorDisk
        add     di, [BytesPerSector]
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
        jmp      $0400:$0000 ; ? ??? ????????

lba_to_chs:
        cmp     dx, [SectorsPerTrack]
        jae     bedNum
        div     [SectorsPerTrack]
        inc     dl
        mov     [sector], dl
        xor     dx, dx
        div     [NumberOfHeads]
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
        mov     dl, [DriveNumber]
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

    times 510-($-BootLoader) db 0
    dw 0xAA55
org 200h

times FAT1_OFS - $ db 0
build_fat

times ROOT_DIR_OFS - $ db 0  
build_root_dir

times DATA_AREA_OFS - $ db 0 
build_data_area

times TOTAL_SECTORS * BYTES_PER_SECTOR - $  db 0