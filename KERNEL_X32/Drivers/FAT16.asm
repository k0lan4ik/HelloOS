block(.consts){
    ; FAT16 Constants
    FAT16_SIGNATURE      = 0x29          ; Extended boot signature
    FAT16_EOC           = 0xFFF8        ; End of cluster chain marker
    FAT16_BAD_CLUSTER   = 0xFFF7        ; Bad cluster marker
    FAT16_FREE_CLUSTER  = 0x0000        ; Free cluster marker
    FAT16_RESERVED_CLUSTER = 0x0001     ; Reserved cluster
    
    ; Boot Sector Offsets
    BS_JMPBOOT          = 0x00          ; 3 bytes
    BS_OEMNAME          = 0x03          ; 8 bytes
    BPB_BYTESPERSECT    = 0x0B          ; 2 bytes
    BPB_SECPERCLUS      = 0x0D          ; 1 byte
    BPB_RSVDSECCNT      = 0x0E          ; 2 bytes
    BPB_NUMFATS         = 0x10          ; 1 byte
    BPB_ROOTENTCNT      = 0x11          ; 2 bytes
    BPB_TOTSEC16        = 0x13          ; 2 bytes
    BPB_MEDIA           = 0x15          ; 1 byte
    BPB_FATSZ16         = 0x16          ; 2 bytes
    BPB_SECPERTRK       = 0x18          ; 2 bytes
    BPB_NUMHEADS        = 0x1A          ; 2 bytes
    BPB_HIDDSEC         = 0x1C          ; 4 bytes
    BPB_TOTSEC32        = 0x20          ; 4 bytes
    BS_DRVNUM           = 0x24          ; 1 byte
    BS_RESERVED1        = 0x25          ; 1 byte
    BS_BOOTSIG          = 0x26          ; 1 byte
    BS_VOLID            = 0x27          ; 4 bytes
    BS_VOLLAB           = 0x2B          ; 11 bytes
    BS_FILSYSTYPE       = 0x36          ; 8 bytes
    
    ; Directory Entry Offsets
    DIR_NAME            = 0x00          ; 11 bytes
    DIR_ATTR            = 0x0B          ; 1 byte
    DIR_NT_RES          = 0x0C          ; 1 byte
    DIR_CRT_TIME_TENTH  = 0x0D          ; 1 byte
    DIR_CRT_TIME        = 0x0E          ; 2 bytes
    DIR_CRT_DATE        = 0x10          ; 2 bytes
    DIR_LST_ACC_DATE    = 0x12          ; 2 bytes
    DIR_FST_CLUS_HI     = 0x14          ; 2 bytes
    DIR_WRT_TIME        = 0x16          ; 2 bytes
    DIR_WRT_DATE        = 0x18          ; 2 bytes
    DIR_FST_CLUS_LO     = 0x1A          ; 2 bytes
    DIR_FILE_SIZE       = 0x1C          ; 4 bytes
    
    ; Directory Attributes
    ATTR_READ_ONLY      = 0x01
    ATTR_HIDDEN         = 0x02
    ATTR_SYSTEM         = 0x04
    ATTR_VOLUME_ID      = 0x08
    ATTR_DIRECTORY      = 0x10
    ATTR_ARCHIVE        = 0x20
    ATTR_LONG_NAME      = 0x0F
    
    ; File Access Modes
    FA_READ             = 0x01
    FA_WRITE            = 0x02
    FA_OPEN_EXISTING    = 0x00
    FA_CREATE_NEW       = 0x04
    FA_CREATE_ALWAYS    = 0x08
    FA_OPEN_ALWAYS      = 0x10
    
    ; Error Codes
    FR_OK               = 0      ; Success
    FR_DISK_ERR         = 1      ; Disk error
    FR_NOT_READY        = 2      ; Drive not ready
    FR_NO_FILE          = 3      ; File not found
    FR_NO_PATH          = 4      ; Path not found
    FR_INVALID_NAME     = 5      ; Invalid filename
    FR_DENIED           = 6      ; Access denied
    FR_EXIST            = 7      ; File already exists
    FR_INVALID_OBJECT   = 8      ; Invalid file object
    FR_WRITE_PROTECTED  = 9      ; Write protected
    FR_INVALID_DRIVE    = 10     ; Invalid drive number
    FR_NOT_ENABLED      = 11     ; Filesystem not mounted
    FR_NO_FILESYSTEM    = 12     ; No valid FAT volume
    FR_MKFS_ABORTED     = 13     ; mkfs aborted
    FR_TIMEOUT          = 14     ; Timeout
    FR_LOCKED           = 15     ; File locked
    FR_NOT_ENOUGH_CORE  = 16     ; Not enough memory
    FR_TOO_MANY_OPEN_FILES = 17  ; Too many open files
    FR_INVALID_PARAMETER = 18    ; Invalid parameter
    
    ; Buffer sizes
    FAT_CACHE_SIZE      = 512    ; FAT cache buffer size
    DIR_CACHE_SIZE      = 512    ; Directory cache buffer size
    MAX_PATH            = 260    ; Maximum path length
    MAX_FILES           = 8      ; Maximum open files
    MAX_DIRS            = 4      ; Maximum open directories
    
    ; Special characters
    DIR_SEP             = '/'    ; Directory separator
    DOT                 = '.'    ; Current directory
    DOTDOT              = ".."   ; Parent directory
}

block(.structs){
; FAT16 Filesystem Structure
virtual at 0
    FAT16.VolumeID         dd ?      ; Volume identifier
    FAT16.BytesPerSector   dw ?      ; Bytes per sector
    FAT16.SectorsPerCluster db ?     ; Sectors per cluster
    FAT16.ReservedSectors  dw ?      ; Reserved sectors count
    FAT16.NumberOfFATs     db ?      ; Number of FATs
    FAT16.RootEntries      dw ?      ; Root directory entries
    FAT16.TotalSectors     dd ?      ; Total sectors
    FAT16.SectorsPerFAT    dw ?      ; Sectors per FAT
    FAT16.SectorsPerTrack  dw ?      ; Sectors per track
    FAT16.NumberOfHeads    dw ?      ; Number of heads
    FAT16.HiddenSectors    dd ?      ; Hidden sectors
    FAT16.FATStart         dd ?      ; FAT start sector (LBA)
    FAT16.RootStart        dd ?      ; Root directory start sector (LBA)
    FAT16.DataStart        dd ?      ; Data area start sector (LBA)
    FAT16.TotalClusters    dd ?      ; Total clusters
    FAT16.FATType          db ?      ; FAT type (0=FAT12, 1=FAT16)
    FAT16.Drive            db ?      ; Physical drive number
    FAT16.Mounted          db ?      ; Mount flag
    FAT16.WriteProtected   db ?      ; Write protected flag
    FAT16.FSInfoSector     dw ?      ; FSInfo sector
    FAT16.FreeClusters     dd ?      ; Free clusters count
    FAT16.LastAllocCluster dd ?      ; Last allocated cluster
    FAT16.CacheDirty       db ?      ; Cache dirty flag
    FAT16.CacheSector      dd ?      ; Cached sector number
    FAT16.CacheBuffer      dd ?      ; Pointer to cache buffer
    FAT16.FATCacheDirty    db ?      ; FAT cache dirty flag
    FAT16.FATCacheSector   dd ?      ; Cached FAT sector
    FAT16.FATCacheBuffer   dd ?      ; Pointer to FAT cache buffer
    FAT16Size:
end virtual

; File Object Structure
virtual at 0
    File.ObjSize           dd ?      ; Structure size
    File.FS                dd ?      ; Pointer to filesystem
    File.Cluster           dw ?      ; Current cluster
    File.Sector            dd ?      ; Current sector
    File.Pointer           dd ?      ; Read/write pointer
    File.Size              dd ?      ; File size
    File.DirSector         dd ?      ; Directory sector
    File.DirIndex          dw ?      ; Directory index
    File.Flags             db ?      ; File flags
    File.Mode              db ?      ; Access mode
    File.Pad               db 2 dup ?; Padding
    FileSize:
end virtual

; Directory Object Structure
virtual at 0
    Dir.ObjSize            dd ?      ; Structure size
    Dir.FS                 dd ?      ; Pointer to filesystem
    Dir.Cluster            dw ?      ; Current cluster
    Dir.Sector             dd ?      ; Current sector
    Dir.Index              dw ?      ; Current index
    Dir.Sect               dd ?      ; Current sector in buffer
    Dir.Dir                dd ?      ; Pointer to directory entry
    DirSize:
end virtual

; File Information Structure
virtual at 0
    FileInfo.FileSize      dd ?      ; File size
    FileInfo.CreateDate    dw ?      ; Creation date
    FileInfo.CreateTime    dw ?      ; Creation time
    FileInfo.AccessDate    dw ?      ; Last access date
    FileInfo.WriteDate     dw ?      ; Last write date
    FileInfo.WriteTime     dw ?      ; Last write time
    FileInfo.Attribute     db ?      ; File attribute
    FileInfo.AltName       db 13 dup ? ; Alternate filename
    FileInfo.Name          db 256 dup ? ; Full filename
    FileInfoSize:
end virtual

; Find File Structure
virtual at 0
    FindFile.Pattern       db MAX_PATH dup ? ; Search pattern
    FindFile.Dir           dd ?      ; Directory object
    FindFile.Cluster       dw ?      ; Current cluster
    FindFile.Sector        dd ?      ; Current sector
    FindFile.Index         dw ?      ; Current index
    FindFileSize:
end virtual
}

block(.text){
proc FAT16.Init
    stdcall Mutex.Start, FAT16.GlobalMutex
    stdcall Mutex.Start, FAT16.FileMutex
    stdcall Mutex.Start, FAT16.DirMutex
    
    mov     edi, FAT16.OpenFiles
    mov     ecx, MAX_FILES * FileSize / 4
    xor     eax, eax
    rep stosd
    
    mov     edi, FAT16.OpenDirs
    mov     ecx, MAX_DIRS * DirSize / 4
    rep stosd
    
    mov     edi, FAT16.FS
    mov     ecx, FAT16Size / 4
    xor     eax, eax
    rep stosd
    
    mov     [FAT16.OpenFileCount], 0
    mov     [FAT16.OpenDirCount], 0
    
    stdcall KernelMemManager.Malloc, FAT_CACHE_SIZE
    mov     dword[FAT16.FS + FAT16.FATCacheBuffer], eax
    
    stdcall KernelMemManager.Malloc, FAT_CACHE_SIZE
    mov     dword[FAT16.FS + FAT16.CacheBuffer], eax
    
    stdcall VGA.PutString, Str.FAT16Init
    
    ret
endp


proc FAT16.Mount uses ebx esi edi, drive: BYTE

    locals
        bootSector dd ?
    endl
    
    movzx   eax, [drive]
    cmp     al, 2
    jb      .DriveOK
    mov     eax, FR_INVALID_DRIVE
    jmp     .EndProc
    
.DriveOK:
    stdcall KernelMemManager.Malloc, 512
    mov     [bootSector], eax
    test    eax, eax
    jnz     .BufferOK
    
    mov     eax, FR_NOT_ENOUGH_CORE
    jmp     .EndProc
    
.BufferOK:

    stdcall Floppy.Read, dword[drive], 0, 1, dword[bootSector]
    test    eax, eax
    jnz     .ReadOK
    
    stdcall KernelMemManager.Free, [bootSector]
    mov     eax, FR_DISK_ERR
    jmp     .EndProc
    
.ReadOK:
    mov     esi, [bootSector]
    
    mov     ax, [esi + 510]
    cmp     ax, 0xAA55
    je      .SignatureOK
    
    stdcall KernelMemManager.Free, [bootSector]
    mov     eax, FR_NO_FILESYSTEM
    jmp     .EndProc
    
.SignatureOK:
    mov     edi, FAT16.FS
    
    mov     ax, [esi + BPB_BYTESPERSECT]
    mov     [edi + FAT16.BytesPerSector], ax
    
    mov     al, [esi + BPB_SECPERCLUS]
    mov     [edi + FAT16.SectorsPerCluster], al
    
    mov     ax, [esi + BPB_RSVDSECCNT]
    mov     [edi + FAT16.ReservedSectors], ax
    
    mov     al, [esi + BPB_NUMFATS]
    mov     [edi + FAT16.NumberOfFATs], al
    
    mov     ax, [esi + BPB_ROOTENTCNT]
    mov     [edi + FAT16.RootEntries], ax
    
    mov     ax, [esi + BPB_TOTSEC16]
    test    ax, ax
    jnz     .Use16Bit
    mov     eax, [esi + BPB_TOTSEC32]
    jmp     .StoreTotal
.Use16Bit:
    movzx   eax, ax
.StoreTotal:
    mov     [edi + FAT16.TotalSectors], eax
    
    mov     ax, [esi + BPB_FATSZ16]
    mov     [edi + FAT16.SectorsPerFAT], ax
    
    mov     ax, [esi + BPB_SECPERTRK]
    mov     [edi + FAT16.SectorsPerTrack], ax
    
    mov     ax, [esi + BPB_NUMHEADS]
    mov     [edi + FAT16.NumberOfHeads], ax
    
    mov     eax, [esi + BPB_HIDDSEC]
    mov     [edi + FAT16.HiddenSectors], eax
    
    mov     al, [drive]
    mov     [edi + FAT16.Drive], al
    
    movzx   eax, word [edi + FAT16.ReservedSectors]
    mov     [edi + FAT16.FATStart], eax
    
    movzx   ebx, byte [edi + FAT16.NumberOfFATs]
    movzx   ecx, word [edi + FAT16.SectorsPerFAT]
    imul    ebx, ecx
    add     eax, ebx
    mov     [edi + FAT16.RootStart], eax
    
    movzx   ebx, word [edi + FAT16.RootEntries]
    shl     ebx, 5          ; *32 (directory entry size)
    movzx   ecx, word [edi + FAT16.BytesPerSector]
    dec     ecx
    add     ebx, ecx
    shr     ebx, 9          ; /512
    mov     ecx, ebx        ; Save root sector count
    
    add     eax, ebx
    mov     [edi + FAT16.DataStart], eax
    
    STOP_POINT
    mov     ebx, [edi + FAT16.TotalSectors]
    sub     ebx, eax        ; Subtract data start from total
    movzx   eax, byte [edi + FAT16.SectorsPerCluster]
    xchg    ebx, eax
    xor     edx, edx
    div     ebx             ; Divide by sectors per cluster
    mov     [edi + FAT16.TotalClusters], eax
    
    cmp     eax, 4085
    ;jb      .FAT12
    cmp     eax, 65525
    jb      .FAT16
    mov     byte [edi + FAT16.FATType], 2  ; FAT32
    jmp     .TypeDone
.FAT12:
    mov     byte [edi + FAT16.FATType], 0
    jmp     .TypeDone
.FAT16:
    mov     byte [edi + FAT16.FATType], 1
.TypeDone:
    
    cmp     byte [edi + FAT16.FATType], 1
    je      .IsFAT16
    STOP_POINT
    stdcall KernelMemManager.Free, [bootSector]
    mov     eax, FR_NO_FILESYSTEM
    jmp     .EndProc
   
.IsFAT16:
    
    mov     byte [edi + FAT16.Mounted], 1

    mov     byte [edi + FAT16.CacheDirty], 0
    mov     dword [edi + FAT16.CacheSector], 0xFFFFFFFF

    mov     byte [edi + FAT16.FATCacheDirty], 0
    mov     dword [edi + FAT16.FATCacheSector], 0xFFFFFFFF

    mov     dword [edi + FAT16.FreeClusters], 0xFFFFFFFF
    mov     dword [edi + FAT16.LastAllocCluster], 2
    
    stdcall KernelMemManager.Free, [bootSector]
    
    stdcall VGA.PutString, Str.FAT16Mounted
    mov     al, [drive]
    add     al, 'A'
    stdcall VGA.PutChar, eax
    stdcall VGA.PutString, Str.ColonSpace
    
    mov     esi, [bootSector]
    add     esi, BS_VOLLAB
    cmp     byte [esi], ' '
    je      .NoLabel
    
    mov     ecx, 11
.PrintLabel:
    lodsb
    cmp     al, ' '
    je      .LabelDone
    stdcall VGA.PutChar, eax
    loop    .PrintLabel
.LabelDone:
    stdcall VGA.PutString, Str.NewLine
.NoLabel:
    
    mov     eax, FR_OK
.EndProc:    
    ret
endp


}

block(.data){
    ; Global variables
    FAT16.GlobalMutex     db ?
    FAT16.FileMutex       db ?
    FAT16.DirMutex        db ?
    
    FAT16.FS              db FAT16Size dup ?  ; Filesystem structure
    
    FAT16.OpenFiles       dd MAX_FILES * FileSize dup ?  ; Open files table
    FAT16.OpenDirs        dd MAX_DIRS * DirSize dup ?   ; Open directories table
    
    FAT16.OpenFileCount   dd ?
    FAT16.OpenDirCount    dd ?
}

block(.initData){
    Str.FAT16Init         db "FAT16 driver initialized", 13, 10, 0
    Str.FAT16Mounted      db "FAT16 mounted on drive ", 0
    Str.ColonSpace        db ": ", 0
    ;Str.NewLine           db 13, 10, 0
    
    ; Test strings
    Str.TestingFAT16      db "Testing FAT16 driver...", 13, 10, 0
    Str.MountFailed       db "Mount failed: ", 0
    Str.MountOK           db "Mount successful", 13, 10, 0
    Str.FileNotFound      db "File not found", 13, 10, 0
    Str.FileFound         db "File found", 13, 10, 0
    Str.FileSize          db "File size: ", 0
    Str.Bytes             db " bytes", 13, 10, 0
    Str.NoMemory          db "Not enough memory", 13, 10, 0
    Str.LoadFailed        db "Load failed: ", 0
    Str.LoadOK            db "Load successful: ", 0
    Str.BytesRead         db " bytes read", 13, 10, 0
    Str.FirstBytes        db "First bytes: ", 0
    Str.TestComplete      db "Test complete", 13, 10, 0
}