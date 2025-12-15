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
    
    mov     ebx, [edi + FAT16.TotalSectors]
    sub     ebx, eax        ; Subtract data start from total
    movzx   eax, byte [edi + FAT16.SectorsPerCluster]
    xor     edx, edx
    div     eax             ; Divide by sectors per cluster
    mov     [edi + FAT16.TotalClusters], eax
    
    cmp     eax, 4085
    jb      .FAT12
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

; ============================================
; FAT16_Unmount - Unmount filesystem
; Input:  drive - Drive number
; Output: EAX = Error code
; ============================================
proc FAT16.Unmount, drive: BYTE
    movzx eax, [drive]
    cmp al, [FAT16.FS + FAT16.Drive]
    je .DriveMatch
    
    mov eax, FR_INVALID_DRIVE
    ret
    
.DriveMatch:
    ; Check for open files
    cmp [FAT16.OpenFileCount], 0
    je .NoOpenFiles
    
    mov eax, FR_LOCKED
    ret
    
.NoOpenFiles:
    ; Flush caches
    stdcall FAT16.Sync, [drive]
    
    ; Mark as unmounted
    mov byte [FAT16.FS + FAT16.Mounted], 0
    
    mov eax, FR_OK
    ret
endp

; ============================================
; FAT16_Sync - Flush cached data to disk
; Input:  drive - Drive number
; Output: EAX = Error code
; ============================================
proc FAT16.Sync uses ebx, drive: BYTE
    mov ebx, FAT16.FS
    
    ; Check if mounted
    cmp byte [ebx + FAT16.Mounted], 0
    jne .Mounted
    
    mov eax, FR_NOT_ENABLED
    ret
    
.Mounted:
    ; Flush FAT cache
    cmp byte [ebx + FAT16.FATCacheDirty], 0
    je .FATClean
    
    push ebx
    stdcall FAT16.WriteFATSector, [ebx + FAT16.FATCacheSector]
    pop ebx
    cmp eax, FR_OK
    jne .Error
    
    mov byte [ebx + FAT16.FATCacheDirty], 0
    
.FATClean:
    ; Flush data cache
    cmp byte [ebx + FAT16.CacheDirty], 0
    je .DataClean
    
    push ebx
    stdcall FAT16.WriteSector, [ebx + FAT16.CacheSector]
    pop ebx
    cmp eax, FR_OK
    jne .Error
    
    mov byte [ebx + FAT16.CacheDirty], 0
    
.DataClean:
    mov eax, FR_OK
    ret
    
.Error:
    mov eax, FR_DISK_ERR
    ret
endp

; ============================================
; Internal helper functions
; ============================================

; Read sector into cache
proc FAT16.ReadSector uses ebx esi edi, sector: DWORD
    mov ebx, FAT16.FS
    
    ; Check if already cached
    mov eax, [sector]
    cmp eax, [ebx + FAT16.CacheSector]
    je .Cached
    
    ; Flush cache if dirty
    cmp byte [ebx + FAT16.CacheDirty], 0
    je .NotDirty
    
    push eax
    stdcall FAT16.WriteSector, [ebx + FAT16.CacheSector]
    pop eax
    cmp eax, FR_OK
    jne .Error
    
.NotDirty:
    ; Read new sector
    movzx ecx, byte [ebx + FAT16.Drive]
    mov edx, [sector]
    mov esi, [ebx + FAT16.CacheBuffer]
    
    push ebx
    stdcall Floppy.Read, ecx, edx, 1, esi
    pop ebx
    test eax, eax
    jnz .ReadOK
    
.Error:
    mov eax, FR_DISK_ERR
    ret
    
.ReadOK:
    mov eax, [sector]
    mov [ebx + FAT16.CacheSector], eax
    mov byte [ebx + FAT16.CacheDirty], 0
    
.Cached:
    mov eax, FR_OK
    ret
endp

; Write cached sector to disk
proc FAT16.WriteSector uses ebx esi, sector: DWORD
    mov ebx, FAT16.FS
    
    ; Check if write protected
    cmp byte [ebx + FAT16.WriteProtected], 0
    je .NotProtected
    
    mov eax, FR_WRITE_PROTECTED
    ret
    
.NotProtected:
    movzx ecx, byte [ebx + FAT16.Drive]
    mov edx, [sector]
    mov esi, [ebx + FAT16.CacheBuffer]
    
    ; For now, we'll just read. Write support needs to be added to Floppy driver
    ; stdcall Floppy.Write, ecx, edx, 1, esi
    
    ; Mark cache as clean
    mov byte [ebx + FAT16.CacheDirty], 0
    
    mov eax, FR_OK
    ret
endp

; Read FAT sector
proc FAT16.ReadFATSector uses ebx esi edi, sector: DWORD
    mov ebx, FAT16.FS
    
    ; Check if already cached
    mov eax, [sector]
    cmp eax, [ebx + FAT16.FATCacheSector]
    je .Cached
    
    ; Calculate actual sector (FAT start + sector)
    add eax, [ebx + FAT16.FATStart]
    
    ; Read sector
    movzx ecx, byte [ebx + FAT16.Drive]
    mov edx, eax
    mov esi, [ebx + FAT16.FATCacheBuffer]
    
    push ebx
    stdcall Floppy.Read, ecx, edx, 1, esi
    pop ebx
    test eax, eax
    jnz .ReadOK
    
    mov eax, FR_DISK_ERR
    ret
    
.ReadOK:
    mov eax, [sector]
    mov [ebx + FAT16.FATCacheSector], eax
    mov byte [ebx + FAT16.FATCacheDirty], 0
    
.Cached:
    mov eax, FR_OK
    ret
endp

; Write FAT sector
proc FAT16.WriteFATSector uses ebx esi, sector: DWORD
    mov ebx, FAT16.FS
    
    ; Check if write protected
    cmp byte [ebx + FAT16.WriteProtected], 0
    je .NotProtected
    
    mov eax, FR_WRITE_PROTECTED
    ret
    
.NotProtected:
    ; Calculate actual sector (FAT start + sector)
    mov eax, [sector]
    add eax, [ebx + FAT16.FATStart]
    
    movzx ecx, byte [ebx + FAT16.Drive]
    mov edx, eax
    mov esi, [ebx + FAT16.FATCacheBuffer]
    
    ; For now, just mark as written (actual write needs Floppy.Write support)
    ; stdcall Floppy.Write, ecx, edx, 1, esi
    
    ; Write to second FAT if it exists
    cmp byte [ebx + FAT16.NumberOfFATs], 1
    jbe .SingleFAT
    
    ; Calculate second FAT sector
    mov eax, [sector]
    add eax, [ebx + FAT16.FATStart]
    add eax, [ebx + FAT16.SectorsPerFAT]
    
    ; stdcall Floppy.Write, ecx, eax, 1, esi
    
.SingleFAT:
    mov byte [ebx + FAT16.FATCacheDirty], 0
    mov eax, FR_OK
    ret
endp

; Get FAT entry for cluster
proc FAT16.GetFAT uses ebx esi, cluster: WORD
    mov ebx, FAT16.FS
    movzx eax, [cluster]
    
    ; Calculate FAT sector for this cluster
    ; Each FAT entry is 2 bytes, 256 entries per sector (512 bytes)
    mov edx, eax
    shl edx, 1          ; cluster * 2 (bytes per entry)
    shr edx, 9          ; / 512 (sector size)
    
    push eax
    stdcall FAT16.ReadFATSector, edx
    pop eax
    cmp eax, FR_OK
    jne .Error
    
    ; Calculate offset within sector
    mov edx, eax
    shl edx, 1          ; cluster * 2
    and edx, 0x1FF      ; % 512
    
    ; Get FAT entry
    mov esi, [ebx + FAT16.FATCacheBuffer]
    add esi, edx
    mov ax, [esi]
    
    ; FAT16 entries are little-endian
    xchg al, ah
    ret
    
.Error:
    mov ax, 0xFFFF
    ret
endp

; Set FAT entry for cluster
proc FAT16.SetFAT uses ebx esi edi, cluster: WORD, value: WORD
    mov ebx, FAT16.FS
    
    ; Calculate FAT sector for this cluster
    movzx eax, [cluster]
    mov edx, eax
    shl edx, 1          ; cluster * 2
    shr edx, 9          ; / 512
    
    push eax
    stdcall FAT16.ReadFATSector, edx
    pop eax
    cmp eax, FR_OK
    jne .Error
    
    ; Calculate offset within sector
    mov edx, eax
    shl edx, 1          ; cluster * 2
    and edx, 0x1FF      ; % 512
    
    ; Set FAT entry
    mov esi, [ebx + FAT16.FATCacheBuffer]
    add esi, edx
    mov ax, [value]
    xchg al, ah
    mov [esi], ax
    
    ; Mark FAT cache as dirty
    mov byte [ebx + FAT16.FATCacheDirty], 1
    
    mov eax, FR_OK
    ret
    
.Error:
    mov eax, FR_DISK_ERR
    ret
endp

; Find free cluster
proc FAT16.FindFreeCluster uses ebx esi edi
    mov ebx, FAT16.FS
    
    ; Start from last allocated cluster
    mov eax, [ebx + FAT16.LastAllocCluster]
    cmp eax, 2
    jae .StartOK
    mov eax, 2
.StartOK:
    
    mov ecx, [ebx + FAT16.TotalClusters]
    add ecx, 2
    
.SearchLoop:
    push eax ecx
    stdcall FAT16.GetFAT, ax
    pop ecx eax
    cmp ax, FAT16_FREE_CLUSTER
    je .Found
    
    inc eax
    cmp eax, ecx
    jb .NotWrap
    
    ; Wrap to beginning
    mov eax, 2
    
.NotWrap:
    ; Check if we've searched all clusters
    cmp eax, [ebx + FAT16.LastAllocCluster]
    jne .Continue
    
    ; No free clusters
    mov eax, FR_DENIED
    mov edx, 0
    ret
    
.Continue:
    jmp .SearchLoop
    
.Found:
    ; Update last allocated cluster
    mov [ebx + FAT16.LastAllocCluster], eax
    
    ; Decrement free clusters count if known
    cmp dword [ebx + FAT16.FreeClusters], 0xFFFFFFFF
    je .CountUnknown
    dec dword [ebx + FAT16.FreeClusters]
    
.CountUnknown:
    mov edx, eax
    mov eax, FR_OK
    ret
endp

; Convert cluster to sector
proc FAT16.ClusterToSector uses ebx, cluster: WORD
    mov ebx, FAT16.FS
    movzx eax, [cluster]
    
    ; First data cluster is #2
    sub eax, 2
    
    ; Multiply by sectors per cluster
    movzx ecx, byte [ebx + FAT16.SectorsPerCluster]
    mul ecx
    
    ; Add data start sector
    add eax, [ebx + FAT16.DataStart]
    
    ret
endp

; ============================================
; File Operations
; ============================================

; Open file
proc FAT16.Open uses ebx esi edi, filename: DWORD, mode: BYTE
    locals
        fs          dd ?
        dirEntry    dd ?
        fileObj     dd ?
        result      dd ?
    endl
    
    stdcall Mutex.Wait, FAT16.FileMutex
    
    ; Check if we can open more files
    mov eax, [FAT16.OpenFileCount]
    cmp eax, MAX_FILES
    jb .CanOpen
    
    stdcall Mutex.Release, FAT16.FileMutex
    mov eax, FR_TOO_MANY_OPEN_FILES
    ret
    
.CanOpen:
    ; Get filesystem pointer
    mov [fs], FAT16.FS
    
    ; Find file in directory
    stdcall FAT16.FindFile, [filename], 0
    mov [result], eax
    mov [dirEntry], edx
    
    ; Check result
    cmp eax, FR_OK
    je .FileFound
    
    ; File not found
    test byte [mode], FA_CREATE_NEW
    jnz .CreateNew
    test byte [mode], FA_CREATE_ALWAYS
    jnz .CreateAlways
    test byte [mode], FA_OPEN_ALWAYS
    jnz .CreateAlways
    
    ; File must exist for other modes
    stdcall Mutex.Release, FAT16.FileMutex
    mov eax, FR_NO_FILE
    ret
    
.CreateNew:
    ; Check if file already exists
    cmp [result], FR_OK
    jne .CreateFile
    
    stdcall Mutex.Release, FAT16.FileMutex
    mov eax, FR_EXIST
    ret
    
.CreateAlways:
.CreateFile:
    ; Create new file
    stdcall FAT16.CreateFile, [filename]
    cmp eax, FR_OK
    je .FileCreated
    
    stdcall Mutex.Release, FAT16.FileMutex
    ret
    
.FileCreated:
    ; Find the created file
    stdcall FAT16.FindFile, [filename], 0
    mov [result], eax
    mov [dirEntry], edx
    cmp eax, FR_OK
    je .FileFound
    
    stdcall Mutex.Release, FAT16.FileMutex
    mov eax, FR_DISK_ERR
    ret
    
.FileFound:
    ; Allocate file object
    stdcall KernelMemManager.Malloc, FileSize
    mov [fileObj], eax
    test eax, eax
    jnz .ObjOK
    
    stdcall Mutex.Release, FAT16.FileMutex
    mov eax, FR_NOT_ENOUGH_CORE
    ret
    
.ObjOK:
    ; Initialize file object
    mov edi, eax
    mov eax, FileSize
    stosd                           ; ObjSize
    
    mov eax, [fs]
    stosd                           ; FS
    
    ; Get directory entry info
    mov esi, [dirEntry]
    
    ; Starting cluster
    mov ax, [esi + DIR_FST_CLUS_LO]
    stosw                           ; Cluster
    add edi, 2                      ; Skip padding
    
    ; Current sector (initialize to 0)
    xor eax, eax
    stosd                           ; Sector
    
    ; File pointer (start at beginning)
    stosd                           ; Pointer
    
    ; File size
    mov eax, [esi + DIR_FILE_SIZE]
    stosd                           ; Size
    
    ; Directory sector and index (stored when finding file)
    ; These would need to be saved during FindFile
    
    ; Flags and mode
    mov al, 0                       ; Flags
    stosb
    mov al, [mode]
    stosb                           ; Mode
    add edi, 2                      ; Padding
    
    ; Add to open files table
    mov ecx, [FAT16.OpenFileCount]
    mov eax, [fileObj]
    mov [FAT16.OpenFiles + ecx * 4], eax
    inc [FAT16.OpenFileCount]
    
    stdcall Mutex.Release, FAT16.FileMutex
    
    mov eax, FR_OK
    mov edx, [fileObj]
    ret
endp

; Close file
proc FAT16.Close uses ebx esi, fileObj: DWORD
    mov ebx, [fileObj]
    test ebx, ebx
    jnz .ValidObj
    
    mov eax, FR_INVALID_OBJECT
    ret
    
.ValidObj:
    stdcall Mutex.Wait, FAT16.FileMutex
    
    ; Sync file if needed
    ; (In a full implementation, we'd flush buffers here)
    
    ; Remove from open files table
    mov ecx, [FAT16.OpenFileCount]
    mov edx, FAT16.OpenFiles
    
.SearchLoop:
    dec ecx
    js .NotFound
    mov eax, [edx + ecx * 4]
    cmp eax, ebx
    jne .SearchLoop
    
    ; Found it, remove by shifting
    mov esi, edx
    add esi, ecx * 4
    lea edi, [esi + 4]
    mov ecx, [FAT16.OpenFileCount]
    sub ecx, [edx + ecx * 4]  ; Calculate remaining count
    rep movsd
    
    dec [FAT16.OpenFileCount]
    
.NotFound:
    ; Free file object
    stdcall KernelMemManager.Free, ebx
    
    stdcall Mutex.Release, FAT16.FileMutex
    mov eax, FR_OK
    ret
endp

; Read from file
proc FAT16.Read uses ebx esi edi, fileObj: DWORD, buffer: DWORD, bytesToRead: DWORD, bytesRead: DWORD
    locals
        fs          dd ?
        cluster     dw ?
        sector      dd ?
        offsetInSect dd ?
        bytesLeft   dd ?
        totalRead   dd ?
    endl
    
    mov ebx, [fileObj]
    test ebx, ebx
    jnz .ValidObj
    
    mov eax, FR_INVALID_OBJECT
    ret
    
.ValidObj:
    ; Check if file is open for reading
    test byte [ebx + File.Mode], FA_READ
    jnz .CanRead
    
    mov eax, FR_DENIED
    ret
    
.CanRead:
    ; Get filesystem
    mov eax, [ebx + File.FS]
    mov [fs], eax
    
    ; Initialize
    mov eax, [bytesToRead]
    mov [bytesLeft], eax
    mov dword [totalRead], 0
    mov esi, [buffer]
    
    ; Check if at EOF
    mov eax, [ebx + File.Pointer]
    cmp eax, [ebx + File.Size]
    jb .NotEOF
    
    ; At EOF, return 0 bytes read
    mov edi, [bytesRead]
    mov dword [edi], 0
    mov eax, FR_OK
    ret
    
.NotEOF:
.ReadLoop:
    cmp [bytesLeft], 0
    jle .Done
    
    ; Calculate current cluster and sector
    mov eax, [ebx + File.Pointer]
    
    ; If no cluster allocated (empty file)
    cmp word [ebx + File.Cluster], 0
    jne .HasCluster
    
    ; Empty file, nothing to read
    jmp .Done
    
.HasCluster:
    ; Calculate sector within cluster
    movzx ecx, byte [eax + FAT16.SectorsPerCluster]
    push eax
    mov eax, [ebx + File.Pointer]
    xor edx, edx
    mov ecx, 512
    div ecx                     ; Pointer / 512
    mov [sector], eax
    mov [offsetInSect], edx
    pop eax
    
    ; Get current cluster chain position
    ; This is simplified - we need to follow cluster chain
    mov ax, [ebx + File.Cluster]
    mov [cluster], ax
    
    ; Calculate sector from cluster
    stdcall FAT16.ClusterToSector, [cluster]
    add eax, [sector]
    
    ; Read sector
    push ebx esi
    mov ebx, [fs]
    stdcall FAT16.ReadSector, eax
    pop esi ebx
    cmp eax, FR_OK
    jne .Error
    
    ; Copy data from sector
    mov edi, [fs]
    mov eax, [edi + FAT16.CacheBuffer]
    add eax, [offsetInSect]
    
    ; Calculate how much to read from this sector
    mov ecx, 512
    sub ecx, [offsetInSect]     ; Bytes remaining in sector
    cmp ecx, [bytesLeft]
    jbe .ReadAmountOK
    mov ecx, [bytesLeft]
    
.ReadAmountOK:
    ; Copy data
    push esi edi
    mov edi, esi
    mov esi, eax
    rep movsb
    pop edi esi
    
    ; Update pointers
    add esi, ecx
    add [totalRead], ecx
    sub [bytesLeft], ecx
    mov eax, [ebx + File.Pointer]
    add eax, ecx
    mov [ebx + File.Pointer], eax
    
    ; Check for EOF
    cmp eax, [ebx + File.Size]
    jb .ContinueRead
    
    ; Reached EOF
    jmp .Done
    
.ContinueRead:
    jmp .ReadLoop
    
.Done:
    mov edi, [bytesRead]
    mov eax, [totalRead]
    mov [edi], eax
    mov eax, FR_OK
    ret
    
.Error:
    mov edi, [bytesRead]
    mov eax, [totalRead]
    mov [edi], eax
    mov eax, FR_DISK_ERR
    ret
endp

; Write to file (simplified - needs full implementation)
proc FAT16.Write uses ebx, fileObj: DWORD, buffer: DWORD, bytesToWrite: DWORD, bytesWritten: DWORD
    ; For now, return "not implemented"
    mov eax, FR_DENIED
    ret
endp

; Seek in file
proc FAT16.Seek uses ebx, fileObj: DWORD, offset: DWORD, origin: DWORD
    mov ebx, [fileObj]
    test ebx, ebx
    jnz .ValidObj
    
    mov eax, FR_INVALID_OBJECT
    ret
    
.ValidObj:
    mov eax, [origin]
    cmp eax, 0          ; SEEK_SET
    je .SeekSet
    cmp eax, 1          ; SEEK_CUR
    je .SeekCur
    cmp eax, 2          ; SEEK_END
    je .SeekEnd
    
    mov eax, FR_INVALID_PARAMETER
    ret
    
.SeekSet:
    mov eax, [offset]
    jmp .SetPointer
    
.SeekCur:
    mov eax, [ebx + File.Pointer]
    add eax, [offset]
    jmp .SetPointer
    
.SeekEnd:
    mov eax, [ebx + File.Size]
    add eax, [offset]
    
.SetPointer:
    ; Check bounds
    cmp eax, [ebx + File.Size]
    jbe .PointerOK
    
    ; Clamp to file size
    mov eax, [ebx + File.Size]
    
.PointerOK:
    mov [ebx + File.Pointer], eax
    
    ; TODO: Update current cluster/sector based on new position
    
    mov eax, FR_OK
    ret
endp

; ============================================
; Directory Operations
; ============================================

; Find file in directory
proc FAT16.FindFile uses ebx esi edi, filename: DWORD, startCluster: WORD
    locals
        fs          dd ?
        nameBuf     db 13 dup ?
        extBuf      db 4 dup ?
        hasExt      db ?
        dirCluster  dw ?
        dirSector   dd ?
        dirIndex    dw ?
        entry       dd ?
    endl
    
    ; Get filesystem
    mov [fs], FAT16.FS
    
    ; Parse filename
    stdcall FAT16.ParseFilename, [filename], nameBuf, extBuf
    cmp eax, FR_OK
    je .NameOK
    
    ret
    
.NameOK:
    ; Check if searching root directory
    cmp [startCluster], 0
    jne .SearchSubdir
    
    ; Search root directory
    mov eax, [fs]
    mov eax, [eax + FAT16.RootStart]
    mov [dirSector], eax
    
    ; Calculate root directory size in sectors
    mov ebx, [fs]
    movzx ecx, word [ebx + FAT16.RootEntries]
    shl ecx, 5          ; *32
    add ecx, 511
    shr ecx, 9          ; /512
    
    mov [dirIndex], 0
    
.RootSearchLoop:
    ; Read directory sector
    push ecx
    stdcall FAT16.ReadSector, [dirSector]
    pop ecx
    cmp eax, FR_OK
    jne .DiskError
    
    ; Search in this sector
    mov ebx, [fs]
    mov esi, [ebx + FAT16.CacheBuffer]
    mov edx, 16         ; 16 entries per sector (512/32)
    
.SectorSearch:
    ; Check if entry is in use
    mov al, [esi]
    test al, al
    jz .NotFound        ; End of directory
    cmp al, 0xE5
    je .SkipEntry       ; Deleted entry
    
    ; Check if long filename entry
    mov al, [esi + DIR_ATTR]
    cmp al, ATTR_LONG_NAME
    je .SkipEntry
    
    ; Compare filename
    push esi edi
    lea edi, [esi + DIR_NAME]
    lea esi, [nameBuf]
    mov ecx, 11
    repe cmpsb
    pop edi esi
    jne .SkipEntry
    
    ; Found it!
    mov eax, FR_OK
    mov edx, esi        ; Return pointer to entry
    ret
    
.SkipEntry:
    add esi, 32
    inc [dirIndex]
    dec edx
    jnz .SectorSearch
    
    ; Next sector
    inc [dirSector]
    dec ecx
    jnz .RootSearchLoop
    
    ; Not found in root
    jmp .NotFound
    
.SearchSubdir:
    ; TODO: Implement subdirectory search
    ; This requires following cluster chains
    
.NotFound:
    mov eax, FR_NO_FILE
    mov edx, 0
    ret
    
.DiskError:
    mov eax, FR_DISK_ERR
    mov edx, 0
    ret
endp

; Create new file
proc FAT16.CreateFile uses ebx esi edi, filename: DWORD
    ; TODO: Implement file creation
    ; This requires finding free directory entry and allocating cluster
    
    mov eax, FR_DENIED  ; Not implemented yet
    ret
endp

; Parse filename to 8.3 format
proc FAT16.ParseFilename uses esi edi ebx, filename: DWORD, nameBuf: DWORD, extBuf: DWORD
    mov esi, [filename]
    mov edi, [nameBuf]
    mov ebx, [extBuf]
    
    ; Clear buffers
    push edi
    mov ecx, 11
    mov al, ' '
    rep stosb
    pop edi
    
    push ebx
    mov ecx, 3
    rep stosb
    pop ebx
    
    ; Find extension
    mov edx, esi
.FindDot:
    lodsb
    test al, al
    jz .NoExtension
    cmp al, '.'
    jne .FindDot
    
    ; Found dot
    dec esi
    mov byte [esi], 0
    inc esi
    jmp .HasExtension
    
.NoExtension:
    mov esi, edx
    jmp .CopyName
    
.HasExtension:
    ; Copy extension (max 3 chars)
    mov ecx, 3
    mov edi, ebx
.CopyExt:
    lodsb
    test al, al
    jz .ExtDone
    cmp al, ' '
    je .ExtDone
    stosb
    loop .CopyExt
    
.ExtDone:
    mov esi, edx
    
.CopyName:
    ; Copy filename (max 8 chars)
    mov ecx, 8
    mov edi, [nameBuf]
.CopyNameLoop:
    lodsb
    test al, al
    jz .NameDone
    cmp al, '.'
    je .NameDone
    cmp al, ' '
    je .NameDone
    stosb
    loop .CopyNameLoop
    
.NameDone:
    ; Convert to uppercase
    stdcall FAT16.StringToUpper, [nameBuf]
    stdcall FAT16.StringToUpper, [extBuf]
    
    mov eax, FR_OK
    ret
endp

; Convert string to uppercase
proc FAT16.StringToUpper uses esi, str: DWORD
    mov esi, [str]
    
.Loop:
    mov al, [esi]
    test al, al
    jz .Done
    cmp al, 'a'
    jb .Next
    cmp al, 'z'
    ja .Next
    sub al, 0x20
    mov [esi], al
.Next:
    inc esi
    jmp .Loop
    
.Done:
    ret
endp

; ============================================
; Directory Listing
; ============================================

; Open directory
proc FAT16.OpenDir uses ebx, path: DWORD
    ; TODO: Implement directory opening
    mov eax, FR_DENIED  ; Not implemented yet
    ret
endp

; Read directory entry
proc FAT16.ReadDir uses ebx, dirObj: DWORD, fileInfo: DWORD
    ; TODO: Implement directory reading
    mov eax, FR_DENIED  ; Not implemented yet
    ret
endp

; Close directory
proc FAT16.CloseDir uses ebx, dirObj: DWORD
    ; TODO: Implement directory closing
    mov eax, FR_DENIED  ; Not implemented yet
    ret
endp

; ============================================
; Utility Functions
; ============================================

; Get free space
proc FAT16.GetFree uses ebx
    mov ebx, FAT16.FS
    
    ; Check if mounted
    cmp byte [ebx + FAT16.Mounted], 0
    jne .Mounted
    
    mov eax, FR_NOT_ENABLED
    mov edx, 0
    ret
    
.Mounted:
    ; If free clusters count is unknown, calculate it
    cmp dword [ebx + FAT16.FreeClusters], 0xFFFFFFFF
    jne .Known
    
    ; TODO: Count free clusters by scanning FAT
    mov eax, FR_OK
    mov edx, 0xFFFFFFFF  ; Unknown
    ret
    
.Known:
    ; Calculate free space in bytes
    mov eax, [ebx + FAT16.FreeClusters]
    movzx ecx, byte [ebx + FAT16.SectorsPerCluster]
    mul ecx
    movzx ecx, word [ebx + FAT16.BytesPerSector]
    mul ecx
    
    mov edx, eax        ; Free space in bytes
    mov eax, FR_OK
    ret
endp

; Get filesystem info
proc FAT16.GetInfo uses ebx esi, infoBuf: DWORD
    mov ebx, FAT16.FS
    mov esi, [infoBuf]
    
    ; Check if mounted
    cmp byte [ebx + FAT16.Mounted], 0
    jne .Mounted
    
    mov eax, FR_NOT_ENABLED
    ret
    
.Mounted:
    ; Fill info structure
    ; TODO: Fill with actual filesystem information
    
    mov eax, FR_OK
    ret
endp

; ============================================
; High-level API
; ============================================

; Load file to memory
proc FAT16.LoadFile uses ebx esi edi, filename: DWORD, buffer: DWORD, maxSize: DWORD, bytesRead: DWORD
    locals
        fileObj     dd ?
        result      dd ?
    endl
    
    ; Open file
    stdcall FAT16.Open, [filename], FA_READ
    cmp eax, FR_OK
    je .OpenOK
    
    mov [result], eax
    jmp .Error
    
.OpenOK:
    mov [fileObj], edx
    
    ; Read file
    stdcall FAT16.Read, [fileObj], [buffer], [maxSize], [bytesRead]
    mov [result], eax
    
    ; Close file
    push eax
    stdcall FAT16.Close, [fileObj]
    pop eax
    
.Error:
    mov eax, [result]
    ret
endp

; Check if file exists
proc FAT16.FileExists, filename: DWORD
    ; Try to find the file
    stdcall FAT16.FindFile, [filename], 0
    cmp eax, FR_OK
    je .Exists
    
    ; File not found or error
    xor eax, eax
    ret
    
.Exists:
    mov eax, 1
    ret
endp

; Get file size
proc FAT16.GetFileSize uses ebx, filename: DWORD
    locals
        dirEntry    dd ?
    endl
    
    ; Find file
    stdcall FAT16.FindFile, [filename], 0
    cmp eax, FR_OK
    je .Found
    
    mov eax, 0xFFFFFFFF  ; Error
    ret
    
.Found:
    ; Get size from directory entry
    mov eax, [edx + DIR_FILE_SIZE]
    ret
endp

; ============================================
; Test Functions
; ============================================

; Test FAT16 driver
proc FAT16.Test uses ebx esi edi
    locals
        buffer      dd ?
        bytesRead   dd ?
        filename    db "KERNEL  SYS",0
    endl
    
    stdcall VGA.PutString, Str.TestingFAT16
    
    ; Mount drive A:
    stdcall FAT16.Mount, 0
    cmp eax, FR_OK
    je .MountOK
    
    stdcall VGA.PutString, Str.MountFailed
    stdcall VGA.PrintDec, eax
    stdcall VGA.PutString, Str.NewLine
    ret
    
.MountOK:
    stdcall VGA.PutString, Str.MountOK
    
    ; Check if KERNEL.SYS exists
    stdcall FAT16.FileExists, filename
    test eax, eax
    jnz .FileExists
    
    stdcall VGA.PutString, Str.FileNotFound
    jmp .Cleanup
    
.FileExists:
    stdcall VGA.PutString, Str.FileFound
    
    ; Get file size
    stdcall FAT16.GetFileSize, filename
    stdcall VGA.PutString, Str.FileSize
    stdcall VGA.PrintDec, eax
    stdcall VGA.PutString, Str.Bytes
    
    ; Allocate buffer
    cmp eax, 0
    je .NoSize
    cmp eax, 0xFFFFFFFF
    je .NoSize
    
    push eax
    stdcall KernelMemManager.Malloc, eax
    mov [buffer], eax
    pop ecx
    test eax, eax
    jnz .BufferOK
    
    stdcall VGA.PutString, Str.NoMemory
    jmp .Cleanup
    
.BufferOK:
    ; Load file
    stdcall FAT16.LoadFile, filename, [buffer], ecx, bytesRead
    cmp eax, FR_OK
    je .LoadOK
    
    stdcall VGA.PutString, Str.LoadFailed
    stdcall VGA.PrintDec, eax
    stdcall VGA.PutString, Str.NewLine
    jmp .FreeBuffer
    
.LoadOK:
    stdcall VGA.PutString, Str.LoadOK
    mov eax, [bytesRead]
    stdcall VGA.PrintDec, eax
    stdcall VGA.PutString, Str.BytesRead
    
    ; Display first few bytes
    stdcall VGA.PutString, Str.FirstBytes
    mov esi, [buffer]
    mov ecx, 16
    cmp ecx, [bytesRead]
    jbe .DisplayLoop
    mov ecx, [bytesRead]
    
.DisplayLoop:
    lodsb
    stdcall VGA.PrintHexByte, eax
    stdcall VGA.PutChar, ' '
    loop .DisplayLoop
    
    stdcall VGA.PutString, Str.NewLine
    
.FreeBuffer:
    stdcall KernelMemManager.Free, [buffer]
    
.NoSize:
.Cleanup:
    ; Unmount
    stdcall FAT16.Unmount, 0
    
    stdcall VGA.PutString, Str.TestComplete
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