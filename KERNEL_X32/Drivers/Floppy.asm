block(.consts) {
    FLOPPY_DOR         equ 0x3F2   ; Digital Output Register
    FLOPPY_MSR         equ 0x3F4   ; Main Status Register
    FLOPPY_FIFO        equ 0x3F5   ; Data FIFO
    FLOPPY_CCR         equ 0x3F7   ; Configuration Control Register
    

    DOR_MOTOR_A        equ 0x01    ; Motor A enable
    DOR_MOTOR_B        equ 0x02    ; Motor B enable
    DOR_MOTOR_C        equ 0x04    ; Motor C enable
    DOR_MOTOR_D        equ 0x08    ; Motor D enable
    DOR_DMA            equ 0x08    ; Enable DMA
    DOR_RESET          equ 0x04    ; Reset FDC
    DOR_SELECT0        equ 0x00    ; Select drive 0
    DOR_SELECT1        equ 0x01    ; Select drive 1
    DOR_SELECT2        equ 0x02    ; Select drive 2
    DOR_SELECT3        equ 0x03    ; Select drive 3
    
    MSR_DRQ            equ 0x08    ; Data Request
    MSR_BUSY           equ 0x10    ; FDC is busy
    
    CMD_READ           equ 0xE6    ; Read command
    CMD_WRITE          equ 0xC5    ; Write command
    CMD_SEEK           equ 0x0F    ; Seek command
    CMD_RECALIBRATE    equ 0x07    ; Recalibrate command
    CMD_SENSEI         equ 0x08    ; Sense interrupt status
    CMD_SPECIFY        equ 0x03    ; Specify parameters
    
    ; Drive parameters (1.44MB floppy)
    SECTORS_PER_TRACK  equ 18
    HEADS              equ 2
    CYLINDERS          equ 80
    SECTOR_SIZE        equ 512
    MAX_SECTORS_PER_OP equ 18      ; Max sectors per operation (1 track)
    DMA_BUFFER_SIZE    equ (MAX_SECTORS_PER_OP * SECTOR_SIZE) ; 9KB
    
    DMA1_CHAN2_ADDR    equ 0x04
    DMA1_CHAN2_COUNT   equ 0x05
    DMA1_MASK_REG      equ 0x0A
    DMA1_MODE_REG      equ 0x0B
    DMA1_CLEAR_FF      equ 0x0C
    DMA1_MASTER_CLEAR  equ 0x0D
    DMA2_PAGE_REG      equ 0x81    ; Page register for channel 2

    DMA_MODE_READ      equ 0x46    ; 01000110b: Channel 2, read, single, auto-init
    DMA_MODE_WRITE     equ 0x4A    ; 01001010b: Channel 2, write, single, auto-init
    
    DMA_MAX_ADDR       equ 0x01000000  ; 16MB limit
    DMA_BOUNDARY_SIZE  equ 0x10000     ; 64KB boundary
    
    KERNEL_DMA_BUFFER  equ 0xF1100000  ; Fixed address in kernel zone
}

block(.structs) {
virtual at 0
    Floppy.DMABufferPhys dd ?     ; Physical address of DMA buffer
    Floppy.DMABufferVirt dd ?     ; Virtual address of DMA buffer (in kernel zone)
    Floppy.Cylinder      db ?     ; Current cylinder
    Floppy.Head          db ?     ; Current head
    Floppy.Sector        db ?     ; Current sector
    Floppy.Drive         db ?     ; Current drive (0-3)
    Floppy.MotorStat     db ?     ; Motor status (0=off, 1=on)
    Floppy.MotorTicks    dd ?     ; Ticks until motor off
end virtual
}

block(.text) {
proc Floppy.Init
    ; Initialize mutex
    stdcall Mutex.Start, Floppy.Mutex
    
    ; Allocate and map DMA-compatible buffer in kernel zone
    stdcall Floppy.SetupDMABuffer
    
    ; Reset FDC
    stdcall Floppy.Reset
    
    ; Configure DMA for floppy (channel 2)
    stdcall Floppy.SetupDMA
    
    ; Set floppy parameters
    stdcall Floppy.Configure
    
    ; Start motor timeout thread
    stdcall Threads.Create, 0, Floppy.MotorTimeoutThread
    
    ret
endp

proc Floppy.SetupDMABuffer
    ; Allocate physical page for DMA buffer (must be below 16MB)
    mov ecx, 100  ; Max attempts
    
.alloc_loop:
    push ecx
    stdcall FramePool.GetFreePage
    pop ecx
    
    ; Check if physical address < 16MB
    cmp eax, (DMA_MAX_ADDR shr 12)
    jae .try_again  ; Address too high
    
    ; Found suitable page
    mov [Floppy.DMABufferPhys], eax
    
    ; Map physical page to kernel zone virtual address
    stdcall Pager.MapPage, KERNEL_DMA_BUFFER shr 12, eax, AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
    mov [Floppy.DMABufferVirt], KERNEL_DMA_BUFFER
    
    ; Check alignment - ensure buffer doesn't cross 64KB boundary
    mov eax, [Floppy.DMABufferPhys]
    shl eax, 12
    mov ebx, eax
    add ebx, DMA_BUFFER_SIZE - 1
    and eax, 0xFFFF0000
    and ebx, 0xFFFF0000
    cmp eax, ebx
    jne .alignment_bad
    
    ; Buffer is properly aligned
    ret
    
.alignment_bad:
    ; Unmap and free page, try again
    stdcall Pager.Unmap, KERNEL_DMA_BUFFER shr 12
    mov eax, [Floppy.DMABufferPhys]
    
.try_again:
    stdcall FramePool.FreePage, eax
    loop .alloc_loop
    
    ; Failed to allocate suitable DMA buffer
    ; Try to use a reserved page in low memory
    ; We'll use physical page 0x100 (1MB) as fallback
    mov dword [Floppy.DMABufferPhys], 0x100
    mov dword [Floppy.DMABufferVirt], KERNEL_DMA_BUFFER
    
    ; Map it to kernel zone
    stdcall Pager.MapPage, KERNEL_DMA_BUFFER shr 12, 0x100, AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
    
    ; Note: We assume this physical page is reserved for DMA
    ; In a real system, you'd need to ensure this page isn't used elsewhere
    
    ret
endp

proc Floppy.Reset
    ; Send reset signal to FDC
    mov al, 0
    mov dx, FLOPPY_DOR
    out dx, al
    
    ; Wait a bit
    mov ecx, 10000
@@:
    nop
    loop @b
    
    ; Re-enable FDC
    mov al, DOR_SELECT0 or DOR_RESET or DOR_DMA
    mov dx, FLOPPY_DOR
    out dx, al
    
    ; Wait for FDC to be ready
    stdcall Floppy.WaitForIRQ
    
    ; Read 4 result bytes (ignore them)
    mov ecx, 4
@@:
    call Floppy.ReadData
    loop @b
    
    ; Recalibrate drive
    stdcall Floppy.Recalibrate, 0
    
    ret
endp

proc Floppy.Configure
    ; Send SPECIFY command
    mov al, CMD_SPECIFY
    call Floppy.WriteData
    
    ; Step rate = 3ms, head unload time = 240ms
    mov al, 0xDF
    call Floppy.WriteData
    
    ; Head load time = 16ms, DMA mode
    mov al, 0x02
    call Floppy.WriteData
    
    ret
endp

proc Floppy.SetupDMA
    ; Disable channel 2
    mov al, 0x06        ; Mask channel 2
    out DMA1_MASK_REG, al
    
    ; Clear byte pointer flip-flop
    mov al, 0xFF
    out DMA1_CLEAR_FF, al
    
    ; Set mode for channel 2
    mov al, DMA_MODE_READ
    out DMA1_MODE_REG, al
    
    ; Unmask channel 2
    mov al, 0x02
    out DMA1_MASK_REG, al
    
    ret
endp

proc Floppy.MotorOn, drive:BYTE
    ; Check if motor is already on
    cmp [Floppy.MotorStat], 1
    je .motor_already_on
    
    ; Turn motor on for specified drive
    mov al, [drive]
    or al, DOR_MOTOR_A or DOR_RESET or DOR_DMA
    out FLOPPY_DOR, al
    
    ; Wait for motor to spin up (500ms)
    stdcall Timer.Sleep, 500
    
    mov [Floppy.MotorStat], 1
    
.motor_already_on:
    ; Reset motor timeout (5 seconds)
    mov [Floppy.MotorTicks], 5000
    
    ret
endp

proc Floppy.MotorOff
    ; Turn motor off
    mov al, DOR_RESET or DOR_DMA
    mov dx, FLOPPY_DOR
    out dx, al
    
    mov [Floppy.MotorStat], 0
    ret
endp

proc Floppy.MotorTimeoutThread
.timeout_loop:
    cmp [Floppy.MotorStat], 0
    je .no_motor
    
    dec [Floppy.MotorTicks]
    jnz .no_motor
    
    ; Motor timeout reached, turn off
    stdcall Floppy.MotorOff
    
.no_motor:
    stdcall Timer.Sleep, 1
    jmp .timeout_loop
endp

proc Floppy.LBA2CHS uses ebx, lba
    ; Convert LBA to CHS
    ; Input: EAX = LBA sector
    ; Output: CH = cylinder, CL = sector, DH = head
    
    ; Sector = (LBA % SECTORS_PER_TRACK) + 1
    xor edx, edx
    mov ebx, SECTORS_PER_TRACK
    div ebx
    mov cl, dl      ; Sector in CL
    inc cl
    
    ; Head = (LBA / SECTORS_PER_TRACK) % HEADS
    xor edx, edx
    mov ebx, HEADS
    div ebx
    mov dh, dl      ; Head in DH
    
    ; Cylinder = (LBA / SECTORS_PER_TRACK) / HEADS
    mov ch, al      ; Cylinder in CH
    
    ret
endp

proc Floppy.ReadSectors uses esi edi ebx, drive, lba, count, buffer
    ; Validate parameters
    cmp [count], 0
    je .done
    
    stdcall Mutex.Wait, Floppy.Mutex
    
    ; Turn motor on
    stdcall Floppy.MotorOn, [drive]
    
    mov esi, [buffer]      ; User buffer pointer
    mov ebx, [lba]         ; Current LBA
    mov edi, [count]       ; Remaining sectors
    
.read_loop:
    ; Calculate how many sectors we can transfer in this operation
    ; Limited by: MAX_SECTORS_PER_OP, sectors remaining, and track boundary
    mov eax, ebx
    xor edx, edx
    mov ecx, SECTORS_PER_TRACK
    div ecx                ; EDX = sector in track
    
    ; Sectors left in current track = SECTORS_PER_TRACK - EDX
    mov ecx, SECTORS_PER_TRACK
    sub ecx, edx
    
    ; Limit by remaining sectors
    cmp ecx, edi
    jbe @f
    mov ecx, edi
@@:
    
    ; Limit by MAX_SECTORS_PER_OP
    cmp ecx, MAX_SECTORS_PER_OP
    jbe @f
    mov ecx, MAX_SECTORS_PER_OP
@@:
    
    push ebx              ; Save current LBA
    push ecx              ; Save sector count for this operation
    
    ; Setup DMA for read operation
    stdcall Floppy.PrepareDMA, ecx, 0  ; 0 = read
    
    ; Convert LBA to CHS for first sector
    mov eax, ebx
    call Floppy.LBA2CHS
    
    ; Send read command
    mov al, CMD_READ
    call Floppy.WriteData
    
    ; Send parameters
    mov al, dh      ; Head << 2 | Drive
    shl al, 2
    or al, [drive]
    call Floppy.WriteData
    
    mov al, ch      ; Cylinder
    call Floppy.WriteData
    
    mov al, dh      ; Head
    call Floppy.WriteData
    
    mov al, cl      ; Sector
    call Floppy.WriteData
    
    mov al, 2       ; Sector size (512 bytes)
    call Floppy.WriteData
    
    mov al, SECTORS_PER_TRACK  ; Sectors per track
    call Floppy.WriteData
    
    mov al, 0x1B    ; GAP length
    call Floppy.WriteData
    
    mov al, 0xFF    ; Data length
    call Floppy.WriteData
    
    ; Wait for operation to complete
    stdcall Floppy.WaitForIRQ
    
    ; Read result bytes
    call Floppy.ReadData  ; ST0
    call Floppy.ReadData  ; ST1
    call Floppy.ReadData  ; ST2
    call Floppy.ReadData  ; Cylinder
    call Floppy.ReadData  ; Head
    call Floppy.ReadData  ; Sector
    call Floppy.ReadData  ; Sector size
    
    ; Copy data from kernel DMA buffer to user buffer
    pop ecx                 ; Restore sector count
    pop ebx                 ; Restore LBA
    
    ; Calculate bytes to copy
    mov eax, ecx
    shl eax, 9              ; *512
    
    ; Source = kernel DMA buffer, Destination = user buffer
    mov edi, esi            ; User buffer
    mov esi, [Floppy.DMABufferVirt]  ; Kernel DMA buffer
    
    ; Copy data (using rep movsd for speed)
    mov ecx, eax
    shr ecx, 2              ; Divide by 4 for DWORD copy
    rep movsd
    
    ; Handle remaining bytes (if any)
    test eax, 3
    jz @f
    mov ecx, eax
    and ecx, 3
    rep movsb
@@:
    
    ; Update pointers and counters
    mov esi, edi            ; Update user buffer pointer
    add ebx, [esp + 8]      ; Update LBA
    sub [esp + 12], [esp + 8] ; Update remaining count
    
    ; Check if done
    mov edi, [esp + 12]     ; Remaining sectors
    test edi, edi
    jnz .read_loop
    
.done:
    stdcall Mutex.Release, Floppy.Mutex
    ret
endp

proc Floppy.WriteSectors uses esi edi ebx, drive, lba, count, buffer
    ; Validate parameters
    cmp [count], 0
    je .done
    
    stdcall Mutex.Wait, Floppy.Mutex
    
    ; Turn motor on
    stdcall Floppy.MotorOn, [drive]
    
    mov esi, [buffer]      ; User buffer pointer
    mov ebx, [lba]         ; Current LBA
    mov edi, [count]       ; Remaining sectors
    
.write_loop:
    ; Calculate how many sectors we can transfer in this operation
    mov eax, ebx
    xor edx, edx
    mov ecx, SECTORS_PER_TRACK
    div ecx
    
    mov ecx, SECTORS_PER_TRACK
    sub ecx, edx
    
    cmp ecx, edi
    jbe @f
    mov ecx, edi
@@:
    
    cmp ecx, MAX_SECTORS_PER_OP
    jbe @f
    mov ecx, MAX_SECTORS_PER_OP
@@:
    
    push ebx              ; Save current LBA
    push ecx              ; Save sector count
    
    ; Copy data from user buffer to kernel DMA buffer
    mov eax, ecx
    shl eax, 9              ; *512
    
    ; Source = user buffer, Destination = kernel DMA buffer
    mov edi, [Floppy.DMABufferVirt]  ; Kernel DMA buffer
    mov ecx, eax
    shr ecx, 2              ; Divide by 4 for DWORD copy
    rep movsd
    
    ; Handle remaining bytes
    test eax, 3
    jz @f
    mov ecx, eax
    and ecx, 3
    rep movsb
@@:
    
    ; Setup DMA for write operation
    pop ecx                 ; Restore sector count
    push ecx                ; Save again
    stdcall Floppy.PrepareDMA, ecx, 1  ; 1 = write
    
    ; Convert LBA to CHS for first sector
    pop ecx                 ; Restore sector count
    pop ebx                 ; Restore LBA
    push ebx                ; Save LBA
    push ecx                ; Save sector count
    
    mov eax, ebx
    call Floppy.LBA2CHS
    
    ; Send write command
    mov al, CMD_WRITE
    call Floppy.WriteData
    
    ; Send parameters
    mov al, dh      ; Head << 2 | Drive
    shl al, 2
    or al, [drive]
    call Floppy.WriteData
    
    mov al, ch      ; Cylinder
    call Floppy.WriteData
    
    mov al, dh      ; Head
    call Floppy.WriteData
    
    mov al, cl      ; Sector
    call Floppy.WriteData
    
    mov al, 2       ; Sector size (512 bytes)
    call Floppy.WriteData
    
    mov al, SECTORS_PER_TRACK  ; Sectors per track
    call Floppy.WriteData
    
    mov al, 0x1B    ; GAP length
    call Floppy.WriteData
    
    mov al, 0xFF    ; Data length
    call Floppy.WriteData
    
    ; Wait for operation to complete
    stdcall Floppy.WaitForIRQ
    
    ; Read result bytes
    call Floppy.ReadData  ; ST0
    call Floppy.ReadData  ; ST1
    call Floppy.ReadData  ; ST2
    call Floppy.ReadData  ; Cylinder
    call Floppy.ReadData  ; Head
    call Floppy.ReadData  ; Sector
    call Floppy.ReadData  ; Sector size
    
    ; Update pointers and counters
    pop ecx                 ; Restore sector count
    pop ebx                 ; Restore LBA
    
    add ebx, ecx            ; Update LBA
    sub edi, ecx            ; Update remaining count
    
    ; Check if done
    test edi, edi
    jnz .write_loop
    
.done:
    stdcall Mutex.Release, Floppy.Mutex
    ret
endp

proc Floppy.Seek, drive, cylinder, head
    ; Send seek command
    mov al, CMD_SEEK
    call Floppy.WriteData
    
    ; Send parameters
    mov al, [head]
    shl al, 2
    or al, [drive]
    call Floppy.WriteData
    
    mov al, [cylinder]
    call Floppy.WriteData
    
    ; Wait for seek to complete
    stdcall Floppy.WaitForIRQ
    
    ; Read result
    call Floppy.ReadData
    call Floppy.ReadData
    
    ret
endp

proc Floppy.Recalibrate, drive:BYTE
    ; Send recalibrate command
    mov al, CMD_RECALIBRATE
    call Floppy.WriteData
    
    ; Send drive number
    mov al, [drive]
    call Floppy.WriteData
    
    ; Wait for recalibration
    stdcall Floppy.WaitForIRQ
    
    ; Read result
    call Floppy.ReadData
    call Floppy.ReadData
    
    ret
endp

proc Floppy.PrepareDMA uses ebx, sectors, mode
    ; Disable channel 2
    mov al, 0x06
    out DMA1_MASK_REG, al
    
    ; Clear flip-flop
    mov al, 0xFF
    out DMA1_CLEAR_FF, al
    
    ; Set mode
    mov al, [mode]
    cmp al, 0
    jne .write_mode
    mov al, DMA_MODE_READ
    jmp .set_mode
.write_mode:
    mov al, DMA_MODE_WRITE
.set_mode:
    out DMA1_MODE_REG, al
    
    ; Calculate transfer size (sectors * 512)
    mov eax, [sectors]
    shl eax, 9              ; *512
    
    ; DMA count register expects (bytes - 1)
    dec eax
    
    ; Set count
    out DMA1_CHAN2_COUNT, al
    shr eax, 8
    out DMA1_CHAN2_COUNT, al
    
    ; Get physical address of DMA buffer
    mov eax, [Floppy.DMABufferPhys]
    shl eax, 12             ; Convert page number to physical address
    
    ; Verify address is below 16MB for DMA
    cmp eax, DMA_MAX_ADDR
    jb .address_ok
    
    ; Address too high - this shouldn't happen if SetupDMABuffer worked
    ; Fall back to physical address 0x100000 (1MB)
    mov eax, 0x100000
    
.address_ok:
    ; Set address (low 16 bits)
    out DMA1_CHAN2_ADDR, al
    shr eax, 8
    out DMA1_CHAN2_ADDR, al
    
    ; Set page (high 8 bits)
    shr eax, 8
    out DMA2_PAGE_REG, al
    
    ; Enable channel 2
    mov al, 0x02
    out DMA1_MASK_REG, al
    
    ret
endp

proc Floppy.WaitForIRQ
    ; Wait for IRQ6 (floppy interrupt)
    stdcall HardwInt.WhaitForInt, 6
    ret
endp

proc Floppy.WaitForData
    ; Wait until FDC is ready for data transfer
    mov ecx, 100000  ; Timeout counter
    
.wait_loop:
    in al, FLOPPY_MSR
    test al, MSR_DRQ
    jnz .ready
    loop .wait_loop
    
    ; Timeout error
    mov eax, -1
    ret
    
.ready:
    xor eax, eax
    ret
endp

proc Floppy.WriteData
    ; Write byte to FDC FIFO
    push ecx
    mov ecx, 100000
    
.write_wait:
    mov dx, FLOPPY_MSR
    in al, dx
    test al, MSR_BUSY
    jz .write_ready
    loop .write_wait
    
.write_ready:
    mov al, [esp + 8]  ; Get parameter
    mov dx, FLOPPY_FIFO
    out dx, al
    pop ecx
    ret
endp

proc Floppy.ReadData
    ; Read byte from FDC FIFO
    push ecx
    mov ecx, 100000
    
.read_wait:
    mov dx, FLOPPY_MSR
    in al, dx
    test al, MSR_BUSY
    jz .read_ready
    loop .read_wait
    
.read_ready:
    mov dx, FLOPPY_FIFO
    in al, dx
    pop ecx
    ret
endp

proc Floppy.GetStatus
    ; Get floppy controller status
    mov dx, FLOPPY_MSR
    in al, dx
    ret
endp

proc Floppy.DetectDrives
    ; Try to detect floppy drives
    ; Returns: AL = number of drives detected
    
    ; Try to recalibrate drive 0
    stdcall Floppy.Recalibrate, 0
    
    ; Check if successful
    call Floppy.ReadData
    test al, 0xC0  ; Check error bits
    jnz .no_drive0
    
    ; Drive 0 exists
    mov byte [Floppy.DriveCount], 1
    
    ; Try drive 1
    stdcall Floppy.Recalibrate, 1
    call Floppy.ReadData
    test al, 0xC0
    jnz .done
    
    ; Drive 1 exists
    inc byte [Floppy.DriveCount]
    
.done:
    mov al, [Floppy.DriveCount]
    ret
    
.no_drive0:
    mov byte [Floppy.DriveCount], 0
    ret
endp

proc Floppy.Cleanup
    ; Unmap kernel DMA buffer
    stdcall Pager.Unmap, KERNEL_DMA_BUFFER shr 12
    
    ; Free physical page if we allocated it (not using fallback)
    cmp dword [Floppy.DMABufferPhys], 0x100
    je @f  ; Skip if using fallback address
    
    mov eax, [Floppy.DMABufferPhys]
    stdcall FramePool.FreePage, eax
@@:
    mov dword [Floppy.DMABufferPhys], 0
    mov dword [Floppy.DMABufferVirt], 0
    
    ret
endp

; Utility function to get DMA buffer info (for debugging)
proc Floppy.GetBufferInfo
    mov eax, [Floppy.DMABufferPhys]
    shl eax, 12
    mov edx, [Floppy.DMABufferVirt]
    ret
endp
}

block(.data) {
    Floppy.Mutex        db ?
    Floppy.DriveCount   db 0
    Floppy.CurrentCyl   db 0
    Floppy.CurrentHead  db 0
    Floppy.CurrentSect  db 0
    Floppy.MotorStatus  db 0
    Floppy.MotorTimer   dd 0
}