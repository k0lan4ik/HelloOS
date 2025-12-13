block(.consts){
    
    FLOPPY_DOR    = 0x3F2   ; Digital Output Register - управление двигателями и сбросом
    FLOPPY_MSR    = 0x3F4   ; Main Status Register - чтение состояния контроллера
    FLOPPY_FIFO   = 0x3F5   ; Data FIFO - регистр данных для команд и результатов
    FLOPPY_CCR    = 0x3F7   ; Configuration Control Register - настройка скорости
    FLOPPY_DIR    = 0x3F7   ; Digital Input Register - чтение состояния переключателей
    
   
    CMD_SPECIFY         = 0x03  ; Настройка временных параметров
    CMD_WRITE_DATA      = 0xC5  ; Запись данных (MFM)
    CMD_READ_DATA       = 0x46  ; Чтение данных (MFM)
    CMD_RECALIBRATE     = 0x07  ; Калибровка (возврат головки к 0 дорожке)
    CMD_SENSE_INT       = 0x08  ; Определение состояния прерывания
    CMD_SEEK            = 0x0F  ; Поиск дорожки
    CMD_CONFIGURE       = 0x13  ; Настройка контроллера
    CMD_VERSION         = 0x10  ; Получение версии контроллера
    CMD_FORMAT_TRACK    = 0x4D  ; Форматирование дорожки
    
    ST0_IC_NORMAL       = 0x00  ; Нормальное завершение
    ST0_IC_ABNORMAL     = 0x40  ; Аварийное завершение
    ST0_IC_INVALID      = 0x80  ; Неверная команда
    ST0_SEEK_END        = 0x20  ; Поиск завершен
    ST0_EQUIPMENT_CHECK = 0x10  ; Ошибка оборудования
    ST0_NOT_READY       = 0x08  ; Дисковод не готов
    ST0_HEAD_ADDRESS    = 0x04  ; Номер головки
    ST0_UNIT_SELECT     = 0x03  ; Выбор устройства
    
    FLOPPY_NONE     = 0  ; Дисковод отсутствует
    FLOPPY_360KB    = 1  ; 360KB 5.25"
    FLOPPY_1_2MB    = 2  ; 1.2MB 5.25"
    FLOPPY_720KB    = 3  ; 720KB 3.5"
    FLOPPY_1_44MB   = 4  ; 1.44MB 3.5"
    FLOPPY_2_88MB   = 5  ; 2.88MB 3.5"

    FLOPPY_1_44MB_CYLINDERS  = 80   ; 80 цилиндров
    FLOPPY_1_44MB_HEADS      = 2    ; 2 головки
    FLOPPY_1_44MB_SECTORS    = 18   ; 18 секторов на дорожке
    BYTES_PER_SECTOR         = 512  ; 512 байт в секторе
    
    MOTOR_DELAY_MS       = 500   ; Время раскрутки мотора (мс)
    MOTOR_TIMEOUT_MS     = 3000  ; Таймаут выключения мотора (мс)
    SEEK_TIMEOUT_MS      = 5000  ; Таймаут операции поиска
    MAX_RETRIES          = 3     ; Максимальное количество повторов

    DMA_FLAG_READ        = 0x46  ; Чтение: одиночный режим, инкремент, auto-init off
    DMA_FLAG_WRITE       = 0x4A  ; Запись: одиночный режим, декремент, auto-init off
    
    FLOPPY_IRQ           = 6     ; Номер прерывания floppy
}

block(.structs){
; внутренний буфер для кэширования
virtual at 0
    FloppyBuffer.State      db ?    ; Состояние буфера (0=свободен, 1=чтение, 2=готов, 3=грязный)
    FloppyBuffer.Drive      db ?    ; Номер дисковода (0 или 1)
    FloppyBuffer.Head       db ?    ; Номер головки (0 или 1)
    FloppyBuffer.Track      db ?    ; Номер дорожки (0-79)
    FloppyBuffer.Sector     db ?    ; Стартовый сектор (1-18)
    FloppyBuffer.Count      db ?    ; Количество секторов в буфере
    FloppyBuffer.Timestamp  dd ?    ; Время последнего доступа (для LRU)
    FloppyBuffer.VirtAddr   dd ?    ; Виртуальный адрес данных
    FloppyBuffer.PhysAddr   dd ?    ; Физический адрес для DMA
    FloppyBufferSize:
end virtual

; запрос на операцию ввода-вывода
virtual at 0
    FloppyRequest.Type      db ?    ; Тип операции (0=чтение, 1=запись, 2=форматирование)
    FloppyRequest.Drive     db ?    ; Номер дисковода
    FloppyRequest.LBA       dd ?    ; LBA сектора
    FloppyRequest.Count     db ?    ; Количество секторов
    FloppyRequest.Res1      db ?
    FloppyRequest.Buffer    dd ?    ; Буфер пользователя
    FloppyRequest.Status    db ?    ; Статус выполнения (0=ожидание, 1=успех, 2=ошибка)
    FloppyRequest.ErrorCode db ?    ; Код ошибки
    FloppyRequest.ThreadID  dw ?    ; ID потока, ожидающего завершения
    FloppyRequest.Next      dd ?    ; Следующий запрос в очереди
    FloppyRequestSize:
end virtual

; информация о дисководе
virtual at 0
    FloppyDrive.Type        db ?    ; Тип дисковода (FLOPPY_1_44MB и т.д.)
    FloppyDrive.Cylinders   db ?    ; Количество цилиндров
    FloppyDrive.Heads       db ?    ; Количество головок
    FloppyDrive.Sectors     db ?    ; Секторов на дорожке
    FloppyDrive.MotorState  db ?    ; Состояние мотора (0=выкл, 1=вкл)
    FloppyDrive.MotorTimer  dd ?    ; Время выключения мотора
    FloppyDrive.CurTrack    db ?    ; Текущая позиция головки
    FloppyDrive.Calibrated  db ?    ; Флаг калибровки (0=нет, 1=да)
    FloppyDrive.Present     db ?    ; Присутствует ли дисковод
    FloppyDrive.Locked      db ?    ; Заблокирован ли (выполняется операция)
    FloppyDriveSize:
end virtual
}

block(.text){

proc Floppy.Init
    stdcall Mutex.Start, Floppy.GlobalMutex 
    stdcall Mutex.Start, Floppy.RequestMutex  
    stdcall Mutex.Start, Floppy.BufferMutex      
    

    mov     edi, Floppy.Drives
    mov     ecx, (FloppyDriveSize * 2 + FloppyBufferSize * 8 + 64) / 4
    xor     eax, eax
    rep stosd
    
    mov [Floppy.RequestQueue], 0
    mov [Floppy.RequestTail], 0
    
    stdcall Floppy.DetectDrives
    

    stdcall Floppy.ResetController
    
    stdcall HardwInt.RegInt, FLOPPY_IRQ
    
    stdcall Floppy.InitBuffers
    
    stdcall Process.Create

    push    eax
    stdcall Threads.Create, eax, Floppy.ServiceThread
    mov     [Floppy.ServiceThreadID], ax

    pop     eax 
    stdcall Threads.Create, eax, Floppy.MotorManagerThread
    mov     [Floppy.MotorThreadID], ax
    
    stdcall VGA.PutString, Str.FloppyInit
    
    mov     al, [Floppy.DriveCount]
    stdcall VGA.PrintDec, eax
    stdcall VGA.PutString, Str.DrivesFound
    
    ret
endp


proc Floppy.DetectDrives

    mov     al, 0x10          
    out     0x70, al
    in      al, 0x71          
    mov     bl, al            


    mov     al, bl
    shr     al, 4             
    and     al, 0x0F
    stdcall Floppy.DecodeDriveType
    
    
    test    al, al
    jz      .NoDrive0
    
    
    mov     [Floppy.Drive0 + FloppyDrive.Type], al
    mov     [Floppy.Drive0 + FloppyDrive.Present], 1

    mov     byte [Floppy.Drive0 + FloppyDrive.Cylinders], FLOPPY_1_44MB_CYLINDERS
    mov     byte [Floppy.Drive0 + FloppyDrive.Heads], FLOPPY_1_44MB_HEADS
    mov     byte [Floppy.Drive0 + FloppyDrive.Sectors], FLOPPY_1_44MB_SECTORS
    
    inc [Floppy.DriveCount]
    
.NoDrive0:
    mov     al, bl
    and     al, 0x0F          
    stdcall Floppy.DecodeDriveType
    
    test    al, al
    jz      .NoDrive1
    
    mov     [Floppy.Drive1 + FloppyDrive.Type], al
    mov     [Floppy.Drive1 + FloppyDrive.Present], 1

    mov     byte [Floppy.Drive1 + FloppyDrive.Cylinders], FLOPPY_1_44MB_CYLINDERS
    mov     byte [Floppy.Drive1 + FloppyDrive.Heads], FLOPPY_1_44MB_HEADS
    mov     byte [Floppy.Drive1 + FloppyDrive.Sectors], FLOPPY_1_44MB_SECTORS
    
    inc [Floppy.DriveCount]
    
.NoDrive1:
    cmp [Floppy.DriveCount], 0
    jne .Done
    
    stdcall VGA.PutString, Str.NoDrives
    
.Done:
    ret
endp


proc Floppy.DecodeDriveType
    cmp     al, 1
    je      .Type360Kb
    cmp     al, 2
    je      .Type1.2Mb
    cmp     al, 3
    je      .Type720Kb
    cmp     al, 4
    je      .Type1.44Mb
    cmp     al, 5
    je      .Type2.88Mb
    cmp     al, 6
    je      .TypeUnknown
    
    mov     al, FLOPPY_1_44MB
    ret
    
.Type360Kb:
    mov     al, FLOPPY_360KB
    ret
.Type1.2Mb:
    mov     al, FLOPPY_1_2MB
    ret
.Type720Kb:
    mov     al, FLOPPY_720KB
    ret
.Type1.44Mb:
    mov     al, FLOPPY_1_44MB
    ret
.Type2.88Mb:
    mov     al, FLOPPY_2_88MB
    ret
.TypeUnknown:
    xor     al, al  
    ret
endp

proc Floppy.ResetController
    
    mov     al, 0x00
    mov     dx, FLOPPY_DOR
    out     dx, al
    mov     [Floppy.DOR_State], al
    
    stdcall Timer.Sleep, 10  ; 10 мс
    
    mov     al, 0x0C            
    mov     dx, FLOPPY_DOR
    out     dx, al
    mov     [Floppy.DOR_State], al

    stdcall Floppy.WaitForMSR
    jc      .Timeout
    
    mov     ecx, 4
.SenseLoop:
    mov     al, CMD_SENSE_INT
    mov     dx, FLOPPY_FIFO
    out     dx, al
    stdcall Floppy.WaitForFIFO
    in      al, dx      
    in      al, dx      
    loop    .SenseLoop
    
    mov     al, 0x03 
    mov     dx, FLOPPY_CCR        
    out     dx, al

   
    mov     al, CMD_CONFIGURE
    mov     dx, FLOPPY_FIFO
    out     dx, al
    mov     al, 0x00           
    out     dx, al
    mov     al, 0x00            
    out     dx, al
    mov     al, 0x00            
    out     dx, al

    mov     al, CMD_SPECIFY
    out     dx, al
    mov     al, 0xDF            
    out     dx, al
    mov     al, 0x02            
    out     dx, al

    
    mov     byte [Floppy.Drive0 + FloppyDrive.Calibrated], 0
    mov     byte [Floppy.Drive0 + FloppyDrive.CurTrack], 0xFF 
    mov     byte [Floppy.Drive0 + FloppyDrive.MotorState], 0

    mov     byte [Floppy.Drive1 + FloppyDrive.Calibrated], 0
    mov     byte [Floppy.Drive1 + FloppyDrive.CurTrack], 0xFF
    mov     byte [Floppy.Drive1 + FloppyDrive.MotorState], 0

    mov     byte [Floppy.LastError], 0

    jmp     .Success
    
.Timeout:
    mov     byte [Floppy.LastError], 0xFF
    
.Success:
    ret
endp


proc Floppy.InitBuffers uses ebx edi
    
    mov edi, Floppy.Buffers
    mov ecx, 8  
    
.InitLoop:
    mov     byte [edi + FloppyBuffer.State], 0      
    mov     dword [edi + FloppyBuffer.Timestamp], 0
    
    stdcall DMA.AllocateBuffer, 18432, 2, 0, FLOPPY_OWNER_ID
    test    eax, eax
    jz      .AllocationFailed
    
    
    mov     [edi + FloppyBuffer.VirtAddr], eax
    mov     [edi + FloppyBuffer.PhysAddr], edx
    
    add     edi, FloppyBufferSize
    loop    .InitLoop
    
    mov     [Floppy.BufferCount], 8
    mov     [Floppy.FreeBuffers], 8
    
    jmp     .Done
    
.AllocationFailed:
    stdcall VGA.PutString, Str.BufferAllocFailed
    
.Done:
    ret
endp


proc Floppy.Read uses ebx esi edi, drive: BYTE, lba: DWORD, count: BYTE, buffer: DWORD
    locals
        Drive       db ?
        Head        db ?
        Track       db ?
        Sector      db ?
        Count       db ?
        BufferIndex dw ?
    endl

    cmp     [count], 0
    je      .Invalid
    cmp     [count], 18
    ja      .Invalid
    
    movzx   ebx, [drive]
    imul    ebx, ebx, FloppyDriveSize
    add     ebx, Floppy.Drive0
    cmp     byte [ebx + FloppyDrive.Present], 0
    je      .NoDrive
    
    stdcall Floppy.LBAtoCHS, [lba], [drive] 
    jc      .ConversionError
    
    mov     [Drive], al
    mov     [Head], bl
    mov     [Track], cl
    mov     [Sector], dl
    
    stdcall Floppy.FindInCache, [Drive], [Head], [Track], [Sector]
    cmp     eax, -1
    jne     .CacheHit
    
    inc     [Floppy.CacheMisses]
    
    stdcall Floppy.GetFreeBuffer
    cmp     eax, -1
    je      .NoBuffer
    
    mov     [BufferIndex], ax
    
    stdcall Floppy.ReadSectors, [Drive], [Head], [Track], [Sector], [count], eax
    test    eax, eax
    jz      .ReadError
    
    mov     al, [Drive]
    mov     bl, [Head]
    mov     cl, [Track]
    mov     dl, [Sector]
    mov     si, [BufferIndex]
    stdcall Floppy.AddToCache
    
    stdcall Floppy.CopyFromBuffer, [BufferIndex], [buffer], [count]
    
    mov     eax, 1
    jmp     .Done
    
.CacheHit:
    
    inc     [Floppy.CacheHits]
    stdcall Floppy.CopyFromBuffer, eax, [buffer], [count]
    mov     eax, 1
    jmp     .Done
    
.Invalid:
    mov     byte [Floppy.LastError], 0x01
    jmp     .Error
.NoDrive:
    mov     byte [Floppy.LastError], 0x02
    jmp     .Error
.ConversionError:
    mov     byte [Floppy.LastError], 0x03
    jmp     .Error
.NoBuffer:
    mov     byte [Floppy.LastError], 0x04
    jmp     .Error
.ReadError:
    mov     eax, [BufferIndex]
    stdcall Floppy.ReleaseBuffer
    
.Error:
    xor eax, eax
    
.Done:
    ret
endp


; AL = drive, BL = head, CL = track, DL = sector
; CF = 1 при ошибке
proc Floppy.LBAtoCHS uses ebx, lba, nDrive:BYTE 
    locals
        Drive    db ?
        Cylinder db ?
        Head     db ?
        Sector   db ?
    endl

    mov     eax, [lba]
    movzx   ebx, [nDrive]
    imul    ebx, ebx, FloppyDriveSize
    add     ebx, Floppy.Drive0
    
    mov     cl, [ebx + FloppyDrive.Heads]
    mov     ch, [ebx + FloppyDrive.Sectors]
    
    movzx   edx, cl      
    movzx   ebx, ch      
    imul    edx, ebx      
    mov     ebx, [ebx + FloppyDrive.Cylinders]
    imul    edx, ebx      
    
    cmp     eax, edx
    jae     .OutOfRange

     
    movzx   ebx, ch        
    xor     edx, edx      
    div     ebx  
    inc     edx          
    
    mov     [Sector], dl
    
    xor     edx, edx
    movzx   ebx, ch 
    div     ebx
    mov     [Heads], dl
    mov     [Culinder], al
    
    cmp     [Cylinder], 79
    ja      .OutOfRange
    cmp     [Head], 1
    ja      .OutOfRange
    cmp     [Sector], 18
    ja      .OutOfRange
    
    mov     al, [Drive]
    mov     bl, [Head]
    mov     cl, [Cylinder]
    mov     dl, [Sector]
    
    clc
    jmp     .Done
    
.OutOfRange:
    stc
    
.Done:
    ret
endp


proc Floppy.ReadSectors uses ebx esi edi, drive: BYTE, head: BYTE, track: BYTE, \ 
                                   sector: BYTE, count: BYTE, bufferIndex: WORD
    movzx   eax, [bufferIndex]
    imul    esi, eax, FloppyBufferSize
    add     esi, Floppy.Buffers
    
    mov     al, [drive]
    stdcall Floppy.MotorOn, eax
    
    movzx   ebx, [drive]
    imul    ebx, ebx, FloppyDriveSize
    add     ebx, Floppy.Drive0
    
    cmp     byte [ebx + FloppyDrive.Calibrated], 0
    jne     .AlreadyCalibrated
    
    stdcall Floppy.Calibrate, [drive]
    test    al, al
    jz      .CalibrationFailed
    
.AlreadyCalibrated:
    
    stdcall Floppy.Seek, [drive], [track]
    jc      .SeekFailed
    
    mov     cx, [count]
    shl     cx, 9       

    stdcall Floppy.SetupDMA, [esi + FloppyBuffer.PhysAddr], ecx, DMA_FLAG_READ
    
    mov     al, CMD_READ_DATA
    mov     dx, FLOPPY_FIFO
    out     dx, al
    
    mov     al, [head]
    shl     al, 2
    or      al, [drive]
    out     dx, al  ; Номер головки и дисковода
    
    mov     al, [track]
    out     dx, al  ; Номер дорожки
    
    mov     al, [head]
    out     dx, al  ; Номер головки (с которой начинать)
    
    mov     al, [sector]
    out     dx, al  ; Стартовый сектор
    
    mov     al, 2            ; Размер сектора (2 = 512 байт)
    out     dx, al
    
    mov     al, [count]
    out     dx, al  ; Количество секторов
    
    mov     al, 0x1B         ; GAP3 длина
    out     dx, al
    
    mov     al, 0xFF         ; DTL (Data Length)
    out     dx, al
    
    stdcall Floppy.WaitForIRQ
    jc      .IrqTimeout
    
    mov ecx, 7
.ReadResults:
    stdcall Floppy.WaitForFIFO
    mov     dx, FLOPPY_FIFO
    in      al, dx
    mov     [.results + ecx - 1], al
    loop    .ReadResults
    
    mov     al, [.results]
    and     al, 0xC0
    cmp     al, ST0_IC_NORMAL
    je      .Success
    cmp     al, ST0_IC_ABNORMAL
    je      .AbnormalError
    cmp     al, ST0_IC_INVALID
    je      .InvalidCommand
    
    movzx   ebx, [drive]
    imul    ebx, ebx, FloppyDriveSize
    add     ebx, Floppy.Drive0
    mov     al, [track]
    mov     [ebx + FloppyDrive.CurTrack], al
    
    mov     al, [drive]
    mov     [esi + FloppyBuffer.Drive], al
    mov     al, [head]
    mov     [esi + FloppyBuffer.Head], al
    mov     al, [track]
    mov     [esi + FloppyBuffer.Track], al
    mov     al, [sector]
    mov     [esi + FloppyBuffer.Sector], al
    mov     al, [count]
    mov     [esi + FloppyBuffer.Count], al
    
    stdcall Timer.GetTimeMs
    mov     [esi + FloppyBuffer.Timestamp], eax
    
.Success:
    mov eax, 1
    jmp .Cleanup
    
.CalibrationFailed:
    mov byte [Floppy.LastError], 0x10
    jmp .Error
.SeekFailed:
    mov byte [Floppy.LastError], 0x11
    jmp .Error
.IrqTimeout:
    mov byte [Floppy.LastError], 0x12
    jmp .Error
.AbnormalError:
    mov byte [Floppy.LastError], 0x13
    jmp .Error
.InvalidCommand:
    mov byte [Floppy.LastError], 0x14
    
.Error:
    xor eax, eax
    
.Cleanup:
    ret
    
.results db 7 dup(0)
endp


proc Floppy.SetupDMA uses ebx, phys, size:WORD, mode:BYTE 
    
    mov     eax, [phys]
    mov     cx, [size]
    mov     bl, [mode]

   
    mov     al, 0x06          ; Маска канала 2
    out     0x0A, al

    
    mov     al, 0xFF
    out     0x0C, al          ; Запись любого значения сбрасывает триггер
    
    mov     dx, 0x04          ; Адресный регистр канала 2
    out     dx, al            ; Младший байт
    mov     al, ah
    out     dx, al            ; Старший байт


    shr     eax, 16           ; Получаем старший байт адреса
    mov     dx, 0x81          ; Регистр страницы канала 2
    out     dx, al

    
    mov     al, 0xFF
    out     0x0C, al

    mov     ax, cx
    dec     ax                ; DMA передает count+1 байт
    mov     dx, 0x05          ; Счетчик канала 2
    out     dx, al            ; Младший байт
    mov     al, ah
    out     dx, al            ; Старший байт

    mov     al, bl            ; Режим (чтение или запись)
    out     0x0B, al
    
    mov     al, 0x02
    out     0x0A, al

    mov     eax, [phys]
    stdcall DMA.FlushCache, eax, ecx
    
    ret
endp

proc Floppy.MotorOn uses ebx, drive:BYTE
    
    movzx   ebx, [drive]
    imul    ebx, ebx, FloppyDriveSize
    add     ebx, Floppy.Drive0
    
    cmp     byte [ebx + FloppyDrive.MotorState], 1
    je      .UpdateTimer
    
    mov     al, [Floppy.DOR_State]
    mov     cl, [drive]
    add     cl, 4              ; Бит мотора для дисковода 0 = бит 4, для 1 = бит 5
    mov     dl, 1
    shl     dl, cl
    or      al, dl
    or      al, 0x0C            ; Включение контроллера и DMA
    mov     [Floppy.DOR_State], al
    mov     dx, FLOPPY_DOR
    out     dx, al
    

    mov     byte [ebx + FloppyDrive.MotorState], 2
    
    stdcall Timer.Sleep, MOTOR_DELAY_MS
    
    mov byte [ebx + FloppyDrive.MotorState], 1
    
.UpdateTimer:
    stdcall Timer.GetTimeMs
    add     eax, MOTOR_TIMEOUT_MS
    mov     [ebx + FloppyDrive.MotorTimer], eax
    
    ret
endp

proc Floppy.Calibrate uses ebx, drive: BYTE
    
    mov     ecx, 3
    
.CalibrateLoop:
    push    ecx
    mov     al, CMD_RECALIBRATE
    mov     dx, FLOPPY_FIFO
    out     dx, al
    
    mov     al, [drive]
    out     dx, al
    
    stdcall Floppy.WaitForIRQ
    jc      .Timeout
    
    mov     al, CMD_SENSE_INT
    mov     dx, FLOPPY_FIFO
    out     dx, al
    stdcall Floppy.WaitForFIFO
    mov     dx, FLOPPY_FIFO
    in      al, dx    ; ST0
    in      al, dx    ; PCN (текущая дорожка)
    
    test    al, al
    jz      .AtTrack0
    
    pop     ecx
    loop    .CalibrateLoop
    jmp     .Failed
    
.AtTrack0:
    pop     ecx
    
    movzx   ebx, [drive]
    imul    ebx, ebx, FloppyDriveSize
    add     ebx, Floppy.Drive0
    
    mov     byte [ebx + FloppyDrive.CurTrack], 0
    mov     byte [ebx + FloppyDrive.Calibrated], 1
    
    mov     al, 1
    jmp     .Done
    
.Timeout:
    pop     ecx
.Failed:
    xor     al, al
    
.Done:
    ret
endp


proc Floppy.Seek uses ebx, drive:BYTE, track:BYTE

    movzx   ecx, [drive]
    imul    ecx, ecx, FloppyDriveSize
    add     ecx, Floppy.Drive0
    mov     al, [ecx + FloppyDrive.CurTrack]
    
    cmp     al, [track]
    je      .AlreadyThere
    
    mov     al, CMD_SEEK
    mov     dx, FLOPPY_FIFO
    out     dx, al
    
    mov     al, [drive]
    shl     al, 2          
    out     dx, al
    
    mov     al, [track]
    out     dx, al
    
    stdcall Floppy.WaitForIRQ
    jc      .Timeout
    
    mov     al, CMD_SENSE_INT
    mov     dx, FLOPPY_FIFO
    out     dx, al
    stdcall Floppy.WaitForFIFO
    mov     dx, FLOPPY_FIFO
    in      al, dx    ; ST0
    in      al, dx    ; PCN
    
    cmp     al, [track]
    jne     .WrongTrack
    
    movzx   ecx, [drive]
    imul    ecx, ecx, FloppyDriveSize
    add     ecx, Floppy.Drive0
    mov     al, [track]
    mov     [ecx + FloppyDrive.CurTrack], al
    
.AlreadyThere:
    clc
    jmp .Done
    
.Timeout:
.Wrong_track:
    stc
    
.Done:    
    ret
    
endp


proc Floppy.WaitForIRQ uses ebx
    
    stdcall HardwInt.WhaitForInt, FLOPPY_IRQ
    
    stdcall Timer.GetTimeMs
    mov ebx, eax
    
.WaitLoop:
    cmp     [Floppy.IRQReceived], 1
    jmp     .IRQReceived
    
    stdcall Timer.GetTimeMs
    sub     eax, ebx
    cmp     eax, 500
    jb      .WaitLoop
    
    stc
    jmp     .Done
    
.IRQReceived:
    mov [Floppy.IRQReceived], 0
    clc
    
.Done:
    ret
endp

proc Floppy.WaitForFIFO
    mov     ecx, 1000000  
    
    mov     dx, FLOPPY_MSR
.Wait:
    in      al, dx
    test    al, 0x80     ; Бит RQM (Request for Master) - контроллер готов
    jz      .NotReady
    clc
    jmp     .Done
    
.NotReady:
    loop    .Wait
    
    stc
    
.Done:
    ret
endp

proc Floppy.WaitForMSR    
    mov     ecx, 1000000

    mov     dx, FLOPPY_MSR 
.wait:
    in      al, dx
    test    al, 0x80     ; Бит RQM
    jnz     .Ready
    loop    .Wait
    
    stc
    jmp     .Done
    
.Ready:
    clc
    
.Done:
    ret
endp


proc Floppy.FindInCache uses esi, drive, head, track, sector

    stdcall Mutex.Wait, Floppy.BufferMutex
    
    mov     esi, Floppy.Buffers
    mov     ecx, 8  
    
.Search:
    cmp     byte [esi + FloppyBuffer.State], 2  ; BUFFER_READY
    jne     .Next
    
    mov     al, [drive]
    cmp     [esi + FloppyBuffer.Drive], al
    jne     .Next
    
    mov     al, [head]
    cmp     [esi + FloppyBuffer.Head], al
    jne     .Next
    
    mov     al, [track]
    cmp     [esi + FloppyBuffer.Track], cl
    jne     .Next
    
    mov     al, [esi + FloppyBuffer.Sector]
    cmp     al, [sector]
    jne     .Next
    
    
    stdcall Timer.GetTimeMs
    mov     [esi + FloppyBuffer.Timestamp], eax
    
    mov     eax, 8
    sub     eax, ecx  
    
    stdcall Mutex.Release, Floppy.BufferMutex
    jmp     .Found
    
.Next:
    add     esi, FloppyBufferSize
    loop    .Search
    
    mov     eax, -1
    
.Found:

    ret
endp


proc Floppy.GetFreeBuffer uses ebx esi
    
    stdcall Mutex.Wait, Floppy.BufferMutex
    
    mov esi, Floppy.Buffers
    mov ecx, 8
    
.FindFree:
    cmp     byte [esi + FloppyBuffer.State], 0  ; BUFFER_FREE
    je      .FoundFree
    
    add     esi, FloppyBufferSize
    loop    .FindFree
    
    stdcall Floppy.EvictLRUBuffer
    cmp     eax, -1
    je      .NoBuffer
    
    mov     esi, eax
    mov     ecx, FloppyBufferSize
    sub     eax, Floppy.Buffers
    xor     edx, edx
    div     ecx
    jmp     .MarkUsed
    
.FoundFree:
    mov     eax, 8
    sub     eax, ecx
    
.MarkUsed:
    mov     byte [esi + FloppyBuffer.State], 1  ; BUFFER_READING
    
    dec     [Floppy.FreeBuffers]
    
    stdcall Mutex.Release, Floppy.BufferMutex
    jmp     .Done
    
.NoBuffer:
    stdcall Mutex.Release, Floppy.BufferMutex
    mov     eax, -1
    
.Done:
    ret
endp

proc Floppy.EvictLRUBuffer uses ebx esi
    
    mov     esi, Floppy.Buffers
    mov     ecx, 8
    mov     ebx, 0xFFFFFFFF  ; Самое старое время
    mov     edx, 0           ; Указатель на самый старый буфер
    
.FindLRU:
    cmp     byte [esi + FloppyBuffer.State], 2  ; BUFFER_READY
    jne     .Next
    
    mov     eax, [esi + FloppyBuffer.Timestamp]
    cmp     eax, ebx
    jae     .Next
    
    mov     ebx, eax
    mov     edx, esi
    
.Next:
    add     esi, FloppyBufferSize
    loop    .FindLRU
    
    test    edx, edx
    jz      NotFound
    
    cmp     byte [edx + FloppyBuffer.State], 3  ; BUFFER_DIRTY
    jne     .CleanBuffer
    
    push    edx
    stdcall Floppy.WriteBackBuffer
    pop     edx
    test    al, al
    jz      .WriteFailed
    
.CleanBuffer:
    mov     byte [edx + FloppyBuffer.State], 0
    mov     eax, edx
    jmp     .Done
    
.WriteFailed:
.NotFound:
    mov     eax, -1
    
.Done:
    ret
endp

proc Floppy.CopyFromBuffer uses esi edi ecx, bufferIndex: WORD, userBuffer: DWORD, sectorCount: BYTE
   
    movzx   eax, [bufferIndex]
    imul    eax, eax, FloppyBufferSize
    add     eax, Floppy.Buffers
    mov     esi, [eax + FloppyBuffer.VirtAddr]
    
    mov     edi, [userBuffer]
    
    movzx   ecx, [sectorCount]
    shl     ecx, 9  
    
    rep movsb
    
    ret
endp

proc Floppy.ServiceThread
.MainLoop:
    stdcall Mutex.Wait, Floppy.RequestMutex
    
    cmp     [Floppy.RequestQueue], 0
    je      .NoRequests
    
    mov     esi, [Floppy.RequestQueue]
    mov     eax, [esi + FloppyRequest.Next]
    mov     [Floppy.RequestQueue], eax
    
    test    eax, eax
    jnz     .QueueNotEmpty
    mov     [Floppy.RequestTail], 0

.QueueNotEmpty:
    stdcall Mutex.Release, Floppy.RequestMutex
    
    cmp     byte [esi + FloppyRequest.Type], 0
    je      .ProcessRead
    cmp     byte [esi + FloppyRequest.Type], 1
    je      .ProcessWrite
    
    mov     byte [esi + FloppyRequest.Status], 2  ; Ошибка
    jmp     .CompleteRequest
    
.ProcessRead:
    stdcall Floppy.Read, \
                    [esi + FloppyRequest.Drive], \
                    [esi + FloppyRequest.LBA], \
                    [esi + FloppyRequest.Count], \
                    [esi + FloppyRequest.Buffer]
    test    al, al
    jnz     @F
    mov     [esi + FloppyRequest.Status], 1
    jmp     .CompleteRequest
@@:
    mov     [esi + FloppyRequest.Status], 2
    stdcall Floppy.GetError
    mov     [esi + FloppyRequest.ErrorCode], al 
    jmp     .CompleteRequest
.ProcessWrite:
    ; Обработка запроса на запись (упрощенно)
    mov     byte [esi + FloppyRequest.Status], 2  ; Пока не реализовано
    jmp     .CompleteRequest
    
.CompleteRequest:
    movzx   eax, [esi + FloppyRequest.ThreadID]
    stdcall Sched.Signal, eax
    
    stdcall KernelMemManager.Free, esi

    jmp     .MainLoop
    
.NoRequests:
    stdcall Mutex.Release, Floppy.RequestMutex
    
    stdcall Sched.Block
    
    jmp     .MainLoop
endp

proc Floppy.MotorManagerThread
.MainLoop:
    cmp     byte [Floppy.Drive0 + FloppyDrive.MotorState], 1
    jne     .CheckDrive1
    
    stdcall Timer.GetTimeMs
    cmp     eax, [Floppy.Drive0 + FloppyDrive.MotorTimer]
    jbe     .CheckDrive1
    
    mov     al, [Floppy.DOR_State]
    and     al, not (1 shl 4)     
    mov     [Floppy.DOR_State], al
    mov     dx, FLOPPY_DOR
    out     dx, al

    mov     byte [Floppy.Drive0 + FloppyDrive.MotorState], 0
    
.CheckDrive1:
    cmp     byte [Floppy.Drive1 + FloppyDrive.MotorState], 1
    jne     .Sleep
    
    stdcall Timer.GetTimeMs
    cmp     eax, [Floppy.Drive1 + FloppyDrive.MotorTimer]
    jbe     .Sleep
    
    mov     al, [Floppy.DOR_State]
    and     al, not (1 shl 5)      ; Сброс бита мотора 1
    mov     [Floppy.DOR_State], al
    mov     dx, FLOPPY_DOR
    out     dx, al

    mov     byte [Floppy.Drive1 + FloppyDrive.MotorState], 0
    
.Sleep:
    stdcall Timer.Sleep, 1000
    
    jmp .MainLoop
endp

proc Floppy.SubmitRequest uses ebx, request: DWORD
    mov     ebx, [request]
    
    test    ebx, ebx
    jz      .Invalid
    
    stdcall ProcessManager.GetCurrentThread
    mov     [ebx + FloppyRequest.ThreadID], ax
    
    mov     byte [ebx + FloppyRequest.Status], 0
    
    stdcall Mutex.Wait, Floppy.RequestMutex
    
    cmp     [Floppy.RequestQueue], 0
    jne     .AddToTail
    
    mov     [Floppy.RequestQueue], ebx
    mov     [Floppy.RequestTail], ebx
    jmp     .Added
    
.AddToTail:
    mov     eax, [Floppy.RequestTail]
    mov     [eax + FloppyRequest.Next], ebx
    mov     [Floppy.RequestTail], ebx
    
.Added:
    stdcall Mutex.Release, Floppy.RequestMutex
    
    movzx   eax, [Floppy.ServiceThreadID]
    stdcall Sched.Signal, eax
    
    mov     eax, 1
    ret
    
.Invalid:
    xor     eax, eax
    ret
endp

; ============================================
; Floppy.ReadAsync - Асинхронное чтение секторов
; Вход: drive, lba, count, buffer, callback (опционально)
; Выход: EAX = handle запроса или 0 при ошибке
; ============================================
proc Floppy.ReadAsync uses ebx, drive: BYTE, lba: DWORD, count: BYTE, buffer: DWORD, callback: DWORD
    stdcall KernelMemManager.Malloc, FloppyRequestSize
    test    eax, eax
    jz      .AllocFailed
    
    mov     ebx, eax
    
    mov     byte [ebx + FloppyRequest.Type], 0      ; READ
    mov     al, [drive]
    mov     [ebx + FloppyRequest.Drive], al
    
    mov     eax, [lba]
    mov     [ebx + FloppyRequest.LBA], eax

    mov     al, [count]
    mov     [ebx + FloppyRequest.Count], al
    mov     eax, [buffer]
    mov     [ebx + FloppyRequest.Buffer], eax
    
    stdcall Floppy.SubmitRequest, ebx
    test    eax, eax
    jz      .SubmitFailed
    
    mov eax, ebx
    ret
    
.SubmitFailed:
    stdcall KernelMemManager.Free, ebx
    
.AllocFailed:
    xor eax, eax
    ret
endp


proc Floppy.GetStatus
    xor     eax, eax
    
    ; Бит 0: драйвер инициализирован
    cmp     [Floppy.Initialized], 0
    je      .NotInitialized
    or      eax, 1
    
.NotInitialized:
    ; Бит 1: есть дисковод 0
    cmp     [Floppy.Drive0 + FloppyDrive.Present], 0
    je      .no_drive0
    or      eax, 2
    
.NoDrive0:
    ; Бит 2: есть дисковод 1
    cmp     [Floppy.Drive1 + FloppyDrive.Present], 0
    je      .no_drive1
    or      eax, 4
    
.NoDrive1:
    ; Бит 3: мотор 0 включен
    cmp     [Floppy.Drive0 + FloppyDrive.MotorState], 0
    je      .motor0_off
    or      eax, 8
    
.Motor0Off:
    ; Бит 4: мотор 1 включен
    cmp     [Floppy.Drive1 + FloppyDrive.MotorState], 0
    je      .motor1_off
    or      eax, 16
    
.Motor1Off:
    ret
endp


proc Floppy.GetError
    mov     al, [Floppy.LastError]
    ret
endp

proc Floppy.Reset
    
    movzx   eax, [Floppy.ServiceThreadID]
    stdcall Threads.Kill, eax
    
    movzx   eax, [Floppy.MotorThreadID]
    stdcall ProcessManager.GetProcess, eax
    push    eax eax
    movzx   eax, [Floppy.MotorThreadID]
    stdcall Threads.Kill, eax
    
    stdcall Floppy.ResetController
    
    pop     eax
    
    stdcall Threads.Create, eax, Floppy.ServiceThread
    mov     [Floppy.ServiceThreadID], ax
    
    pop     eax
    stdcall Threads.Create, eax, Floppy.MotorManagerThread
    mov     [Floppy.MotorThreadID], ax

    mov     [Floppy.CacheHits], 0
    mov     [Floppy.CacheMisses], 0
    mov     [Floppy.LastError], 0

    ret
endp

proc Floppy.Shutdown
    mov     al, [Floppy.DOR_State]
    and     al, not 0xF0          
    or      al, 0x0C            
    mov     [Floppy.DOR_State], al
    out     FLOPPY_DOR, al
    
    movzx   eax, [Floppy.ServiceThreadID]
    stdcall Threads.Kill, eax
    
    movzx   eax, [Floppy.MotorThreadID]
    stdcall ProcessManager.GetProcess, eax
    push    eax

    movzx eax, [Floppy.MotorThreadID]
    stdcall Threads.Kill, eax
    
    pop     eax
    stdcall Process.Kill, eax 
    
    mov     esi, Floppy.Buffers
    mov     ecx, 8
    
.FreeBuffers:
    mov     eax, [esi + FloppyBuffer.VirtAddr]
    test    eax, eax
    jz      .NextBuffer
    
    stdcall DMA.FreeBuffer, eax
    
.NextBuffer:
    add     esi, FloppyBuffer.Size
    loop    .FreeBuffers
    
    mov [Floppy.Initialized], 0
    
    ret
endp
}

block(.data){
    ; ============================================
    ; ГЛОБАЛЬНЫЕ ПЕРЕМЕННЫЕ ДРАЙВЕРА
    ; ============================================
    
    ; Мьютексы для синхронизации
    Floppy.GlobalMutex   db ?   ; Общий мьютекс драйвера
    Floppy.RequestMutex  db ?   ; Для очереди запросов
    Floppy.BufferMutex   db ?   ; Для управления буферами
    
    Floppy.Drives:
    ; Информация о дисководах
    Floppy.Drive0        db FloppyDriveSize dup (?)  ; Структура дисковода 0
    Floppy.Drive1        db FloppyDriveSize dup (?)  ; Структура дисковода 1
    Floppy.DriveCount    db ?                         ; Количество обнаруженных дисководов
    
    ; Очередь запросов
    Floppy.RequestQueue  dd ?   ; Указатель на первый запрос
    Floppy.RequestTail   dd ?   ; Указатель на последний запрос
    
    ; Буферы для кэширования (8 буферов по 18KB)
    Floppy.Buffers       db FloppyBufferSize * 8 dup (?)
    Floppy.BufferCount   dd ?   ; Количество буферов (всегда 8)
    Floppy.FreeBuffers   dd ?   ; Количество свободных буферов
    
    ; Состояние контроллера
    Floppy.DOR_State     db ?   ; Текущее значение регистра DOR
    Floppy.IRQReceived   db ?   ; Флаг получения прерывания
    
    ; ID сервисных потоков
    Floppy.ServiceThreadID  dw ?
    Floppy.MotorThreadID    dw ?
    
    ; Статистика и отладка
    Floppy.CacheHits     dd ?   ; Количество попаданий в кэш
    Floppy.CacheMisses   dd ?   ; Количество промахов кэша
    Floppy.LastError     db ?   ; Код последней ошибки
    Floppy.Initialized   db ?   ; Флаг инициализации (1 = инициализирован)
    
    ; Константа владельца для DMA Manager
    FLOPPY_OWNER_ID      = 0x464C4F50  ; 'FLOP' в ASCII
}

block(.initData){
    ; Начальные значения переменных
    Floppy.Initialized   db 0
    Floppy.DriveCount    db 0
    Floppy.IRQReceived   db 0
    Floppy.LastError     db 0
    
    ; Строки для вывода сообщений
    Str.FloppyInit       db "Floppy driver initialized", 13, 10, 0
    Str.DrivesFound      db " floppy drive(s) found", 13, 10, 0
    Str.NoDrives         db "No floppy drives detected", 13, 10, 0
    Str.BufferAllocFailed db "Failed to allocate floppy buffers", 13, 10, 0
}