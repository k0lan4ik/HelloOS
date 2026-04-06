    block(.consts){
        PMM.BitMap equ 0xFFB10000
        PMM.BitMap.End equ 0xFFB30000
        
        PMM.Page.Window equ 0xFFB01000

        PMM.E820 equ Options.Kernel.Hier.IDT_GDT_E820 + 256 * 8
        E820_TYPE_FREE_RAM equ 1

        virtual at 0
    E820Entry: 
        .BaseLow    dd ?    ; Базовый адрес (0-31 биты)
        .BaseHigh   dd ?    ; Базовый адрес (32-63 биты)
        .LengthLow  dd ?    ; Длина региона (0-31 биты)
        .LengthHigh dd ?    ; Длина региона (32-63 биты)
        .Type       dd ?   ; Тип региона (1 = Free RAM, 2 = Reserved, и т.д.)
        .ACPI       dd ?   ; Расширенные атрибуты (бит 0 = "Ignore", если 0)
        end virtual

        virtual at 0 
    PageStack:
        .Next dd ?
        .Prev dd ?
        end virtual

        virtual at 0 
    PMM.Zone:
        .LowPhys    dd ?           ; Нижняя граница зоны (физическая)
        .HighPhys   dd ?           ; Верхняя граница зоны (физическая)
        .FreeLists  dd 11 dup(0)   ; Buddy-списки этой зоны
        .Size:
        end virtual

        PMM.ZONE_DMA    equ 0
        PMM.ZONE_NORMAL equ 1
        PMM.ZONES_COUNT equ 2

        AL_FL_WRITABLE  equ 0x01
        AL_FL_USERACC   equ 0x02
        AL_FL_NOEXEC    equ 0x04
        AL_FL_GLOBAL    equ 0x08
        AL_FL_PAT       equ 0x10
        AL_FL_PCD       equ 0x20
        AL_FL_PWT       equ 0x40

        X86_PTE_PRESENT  equ 0x001
        X86_PTE_WRITABLE equ 0x002
        X86_PTE_USER     equ 0x004
        X86_PTE_PWT      equ 0x008
        X86_PTE_PCD      equ 0x010
        X86_PTE_ACCESSED equ 0x020
        X86_PTE_DIRTY    equ 0x040
        X86_PTE_PAT      equ 0x080
        X86_PTE_GLOBAL   equ 0x100

        PML1BASE        equ 0xFFC00000
        PML2BASE        equ 0xFFFFF000
        PAGE_SHIFT      equ 10
        PAGE_MASK       equ 0xFFFFF000

        PMM.MAX_ORDER equ (1 shl 10)

    }

    block(.text){


    ; @[proc]
    ; .parent:   PMM
    ; .name:     PMM.Init
    ; .desc:     Инициализация физического менеджера памяти.
    ;            1. Отображает Bitmap в виртуальное пространство.
    ;            2. Размечает Bitmap по данным E820 (свободно = 0, занято = 1).
    ;            3. Защищает регион ядра и сам Bitmap.
    ;            4. Строит начальные Buddy-списки.
    ; .in:       PMM.E820 -> адрес таблицы.
    ; .out:      eax -> 0 при успехе.
    proc PMM.Init uses ebx esi edi
        pushf
        cld

        mov edi, 0xFFC00000 + ((PMM.BitMap shr 22) * 1000h)
        stdcall PMM.UnsaveMap, edi, PageTable3, AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC

        xor eax, eax
        mov ecx, 1000h / 4
        rep stosd 

        mov esi, PMM.BitMap
        mov edi, BitMapPage
    .MapPages:
        stdcall PMM.UnsaveMap, esi, edi, AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
        mov     eax, 1000h
        add     esi, eax
        add     edi, eax
        cmp     esi, PMM.BitMap.End
        jb      .MapPages
        push    edi

        mov     ecx, (PMM.BitMap.End - PMM.BitMap) / 4
        mov     eax, -1
        mov     edi, PMM.BitMap 
        push    edi
        push    edi
        rep stosd 

        mov edi, PMM.Zones 
        ; Зона 0 (DMA): 0 - 16МБ
        mov [edi + PMM.Zone.LowPhys], 0
        mov [edi + PMM.Zone.HighPhys], 0x01000000
        
        ; Зона 1 (Normal): 16МБ - до конца битмапа
        add edi, PMM.Zone.Size
        mov [edi + PMM.Zone.LowPhys], 0x01000000
        mov [edi + PMM.Zone.HighPhys], 0xFFFFFFFF

        pop     edi

        mov     esi, PMM.E820 + 4
        mov     ecx, [esi - 4]
        test    ecx, ecx
        jz      .E820End
    .E820:
        push    ecx
        
        cmp     dword [esi + E820Entry.Type], E820_TYPE_FREE_RAM
        jne     .NextEntry

        cmp     dword [esi + E820Entry.BaseHigh], 0    
        jne     .NextEntry

        test    byte [esi + E820Entry.ACPI], 1
        jz      .NextEntry

        mov     eax, [esi + E820Entry.BaseLow]
        mov     ecx, [esi + E820Entry.LengthLow]

        test    ecx, ecx
        jz      .NextEntry

        shr     eax, 12
        shr     ecx, 12
        jz      .NextEntry

    .ClearBits:
        btr     [edi], eax
        inc     eax
        loop    .ClearBits

    .NextEntry:
        add     esi, 24
        pop     ecx
        loop    .E820
    .E820End:

        pop     edi ecx
        shr     ecx, 12
    .ProtectKernel:
        bts [edi], ecx
        loop .ProtectKernel
        bts [edi], ecx
    .EndProc:
        stdcall PMM.InitLists
        xor eax, eax
        popf
        ret  
    endp  


    ; @[proc]
    ; .parent:  PMM
    ; .name:    PMM.UnsaveMap
    ; .desc:    Тупой маппинг страницы для первых этампов 
    ;           Флаги страниц:
    ;           | AL_FL_WRITABLE | 0x01 | можно ли писать                                                   |
    ;           | AL_FL_USERACC  | 0x02 | имеет доступ в user mode                                          |
    ;           | AL_FL_NOEXEC   | 0x04 | запрет на выполнение кода                                         |
    ;           | AL_FL_GLOBAL   | 0x08 | надо ли обновлять при обновлении контекста                        |
    ;           | AL_FL_PAT      | 0x10 | это "продвинутый" режим управления памятью                        |
    ;           | AL_FL_PCD      | 0x20 | полное отключение кэширования для этой страницы                   |
    ;           | AL_FL_PWT      | 0x40 | данные пишутся одновременно и в кэш, и сразу в оперативную память | 
    ; .in:      todovirt -> start of mapped file bytes
    ;           todophys -> byte count of mapped region
    ;           flags    -> флаги страницы (см в описании)
    ; .out:     eax      -> виртуальный адрес при успехе, -1 при неудаче 
    proc PMM.UnsaveMap uses ebx, todovirt, todophys, flags:DWORD
        
        mov     ebx, [todovirt]
        shr     ebx, 12
        shr     ebx, PAGE_SHIFT
        
        push    ebx
        shl     ebx, 2
        add     ebx, PML2BASE
        test byte [ebx], 1
        pop     ebx
        jz      .Err
    
        mov     ebx, [todovirt]
        shr     ebx, 12
        
    ;    push      ebx
    ;    shl       ebx, 2
    ;    add       ebx, PML1BASE
    ;    test byte [ebx], 1
    ;    pop       ebx
    ;    jnz       .Err
    
        mov     eax, [todophys]
        and     eax, PAGE_MASK
        mov     edx, [flags]

        or      eax, X86_PTE_PRESENT 
        test    edx, AL_FL_WRITABLE
        jz      @F
        or      eax, X86_PTE_WRITABLE
    @@:
        test    edx, AL_FL_USERACC
        jz      @F
        or      eax, X86_PTE_USER
    @@:
        test    edx, AL_FL_PWT
        jz      @F
        or      eax, X86_PTE_PWT
    @@:
        test    edx, AL_FL_PCD
        jz      @F
        or      eax, X86_PTE_PCD
    @@:
        test    edx, AL_FL_PAT
        jz      @F
        or      eax, X86_PTE_PAT
    @@:
        test    edx, AL_FL_GLOBAL
        jz      @F
        or      eax, X86_PTE_GLOBAL
    @@:
        test    edx, AL_FL_NOEXEC
        jz      @F
    @@:
        
        shl     ebx, 2
        add     ebx, PML1BASE
        mov     [ebx], eax
        mov     eax, [todovirt]
        invlpg  [eax]
        jmp     .EndProc 
    .Err:
        mov     eax, -1
    .EndProc:
        ret    
    endp


    ; @[proc]
    ; .parent:   PMM
    ; .name:     PMM.InitLists
    ; .desc:     Глобальный сканер физической памяти. 
    ;            Разбивает всю память на блоки по 4 МБ (MAX_ORDER) и запускает 
    ;            рекурсивный разбор каждого блока для наполнения Buddy-списков.
    ; .in:       void
    ; .out:      void
    ; .note:     Вызывается в самом конце PMM.Init, когда битмап уже готов.
    proc PMM.InitLists uses ebx esi edi
        xor     esi, esi
        mov     ecx, ((PMM.BitMap.End - PMM.BitMap) shl 3)

    .MainLoop:
        push    ecx

        stdcall PMM.CheckIsAdd, 10, esi

        pop ecx
        add esi, 1024           ; Прыгаем на 4МБ (Buddy Max Order)
        cmp esi, ecx
        jb .MainLoop
    .EndProc:    
        ret
    endp 


    ; @[proc]
    ; .parent:   PMM
    ; .name:     PMM.CheckIsAdd
    ; .desc:     Рекурсивная проверка региона памяти для инициализации Buddy-списков.
    ;            Сканирует битмап "сверху вниз", начиная с максимального порядка (4 МБ).
    ;            - Если блок (32 dword) полностью свободен (0), вызывает PushFree.
    ;            - Если блок полностью занят (0xFF), игнорирует его.
    ;            - Если блок "смешанный", рекурсивно делит его пополам до уровня бит.
    ; .in:       order    -> текущий порядок блока (0..10)
    ;            bitIndex -> индекс первой страницы в проверяемом блоке
    ; .out:      eax      -> void
    proc PMM.CheckIsAdd uses ebx edi, order, bitIndex
        mov     edx, 1
        mov     ecx, [order]
        shl     edx, cl
        
        cmp     ecx, 5
        jl      .BitLevel

        shr     edx, 5
        mov     ebx, [bitIndex]
        shr     ebx, 5
        lea     edi, [PMM.BitMap + ebx*4]
        
        mov     ecx, edx
        xor     eax, eax
        push    edi
        repe scasd
        pop     edi
        je      .AddBlock

        mov     ecx, edx
        or      eax, -1             
        repe scasd
        je      .EndProc

    .Split:
        mov     eax, [order]
        dec     eax
        push    eax                    

        stdcall PMM.CheckIsAdd, eax, [bitIndex]
        
        pop     ecx
        mov     ebx, 1
        shl     ebx, cl
        add     ebx, [bitIndex]
        stdcall PMM.CheckIsAdd, ecx, ebx
        jmp .EndProc

    .AddBlock:
        mov     eax, [bitIndex]
        shl     eax, 12
        stdcall PMM.PushFree, eax, [order]
        jmp     .EndProc

    .BitLevel: 
        mov     eax, 1
        shl     eax, cl
        xchg    eax, ecx
        push    eax
        mov     eax, [bitIndex]
        xor     edx, edx
    .LoopBit:
        bt      [PMM.BitMap], eax
        adc     edx, 0
        inc     eax
        loop    .LoopBit
        pop     ecx
        test    edx, edx
        jz      .AddBlock
        shr     edx, cl
        test    edx, edx
        jz     .Split

    .EndProc:    
        ret
    endp 


    ; @[proc]
    ; .parent:   PMM
    ; .name:     PMM.PushFree
    ; .desc:     Поиск структуры зоны по физическому адресу
    ; .in:       eax -> физический адрес
    ; .out:      eax -> указатель на PMM.Zone (или 0)
    proc PMM.GetZoneByAddr uses edx ecx, phys_addr
        mov     eax, [phys_addr]
        mov     edx, PMM.Zones
        mov     ecx, PMM.ZONES_COUNT
    .ZoneLoop:
        cmp     eax, [edx + PMM.Zone.HighPhys]
        jae     .Skip
        cmp     eax, [edx + PMM.Zone.LowPhys]
        jae     .Found 
    .Skip:
        add     edx, PMM.Zone.Size
        loop    .ZoneLoop
        xor     eax, eax
        jmp     .EndProc
    .Found:
        mov     eax, edx
    .EndProc:
        ret
    endp


    ; @[proc]
    ; .parent:   PMM
    ; .name:     PMM.PushFree
    ; .desc:     Добавление блока в начало двусвязного списка свободных страниц.
    ; .in:       pageIndex -> физический адрес начала блока
    ;            pageOrder -> порядок (список), в который нужно поместить блок
    ; .out:      eax       -> void (eax = -1 при ошибке маппинга)
    ; .note:     Использует PMM.Page.Window для записи указателей в физ. память.
    ;            ВНИМАНИЕ: затирает текущий маппинг в окне при обращении к следующему элементу.
    proc PMM.PushFree uses ebx edi, pageIndex, pageOrder
        stdcall PMM.GetZoneByAddr, [pageIndex]
        test    eax, eax
        jz      .EndProc       
        xchg    edi, eax
        lea     edi, [edi + PMM.Zone.FreeLists]

        stdcall PMM.UnsaveMap, PMM.Page.Window, [pageIndex], AL_FL_WRITABLE
        cmp     eax, -1
        je      .EndProc    

        mov     ebx, [pageOrder]
        mov     eax, [edi + ebx*4]
        mov     edx, [pageIndex]
        mov     [edi + ebx*4], edx
        mov     [PMM.Page.Window + PageStack.Next], eax
        mov     [PMM.Page.Window + PageStack.Prev], 0
        test    eax, eax
        jz      .EndProc

        stdcall PMM.UnsaveMap, PMM.Page.Window, eax, AL_FL_WRITABLE
        cmp     eax, -1
        je      .EndProc
        mov     eax, [pageIndex]
        mov     [PMM.Page.Window + PageStack.Prev], eax
    
    .EndProc: 
    
        ret
    endp 


    ; @[proc]
    ; .parent:   PMM
    ; .name:     PMM.PopFree
    ; .desc:     Извлечение первого доступного блока из списка заданного порядка и заданной зоны.
    ; .in:       pageOrder -> порядок списка (0..10)
    ;            zone      -> указатель на структуру PMM.Zone
    ; .out:      eax       -> физический адрес блока или 0, если список пуст
    proc PMM.PopFree uses ebx edi esi, pageOrder, zone
        mov     esi, [zone]
        lea     esi, [esi + PMM.Zone.FreeLists]
        
        mov     ebx, [pageOrder]
        mov     eax, [esi + ebx*4]
        push    eax
        test    eax, eax 
        jz      .Err
        stdcall PMM.UnsaveMap, PMM.Page.Window, eax, AL_FL_WRITABLE
        cmp     eax, -1
        je      .Err
        
        mov     edx, [PMM.Page.Window + PageStack.Next]
        mov     [esi + ebx*4], edx
        
        stdcall PMM.UnsaveMap, PMM.Page.Window, edx, AL_FL_WRITABLE
        cmp     eax, -1
        je      .Err

        mov     [PMM.Page.Window + PageStack.Prev], 0
        pop     eax
        jmp     .EndProc

    .Err:
        pop     eax
        xor     eax, eax
    .EndProc:   
        ret
    endp 


    ; @[proc]
    ; .parent:   PMM
    ; .name:     PMM.PickFree
    ; .desc:     Извлечение конкретного блока из произвольного места двусвязного списка.
    ;            Используется при слиянии соседних блоков (Coalescing).
    ; .in:       pageIndex -> физический адрес блока, который нужно "выкусить"
    ;            pageOrder -> порядок списка, в котором находится блок
    ; .out:      eax       -> void
    ; .note:     Требует осторожности при маппинге: функция последовательно переключает
    ;            PMM.Page.Window между текущим, следующим и предыдущим элементами
    proc PMM.PickFree uses edi esi ebx, pageIndex, pageOrder
        stdcall PMM.GetZoneByAddr, [pageIndex]
        test    eax, eax
        jz      .EndProc
        xchg    ebx, eax
        lea     ebx, [ebx + PMM.Zone.FreeLists]

        stdcall PMM.UnsaveMap, PMM.Page.Window, [pageIndex], AL_FL_WRITABLE
        cmp     eax, -1
        je      .EndProc

        mov     esi, [PMM.Page.Window + PageStack.Next]
        mov     edi, [PMM.Page.Window + PageStack.Prev]

        test    esi, esi
        jz      @F  
        stdcall PMM.UnsaveMap, PMM.Page.Window, esi, AL_FL_WRITABLE 
        mov     [PMM.Page.Window + PageStack.Prev], edi
        cmp     eax, -1
        je      .EndProc

    @@:
        test    edi, edi
        jnz     @F
        
        mov     eax, [pageOrder]
        mov     edx, [pageIndex]
        cmp     [ebx + eax*4], edx
        jne     @F
        mov     [ebx + eax*4], esi
        jmp     .EndProc    
    @@:    
        stdcall PMM.UnsaveMap, PMM.Page.Window, edi, AL_FL_WRITABLE 
        mov     [PMM.Page.Window + PageStack.Next], esi
        cmp     eax, -1
        je      .EndProc

    .EndProc:   
        ret
    endp 


    ; @[proc]
    ; .parent:   PMM
    ; .name:     PMM.ZeroPage
    ; .desc:     Обнуление физического блока памяти заданного порядка.
    ;            Использует SSE (XMM) и Non-Temporal инструкции для скорости.
    ; .in:       pageIndex -> физический адрес начала блока
    ;            pageOrder -> порядок блока (0=4КБ, 1, 2...)
    ; .out:      void
    ; .note:     Требует, чтобы физический адрес был выровнен по 16 байт.
    proc PMM.ZeroPage uses edi pageIndex, pageOrder
        stdcall PMM.UnsaveMap, PMM.Page.Window, [pageIndex], AL_FL_WRITABLE
        
        pxor    xmm0, xmm0        
        mov     edi, [pageIndex]
        mov     eax, 4096 / 16     ; 256 итераций по 16 байт
        mov     ecx, [pageOrder]
        shl     eax, cl
    .ZerodLoop:
        movntps [edi], xmm0    
        add     edi, 16
        loop    .ZerodLoop
        ret
    endp


    ; @[proc]
    ; .parent:   PMM
    ; .name:     PMM.Alloc
    ; .desc:     Выделение блока физической памяти заданного порядка.
    ; .in:       order -> желаемый порядок (0 = 4КБ, 10 = 4МБ)
    ;            zoneId -> ID зоны, с которой начать поиск (напр. PMM.ZONE_NORMAL)
    ; .out:      eax   -> физический адрес блока или 0, если памяти нет
    proc PMM.Alloc uses ebx esi edi, order, zoneID
        SPIN_LOCK PMM.GlobalLock
        mov     esi, [zoneID]

    .LoopZones:
        
        mov     eax, PMM.Zone.Size
        imul    eax, esi
        add     eax, PMM.Zones
        xchg    edi, eax

        mov     ebx, [order]
        cmp     ebx, 11
        jae      .Failed    

    .FindInZone: 
        stdcall PMM.PopFree, ebx, edi
        test    eax, eax
        jnz     .Found
        
        inc     ebx
        cmp     ebx, 11        
        jb      .FindInZone

        test    esi, esi
        jz      .Failed         
        dec     esi
        jmp     .LoopZones

    .Found:
    .SplitLoop:
        cmp     ebx, [order]
        je      .MarkBitmap     

        dec     ebx             
        
        mov     edx, 1
        mov     ecx, ebx
        shl     edx, cl         
        shl     edx, 12         
        
        lea     edx, [eax + edx]
        
        push    eax 
        stdcall PMM.PushFree, edx, ebx
        pop     eax
        
        jmp     .SplitLoop
        
    .MarkBitmap:
        push    eax
        
        mov     ebx, eax
        shr     ebx, 12         
        
        mov     eax, 1
        mov     ecx, [order]
        shl     eax, cl       
        xchg    eax, ecx

    .MarkLoop:
        bts     [PMM.BitMap], ebx
        inc     ebx
        loop    .MarkLoop
        
        pop     eax
        jmp     .EndProc

    .Failed:
        xor     eax, eax
    .EndProc:
        SPIN_UNLOCK PMM.GlobalLock
        ret
    endp


    ; @[proc]
    ; .parent:   PMM
    ; .name:     PMM.Free
    ; .desc:     Освобождение блока и попытка слияния с соседями.
    ; .in:       pageIndex -> физический адрес
    ;            pageOrder -> порядок блока
    proc PMM.Free uses ebx esi edi, pageIndex, pageOrder
        SPIN_LOCK PMM.GlobalLock
        mov     ebx, [pageOrder]
        mov     esi, [pageIndex]

    .UnmarkBitmap:
        mov     edi, esi
        shr     edi, 12                 
        
        mov     eax, 1
        mov     ecx, ebx
        shl     eax, cl               
        xchg    eax, ecx

    .UnmarkLoop:
        btr     [PMM.BitMap], edi
        inc     edi
        loop    .UnmarkLoop
        
    .CoalesceLoop:    
        cmp     ebx, 10               
        je      .Done

        mov     eax, 1
        mov     ecx, ebx
        shl     eax, cl
        xchg    eax, ecx

        mov     edx, ecx
        shl     edx, 12                
        
        mov     eax, esi                
        xor     eax, edx                
        
        push    ecx
        mov     edi, eax
        shr     edi, 12                 
        
    .CheckBuddyLoop:
        bt      [PMM.BitMap], edi
        jc      .BuddyBusy              
        inc     edi
        loop    .CheckBuddyLoop
        pop     ecx
        
        
        push    edx eax
        stdcall PMM.PickFree, eax, ebx
        pop     eax edx
        

        cmp     esi, eax 
        jb      .NextLevel
        mov     esi, eax               

    .NextLevel:
        inc     ebx                     
        jmp     .CoalesceLoop           

    .BuddyBusy:
        pop     ecx
    .Done:
        
        stdcall PMM.PushFree, esi, ebx
        SPIN_UNLOCK PMM.GlobalLock
        ret
    endp

    }


    block(.initData){
        PMM.GlobalLock    dd 0
        PMM.Zones db (PMM.ZONES_COUNT * PMM.Zone.Size) dup (0)   
    }