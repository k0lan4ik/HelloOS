block(.consts)  { 
    virtual at 0
    Slab.Cache:
    .Lock           dd ?
    .NextCache      dd ?  
    .ObjSize        dd ?  
    ;.AlignShift     dd ?  
    
    ; Списки страниц
    .SlabsFull      dd ?  
    .SlabsPartial   dd ?  
    .SlabsEmpty     dd ?  
    
    .FreeList       dd ?  
    
    ; Статистика
    .TotalObjects   dd ? 
    .ActiveObjects  dd ?  
    .Name           db 24 dup (?)
    .Size: 
    end virtual
}

block(.text) {
; @[proc]
; .name: Slab.Init
; .desc: Подготовка инфраструктуры Slab-аллокатора.
proc Slab.Init uses edi esi

    mov     eax, Kernel.Executable.End
    add     eax, 0xFFF
    and     eax, 0xFFFFF000
    mov     [Slab.HeapPointer], eax


    mov     edi, Slab.InitialCache
    xor     eax, eax
    mov     ecx, Slab.Cache.Size / 4
    rep stosd
    
    mov edi, Slab.InitialCache

    mov [Slab.CacheList], edi 
    mov [edi + Slab.Cache.ObjSize], Slab.Cache.Size
    
    add edi, Slab.Cache.Name
    mov esi, Slab.Name.InitialCache
    mov ecx, Slab.Name.InitialCache.length + 1
    rep movsb
      
    ret
endp

; .in: ptrCache = указатель на дескриптор кэша (Slab.Cache)
; .out: eax = адрес выделенного объекта
proc Slab.Alloc uses ebx esi edi, ptrCache
    mov     esi, [ptrCache]
    mov     edi, esi
    add     edi, Slab.Cache.Lock
    
    SPIN_LOCK edi        

    mov     eax, [esi + Slab.Cache.FreeList]
    test    eax, eax
    jnz     .FoundFree

    stdcall Slab.Grow [ptrCache]
    
    mov     eax, [esi + Slab.Cache.FreeList]
    test    eax, eax
    jz      .CriticalError 

 .FoundFree:
    ; Извлекаем объект из односвязного списка FreeList
    ; (первые 4 байта свободного объекта хранят адрес следующего)
    mov     ebx, [eax]
    mov     [esi + Slab.Cache.FreeList], ebx
    
    inc     [esi + Slab.Cache.ActiveObjects]

    jmp     .EndProc

 .CriticalError:
    
    xor     eax, eax
 .EndProc:
    SPIN_UNLOCK edi
    ret
endp

; .in: ptrCache = указатель на Slab.Cache
proc Slab.Grow uses esi edi, ptrCache
   
    stdcall PMM.Alloc, 0, PMM.ZONE_NORMAL
    test    eax, eax
    jz      .Fail

    mov     edi, [Slab.HeapPointer]
    stdcall VMM.MapPage, edi, eax, VMM_PRESENT or VMM_WRITABLE
    cmp     eax, -1
    je      .Fail
    
    add     dword [Slab.HeapPointer], PAGE_SIZE 
    
    mov     esi, [ptrCache]

    mov     eax, PAGE_SIZE
    xor     edx, edx
    div     dword[esi + Slab.Cache.ObjSize]
    add [esi + Slab.Cache.TotalObjects], eax
    
    
    xchg    ecx, eax            

    mov edx, [esi + Slab.Cache.ObjSize]
    mov eax, [esi + Slab.Cache.FreeList] ; Текущий список (может быть null)    

 .SliceLoop:
    mov [edi], eax          ; Текущий указывает на предыдущую голову
    mov eax, edi            ; Текущий становится новой головой
    add edi, edx            ; Переходим к следующему блоку
    loop .SliceLoop

    mov [esi + Slab.Cache.FreeList], eax
    
 .Fail:
    ret
endp

; .in: objSize = размер объекта, ptrName = указатель на строку-имя
; .out: eax = указатель на новый Slab.Cache или 0
proc Slab.CreateCache uses ebx esi edi, objSize, ptrName
    stdcall Slab.Alloc, Slab.InitialCache
    test    eax, eax
    jz      .Fail
    
    mov     ebx, eax

    mov     ecx, Slab.Cache.Size / 4
    mov     edi, ebx
    xor     eax, eax
    rep     stosd 

    mov     eax, [objSize]
    add     eax, 3
    and     eax, -4
    mov     [ebx + Slab.Cache.ObjSize], eax

    lea     edi, [ebx + Slab.Cache.Name]
    mov     esi, [ptrName]
    mov     ecx, 23
 .CopyName:
    lodsb
    stosb
    test    al, al
    jz      .NameDone
    loop    .CopyName
    mov     byte [edi], 0
 .NameDone:

    SPIN_LOCK Slab.GlobalLock
    
    mov     eax, [Slab.CacheList]
    mov     [ebx + Slab.Cache.NextCache], eax
    mov     [Slab.CacheList], ebx
    
    SPIN_UNLOCK Slab.GlobalLock
    
    mov     eax, ebx
 .Fail:
    ret
endp

}



block(.data) {
    Slab.HeapPointer  dd ?
    Slab.CacheList    dd ?
    Slab.InitialCache db Slab.Cache.Size dup (?)
}

block(.initData) {
    Slab.GlobalLock   dd 0
    ustr0 Slab.Name.InitialCache 'INIT_CHACHE'

}