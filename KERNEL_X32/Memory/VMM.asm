block(.consts){
    PAGE_SIZE        equ 4096
    PAGE_MASK        equ 0xFFFFF000
    PAGE_OFFSET_MASK equ 0x00000FFF

    VMM.PML2_INDEX_SHIFT equ 22
    VMM.PML1_INDEX_SHIFT equ 12
    VMM.INDEX_MASK       equ 0x3FF 

    VMM.RECURSIVE_SLOT   equ 1023
    VMM.TABLES_BASE      equ 0xFFC00000 
    VMM.DIRECTORY_BASE   equ 0xFFFFF000 

    ; Флаги записей таблицы страниц (Hardware)
    VMM_PRESENT  equ 0x01      ; P - Наличие в памяти
    VMM_WRITABLE equ 0x02      ; R/W - 0: Read-only, 1: Read/Write
    VMM_USER     equ 0x04      ; U/S - 0: Supervisor only, 1: User access
    VMM_PWT      equ 0x08      ; Write-through
    VMM_PCD      equ 0x10      ; Cache disable (для MMIO)
    VMM_ACCESSED equ 0x20      ; Устанавливается CPU при чтении
    VMM_DIRTY    equ 0x40      ; Устанавливается CPU при записи
    VMM_PAT      equ 0x80      ; Page Attribute Table
    VMM_GLOBAL   equ 0x100     ; G - Не сбрасывать TLB при смене CR3
}

block(.text){

; @[proc]
; .parent:   VMM
; .name:     VMM.GetPTE
; .desc:     Находит виртуальный адрес записи в таблице страниц (PTE).
;            Если таблицы (PML1) нет и allocate=1, выделяет новую через PMM.
proc VMM.GetPTE uses ebx edi, virtAddr, allocate
    mov     edx, [virtAddr]
    
    VMM.GET_PDE_INDEX ebx, edx
    lea     ebx, [VMM.DIRECTORY_BASE + ebx*4]
    
    test    dword [ebx], VMM_PRESENT
    jnz     .TableExists

    cmp     [allocate], 0
    je      .Error
    
    stdcall PMM.Alloc, 0, PMM.ZONE_NORMAL
    test    eax, eax
    jz      .Error
    
    or      eax, VMM_PRESENT or VMM_WRITABLE or VMM_USER
    mov     [ebx], eax
    
    VMM.GET_PDE_INDEX edi, edx
    shl     edi, VMM.PML1_INDEX_SHIFT
    add     edi, VMM.TABLES_BASE 
    
    xor     eax, eax
    mov     ecx, PAGE_SIZE / 4
    rep stosd

    invlpg  [ebx]

 .TableExists:
    shr     edx, VMM.PML1_INDEX_SHIFT           
    lea     eax, [VMM.TABLES_BASE + edx*4]
    jmp     .EndProc

 .Error:
    xor     eax, eax
 .EndProc:
    ret
endp

; @[proc]
; .parent:   VMM
; .name:     VMM.MapPage
; .desc:     Связывает виртуальную страницу с физической.
proc VMM.MapPage uses ebx, virt, phys, flags
    SPIN_LOCK VMM.GlobalLock
    
    stdcall VMM.GetPTE, [virt], 1 
    test    eax, eax
    jz      .Failed
    
    mov     edx, [phys]
    and     edx, PAGE_MASK 
    mov     ebx, [flags]
    and     ebx, not PAGE_MASK
    or      edx, ebx
    or      edx, VMM_PRESENT    
    
    mov     [eax], edx          
    invlpg  [virt]              
    
    xor     eax, eax
    jmp     .EndProc
 .Failed:
    mov     eax, -1
 .EndProc:   
    SPIN_UNLOCK VMM.GlobalLock
    ret
endp

; @[proc]
; .parent:   VMM
; .name:     VMM.UnmapPage
; .desc:     Разрывает связь виртуальной страницы с физической.
proc VMM.UnmapPage, virt
    SPIN_LOCK VMM.GlobalLock

    stdcall VMM.GetPTE, [virt], 0
    test    eax, eax
    jz      .EndProc
    
    mov     dword [eax], 0
    invlpg  [virt]
 .EndProc:
    SPIN_UNLOCK VMM.GlobalLock
    ret
endp

}

block(.initData){
    VMM.GlobalLock    dd 0
}