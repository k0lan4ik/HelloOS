block(.consts){
        PROCESS_FREE    equ 0
        PROCESS_ACTIVE  equ 1
        PROCESS_DEAD    equ 2

virtual at 0
        Procces.CR3         dd ?
        Procces.FileTab     dd ?
        Procces.SigHandler  dd ?
        Procces.Accounting  dd ?
        Procces.Procname    dd ?
        Procces.Status      db ?
        Procces.Pad         db 11 dup ?
        ProccesSize:
end virtual
}


block(.initData){
     Procces.Procces dd 0xFF102000
}

block(.data){
     Process.Mutex db ?
}