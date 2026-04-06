block(.const) {
    ; Макрос захвата спин-лока (с оптимизацией под кэш процессора)
    macro SPIN_LOCK lock_var {
        local .retry, .pause, .done
    .retry:
        lock bts dword [lock_var], 0 
        jnc .done                    
    .pause:
        pause                        
        bt dword [lock_var], 0       
        jc .pause                    
        jmp .retry                   
    .done:
    }

    ; Макрос освобождения спин-лока
    macro SPIN_UNLOCK lock_var {
        mov dword [lock_var], 0      
    }
}