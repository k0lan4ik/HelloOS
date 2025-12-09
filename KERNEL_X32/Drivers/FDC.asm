include 'macro/proc32.inc'

use32

;Порты
STATUS_REGISTER_A               equ 0x3F0
STATUS_REGISTER_B               equ 0x3F1
DIGITAL_OUTPUT_REGISTER         equ 0x3F2
TAPE_DRIVE_REGISTER             equ 0x3F3
MAIN_STATUS_REGISTER            equ 0x3F4
DATARATE_SELECT_REGISTER        equ 0x3F4
DATA_FIFO                       equ 0x3F5
DIGITAL_INPUT_REGISTER          equ 0x3F7
CONFIGURATION_CONTROL_REGISTER  equ 0x3F7


;Команды
READ_TRACK                      equ 2
SPECIFY                         equ 3
SENSE_DRIVE_STATUS              equ 4
WRITE_DATA                      equ 5      
READ_DATA                       equ 6     
RECALIBRATE                     equ 7   
SENSE_INTERRUPT                 equ 8    
WRITE_DELETED_DATA              equ 9
READ_ID                         equ 10
READ_DELETED_DATA               equ 12
FORMAT_TRACK                    equ 13
DUMPREG                         equ 14
SEEK                            equ 15    
VERSION                         equ 16
SCAN_EQUAL                      equ 17
PERPENDICULAR_MODE              equ 18
CONFIGURE                       equ 19
LOCK                            equ 20
VERIFY                          equ 22
SCAN_LOW_OR_EQUAL               equ 25
SCAN_HIGH_OR_EQUAL              equ 29

;Статусы драйвера
FDC_STATE_IDLE                  equ 0x0   
FDC_STATE_SEEK                  equ 0x1   
FDC_STATE_RW                    equ 0x2   
FDC_STATE_SENSEI                equ 0x3   

; Чтение регистра в память
macro FDCREAD m {
    call    FDC.ReadReg
    mov     [m], al
}

; Запись
macro FDCWRI m {

    mov     al, m
    call    FDC.WriteReg
}

; Ожидать готовность IRQ
macro FDC_WAIT_IRQ {
@@:     cmp     [FDC.Ready], 0
        je      @b
}






; Переменные
FDC.Status          db 0
FDC.Ready           db 0
FDC.Error           db 0
FDC.Motor           db 0
FDC.MotorTime       dd 0
FDC.LBA             dw 0

;Параметры CHS
FDC.RCylinder       db 0
FDC.RHead           db 0
FDC.RSector         db 0

;Данные регистров
FDC.st0             db 0
FDC.st1             db 0
FDC.st2             db 0 
FDC.CurCyilnder     db 0
FDC.HeadStart       db 0
FDC.HeadEnd         db 0
FDC.SectorEnd       db 0

;Кеш диска
FDC.Cache.Mask      dd 0 
FDC.Cache.Data      dd 0


