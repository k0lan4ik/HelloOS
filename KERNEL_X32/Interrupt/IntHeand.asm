proc IntHeand.Init
     mov  ecx, 0x100
@@:
     stdcall IntHeand.SetIntSave, ecx, IntHeand.Void     
     loop @B
endp

proc IntHeand.Void 
     STOP_POINT
     iret
endp

proc IntHeand.SetIntSave, interupt:BYTE, handler
     movzx     edx, [interupt] 
     shl       edx, 3
     add       edx, IDT

     cli

     cmp       [handler], 0
     jne       @F
     mov       eax, IntHeand.Void
     jmp       .EndIf
@@:
     mov       eax, [handler]
.EndIf:

     mov       word[edx], ax
     shr       edx, 16
     mov       word[edx + 6], ax
     mov       byte[edx + 5], 0x8E
     mov       word[edx + 3], 0
     mov       byte[edx + 2], 8
     
     ret
endp