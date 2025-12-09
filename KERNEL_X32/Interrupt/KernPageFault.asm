block(.text) {
proc KernPageFault.Init
     stdcall Pager.AddPFHandler, KernPageFault.CodeFault, 0xF0000000, 0xF3FFFFFF, 0xF0000000, 0xF3FFFFFF
     stdcall Pager.AddPFHandler, KernPageFault.StaticDataFault, 0xF4000000, 0xFDFFFFFF, 0xF0000000, 0xF3FFFFFF
	stdcall Pager.AddPFHandler, KernPageFault.DataFault, 0xFE000000, PML1BASE-1, 0xF0000000, 0xF3FFFFFF
     ret
endp


proc KernPageFault.DataFault, error, eeip, virt
     STOP_POINT
     xchg eax, eax
     ret
endp

proc KernPageFault.StaticDataFault, error, eeip, virt
     STOP_POINT
     stdcall   FramePool.GetFreePage
     mov       edx, [virt]
     shr       edx, 12
     stdcall   Pager.MapPage, edx, eax, AL_FL_WRITABLE or AL_FL_GLOBAL or AL_FL_NOEXEC
     ret
endp


proc KernPageFault.CodeFault, error, eeip, virt
     STOP_POINT
     ret
endp
}